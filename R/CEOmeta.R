#' Internal function to get the metadata of CEO surveys into cache
#'
#' Used when loading the package. It gets the latest metadata of CEO surveys,
#' cleans it and makes it ready for the rest of functions in the package.
#' @keywords internal
#' @encoding UTF-8
the <- new.env(parent = emptyenv())

# Expected columns required by public-facing filters and browse behavior.
ceo_meta_required_cols <- function() {
  c(
    "REO",
    "Titol enquesta",
    "Titol estudi",
    "Objectius",
    "Resum",
    "Descriptors",
    "Data d'alta al REO",
    "Enllac",
    "Microdades 1"
  )
}

ceo_meta_cache_valid <- function(x, required_cols = ceo_meta_required_cols()) {
  is.data.frame(x) && all(required_cols %in% names(x))
}

CEOmetadata <- function() {
  required_cols <- ceo_meta_required_cols()
  cache_env <- ceo_cache_env()

  cached <- cache_env$CEOmetadata
  cache_invalid <- is.null(cached) || !ceo_meta_cache_valid(cached, required_cols)

  if (isTRUE(cache_invalid)) {
    cache_env$CEOmetadata <- getCEOmetadata()
  }

  cache_env$CEOmetadata
}

getCEOmetadata <- function() {
  # CSV/TSV exports are not robust due to embedded newlines in text fields;
  # use JSON endpoint instead.
  url.ceo.table <- paste0(
    "https://analisi.transparenciacatalunya.cat/api/views/",
    "m5mb-xt5e/rows.json?accessType=DOWNLOAD&sorting=true"
  )

  ceo.meta <- tryCatch(
    jsonlite::fromJSON(url.ceo.table),
    error = function(e) e
  )

  if (inherits(ceo.meta, "error")) {
    message(
      paste0(
        "A problem downloading the metadata has occurred. ",
        "The server may be temporarily down, or the API has changed. ",
        "Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating ",
        "'Problem with metadata file'.\n\n",
        "Underlying error: ", conditionMessage(ceo.meta)
      )
    )
    return(NULL)
  }

  if (!is.list(ceo.meta) || length(ceo.meta) < 2L) {
    message("Unexpected metadata response shape from the CEO API.")
    return(NULL)
  }

  ceo.meta.info <- ceo.meta[[1]]
  ceo.table.raw <- ceo.meta[[2]]

  if (is.null(ceo.table.raw) || length(ceo.table.raw) == 0L) {
    message("The metadata response does not include survey rows.")
    return(NULL)
  }

  ceo.rows <- lapply(ceo.table.raw, read.ceo.json)
  ceo.table <- tibble::as_tibble(as.data.frame(do.call(rbind, ceo.rows)))

  col_names <- tryCatch(
    ceo.meta.info[[1]]$columns$name,
    error = function(e) NULL
  )
  if (!is.null(col_names) && length(col_names) == ncol(ceo.table)) {
    names(ceo.table) <- col_names
  } else {
    message(
      "Could not align metadata column names from API response. ",
      "The schema may have changed."
    )
    return(NULL)
  }

  # Normalize selected non-ASCII / variant column names used downstream.
  names(ceo.table)[grep("Enlla.$", names(ceo.table))] <- "Enllac"
  names(ceo.table)[grep("Enlla. matriu de dades$", names(ceo.table))] <- "Enllac matriu de dades"
  names(ceo.table)[grep("^Microdades[[:space:][:punct:]]*1$", names(ceo.table))] <- "Microdades 1"
  names(ceo.table)[grep("M.tode de recollida de dades$", names(ceo.table))] <- "Metode de recollida de dades"
  names(ceo.table)[grep(".mbit territorial", names(ceo.table))] <- "Ambit territorial"
  names(ceo.table)[grep("T.tol enquesta", names(ceo.table))] <- "Titol enquesta"
  names(ceo.table)[grep("T.tol estudi", names(ceo.table))] <- "Titol estudi"

  # Backward-compatible fallback while the portal transitions field names.
  if (!("Microdades 1" %in% names(ceo.table)) && ("Enllac matriu de dades" %in% names(ceo.table))) {
    ceo.table[["Microdades 1"]] <- ceo.table[["Enllac matriu de dades"]]
  }

  expected <- c(
    ceo_meta_required_cols(),
    "Metodologia enquesta",
    "Metode de recollida de dades",
    "Ambit territorial",
    "Dia inici treball de camp",
    "Dia final treball de camp",
    "Any d'entrada al REO",
    "Mostra estudis quantitatius",
    "Cost"
  )
  ceo_warn_missing_cols(ceo.table, expected, "CEO metadata")
  ceo.table <- ceo_ensure_cols(ceo.table, expected)

  d <- ceo.table |>
    dplyr::select(
      -dplyr::any_of(
        c(
          "sid", "id", "position", "created_at", "created_meta",
          "updated_at", "updated_meta", "meta"
        )
      )
    ) |>
    dplyr::mutate(
      REO = factor(REO, levels = rev(unique(REO[!is.na(REO)]))),
      `Metodologia enquesta` = factor(`Metodologia enquesta`),
      `Metode de recollida de dades` = factor(`Metode de recollida de dades`),
      `Ambit territorial` = factor(`Ambit territorial`),
      `Dia inici treball de camp` = ceo_to_date_safe(`Dia inici treball de camp`, n = dplyr::n()),
      `Dia final treball de camp` = ceo_to_date_safe(`Dia final treball de camp`, n = dplyr::n()),
      `Any d'entrada al REO` = suppressWarnings(as.integer(`Any d'entrada al REO`)),
      `Data d'alta al REO` = ceo_to_date_safe(`Data d'alta al REO`, n = dplyr::n()),
      `Mostra estudis quantitatius` = suppressWarnings(as.numeric(`Mostra estudis quantitatius`)),
      Cost = suppressWarnings(as.numeric(Cost)),
      microdata_available = {
        # Single-study downloads currently support only direct .sav links.
        micro_url <- tolower(trimws(as.character(`Microdades 1`)))
        !is.na(micro_url) & grepl("\\.sav($|\\?)", micro_url)
      }
    )

  ceo_assert_cols(d, ceo_meta_required_cols(), "CEO metadata after normalization")

  d
}


#' Import metadata from the "Centre d'Estudis d'Opinio"
#'
#' Easy and convenient access to the metadata of the "Centre d'Estudis d'Opinio",
#' the Catalan institution for polling and public opinion.
#'
#' @encoding UTF-8
#' @param reo Character vector with one or more REO identifiers. When not NULL it
#'   has precedence over `search`, `date_start`, and `date_end`.
#' @param search Character vector with keywords to look for in metadata text
#'   fields. Each element is interpreted as one search expression; multiple
#'   elements are combined with OR. Matching is case-insensitive.
#' @param date_start Optional start date (`"YYYY-MM-DD"` or `Date`) for
#'   `Data d'alta al REO`.
#' @param date_end Optional end date (`"YYYY-MM-DD"` or `Date`) for
#'   `Data d'alta al REO`.
#' @param browse Logical value. If TRUE, open matching metadata URLs in the
#'   browser (up to 10 entries unless `browse_force = TRUE`).
#' @param browse_translate Optional language code when `browse = TRUE`:
#'   `'oc'` (Apertium), or Google-Translate target among
#'   `'de'`, `'en'`, `'eu'`, `'gl'`, `'sp'`/`'es'`.
#' @param browse_force Logical value. If TRUE, bypasses the safety limit of
#'   opening at most 10 URLs.
#' @export
#' @return A tibble with the metadata of surveys produced by the CEO.
#' @examples
#' \dontrun{
#' # Retrieve metadata of all surveys:
#' meta <- CEOmeta()
#' dim(meta)
#'
#' # Search for specific terms:
#' CEOmeta(search = "internet")
#'
#' # "Medi" AND "Ambient"
#' CEOmeta(search = "Medi ambient")
#'
#' # ("Medi" AND "Ambient") OR "Municipi"
#' CEOmeta(search = c("Medi ambient", "Municipi"))
#'
#' # Search for entries starting in 2020
#' CEOmeta(date_start = "2020-01-01")
#'
#' # Get a specific REO and open its description in browser
#' CEOmeta(reo = "746", browse = TRUE)
#' }
CEOmeta <- function(
  reo = NULL,
  search = NULL,
  date_start = NA,
  date_end = NA,
  browse = FALSE,
  browse_translate = NULL,
  browse_force = FALSE
) {
  # ---- Load cached metadata ----
  d <- CEOmetadata()
  if (is.null(d)) {
    return(NULL)
  }

  # ---- Validate control arguments ----
  ceo_assert_cols(d, c("REO", "Data d'alta al REO"), "CEO metadata")

  if (!is.logical(browse) || length(browse) != 1L || is.na(browse)) {
    stop("`browse` must be a single non-missing logical value.", call. = FALSE)
  }
  if (!is.logical(browse_force) || length(browse_force) != 1L || is.na(browse_force)) {
    stop("`browse_force` must be a single non-missing logical value.", call. = FALSE)
  }

  # ---- Filter by REO or text search ----
  if (!is.null(reo)) {
    if (!is.character(reo)) {
      stop("`reo` must be a character vector.", call. = FALSE)
    }
    d <- d |>
      dplyr::filter(REO %in% reo)
  } else if (!is.null(search)) {
    if (!is.character(search)) {
      stop("`search` must be a character vector.", call. = FALSE)
    }

    search.strings <- unique(stringr::str_trim(tolower(search)))
    search.strings <- search.strings[nzchar(search.strings)]
    if (length(search.strings) == 0L) {
      stop("`search` must contain at least one non-empty term.", call. = FALSE)
    }

    message("Looking for entries with: ", paste(search.strings, collapse = " OR "))

    searchable_fields <- c(
      "Titol enquesta", "Titol estudi", "Objectius", "Resum", "Descriptors"
    )
    columns.to.search <- intersect(searchable_fields, names(d))

    if (length(columns.to.search) == 0L) {
      stop(
        "Metadata does not include searchable text columns. The schema may have changed.",
        call. = FALSE
      )
    }

    search.pattern <- paste(search.strings, collapse = "|")
    reo.match <- d |>
      dplyr::select(dplyr::all_of(c("REO", columns.to.search))) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(columns.to.search),
          ~ tolower(as.character(.x))
        )
      ) |>
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(columns.to.search),
          ~ stringr::str_detect(.x, pattern = search.pattern)
        )
      ) |>
      dplyr::pull(REO) |>
      unique()

    if (length(reo.match) < 1L) {
      stop(
        paste0(
          "There are no entries matching: ",
          paste(search.strings, collapse = " OR "),
          ".\nYou may want to reduce the scope or change the text."
        ),
        call. = FALSE
      )
    }

    d <- d |>
      dplyr::filter(REO %in% reo.match)
  }

  parse_user_date <- function(x, arg_name) {
    if (inherits(x, "Date")) {
      if (length(x) != 1L) {
        stop("`", arg_name, "` must be length 1 when a Date is provided.", call. = FALSE)
      }
      return(x[[1]])
    }
    if (length(x) != 1L) {
      stop("`", arg_name, "` must be NA, Date, or a single 'YYYY-MM-DD' string.", call. = FALSE)
    }
    if (is.na(x)) return(as.Date(NA))
    if (!is.character(x)) {
      stop("`", arg_name, "` must be NA, Date, or a single 'YYYY-MM-DD' string.", call. = FALSE)
    }
    out <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
    if (is.na(out)) {
      stop("`", arg_name, "` is not a valid date. Use 'YYYY-MM-DD'.", call. = FALSE)
    }
    out
  }

  date_start_parsed <- parse_user_date(date_start, "date_start")
  date_end_parsed <- parse_user_date(date_end, "date_end")

  if (!is.na(date_start_parsed) && !is.na(date_end_parsed) && date_start_parsed > date_end_parsed) {
    stop("`date_start` cannot be after `date_end`.", call. = FALSE)
  }

  # ---- Filter by registration date ----
  if (!is.na(date_start_parsed)) {
    d <- d |>
      dplyr::filter(`Data d'alta al REO` >= date_start_parsed)
  }
  if (!is.na(date_end_parsed)) {
    d <- d |>
      dplyr::filter(`Data d'alta al REO` <= date_end_parsed)
  }

  if (isTRUE(browse)) {
    # ---- Optional browser opening ----
    n_matches <- nrow(d)
    if (n_matches == 0L) {
      message("No entries to browse.")
      return(d)
    }

    if (!is.null(browse_translate)) {
      if (!is.character(browse_translate) || length(browse_translate) != 1L) {
        stop("`browse_translate` must be NULL or a single language code.", call. = FALSE)
      }
      browse_translate <- tolower(browse_translate)
      allowed_langs <- c("oc", "de", "en", "eu", "gl", "sp", "es")
      if (!(browse_translate %in% allowed_langs)) {
        stop(
          "`browse_translate` must be one of: ",
          paste(allowed_langs, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
    }

    if (n_matches > 10L && !isTRUE(browse_force)) {
      message(
        "Browse request skipped: ", n_matches, " entries match. ",
        "Use `browse_force = TRUE` to open more than 10 URLs."
      )
      return(d)
    }

    for (i in seq_len(n_matches)) {
      url.to.open <- as.character(d$Enllac[[i]])
      if (is.na(url.to.open) || !nzchar(url.to.open)) {
        next
      }

      if (!is.null(browse_translate)) {
        if (browse_translate == "oc") {
          url.to.open <- paste0(
            "https://www.apertium.org/index.eng.html#webpageTranslation?dir=cat-oci&qW=",
            url.to.open
          )
        } else {
          tl <- if (browse_translate == "sp") "es" else browse_translate
          domain <- urltools::domain(url.to.open)
          path <- sub("^https?://[^/]*", "", url.to.open)
          if (!nzchar(path)) path <- "/"

          if (!is.na(domain) && nzchar(domain)) {
            url.to.open <- paste0(
              "https://",
              gsub("\\.", "-", domain),
              ".translate.goog",
              path,
              "?_x_tr_sl=ca&_x_tr_tl=",
              tl
            )
          }
        }
      }

      utils::browseURL(url.to.open)
      Sys.sleep(0.05)
    }
  }

  d
}

#' Internal function to properly read the JSON from CEO
#'
#' Used to address limitations of the JSON format provided.
#'
#' @keywords internal
#' @encoding UTF-8
#' @param x JSON data structure
read.ceo.json <- function(x) {
  vapply(
    seq_along(x),
    function(i) {
      element <- x[[i]]
      if (length(element) == 0L) {
        return(NA_character_)
      }
      if (length(element) == 1L) {
        return(as.character(element))
      }
      as.character(element[[1]])
    },
    FUN.VALUE = character(1)
  )
}
