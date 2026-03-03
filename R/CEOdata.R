# ---- Internal helpers used by CEOdata() --------------------------------------

ceodata_norm_scalar <- function(x, arg_name) {
  if (length(x) != 1L) {
    stop("`", arg_name, "` must be length 1.", call. = FALSE)
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    if (is.na(x)) return(NA_character_)
    # Avoid scientific notation for IDs.
    x <- format(x, scientific = FALSE, trim = TRUE)
  }
  x <- as.character(x)
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  trimws(x)
}

ceodata_is_na_scalar <- function(x) {
  length(x) == 1L && is.na(x)
}

ceodata_url_kind <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) return("none")
  u <- tolower(trimws(url))
  if (!nzchar(u)) return("none")
  if (grepl("\\.sav($|\\?)", u)) return("sav")
  if (grepl("\\.zip($|\\?)", u)) return("zip")
  if (grepl("\\.csv($|\\?)", u)) return("csv")
  "other"
}

ceodata_pick_best_url <- function(urls, context) {
  if (is.null(urls) || length(urls) == 0L) {
    stop("No microdata links found for ", context, ".", call. = FALSE)
  }

  urls <- trimws(as.character(urls))
  urls <- urls[!is.na(urls) & nzchar(urls)]
  urls <- unique(urls)

  if (length(urls) == 0L) {
    stop("No non-empty microdata links found for ", context, ".", call. = FALSE)
  }

  kinds <- vapply(urls, ceodata_url_kind, FUN.VALUE = character(1))

  # Prefer direct .sav over .zip (which may contain one .sav), then fail.
  sav_idx <- which(kinds == "sav")
  if (length(sav_idx) > 0L) {
    return(urls[[sav_idx[[1]]]])
  }

  zip_idx <- which(kinds == "zip")
  if (length(zip_idx) > 0L) {
    return(urls[[zip_idx[[1]]]])
  }

  if (any(kinds == "csv")) {
    stop(
      "Only CSV links were found for ", context, " (",
      paste(urls[kinds == "csv"], collapse = ", "),
      "). CEOdata only supports .sav links or .zip files containing one .sav.",
      call. = FALSE
    )
  }

  stop(
    "No supported microdata link was found for ", context, ". ",
    "Expected .sav or .zip. Found: ", paste(urls, collapse = ", "),
    call. = FALSE
  )
}

ceodata_norm_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", iconv(x, to = "ASCII//TRANSLIT")))
}

ceodata_guess_col <- function(nms, canonical, aliases = character(0)) {
  if (canonical %in% nms) return(canonical)

  nms_norm <- ceodata_norm_name(nms)
  target_norm <- unique(ceodata_norm_name(c(canonical, aliases)))
  idx <- which(nms_norm %in% target_norm)
  if (length(idx) < 1L) return(NULL)
  nms[[idx[[1]]]]
}

ceodata_extract_urls <- function(d, cols) {
  vals <- unlist(
    lapply(cols, function(col) {
      if (is.null(col) || !(col %in% names(d))) return(character(0))
      as.character(d[[col]])
    }),
    use.names = FALSE
  )
  vals
}

#' Import datasets / microdata from the Centre d'Estudis d'Opinio
#'
#' Easy and convenient access to datasets / microdata from the
#' "Centre d'Estudis d'Opinio". The function can return either:
#' (1) an accumulated microdata series (identified by `series`), or
#' (2) a single study microdata dataset (identified by `reo`).
#'
#' Accumulated series are obtained from the Dades Obertes catalogue and are
#' usually identified by `codi_serie` (e.g. `"BOP_presencial"`).
#'
#' @encoding UTF-8
#' @param series Character scalar identifying the accumulated series to download.
#'   Ignored when `reo` is provided.
#' @param reo Single REO identifier of a study to download. Can be character,
#'   numeric, or factor-like, and is normalized internally.
#' @param raw Logical. If FALSE (default), converts SPSS labelled vectors into
#'   factors. If TRUE, returns raw haven-labelled vectors.
#' @export
#' @return A tibble with individuals' responses to the requested dataset.
#' @examples
#' \dontrun{
#' # Default: accumulated microdata series
#' d <- CEOdata()
#'
#' # Load another accumulated series by code
#' d_tel <- CEOdata(series = "BOP_telefonica")
#'
#' # Load a single study by REO
#' d1145 <- CEOdata(reo = "1145")
#' }
CEOdata <- function(series = "BOP_presencial",
                    reo = NA,
                    raw = FALSE) {

  # ---- Validate shared inputs ----
  if (!is.logical(raw) || length(raw) != 1L || is.na(raw)) {
    stop("'raw' must be a single logical value.", call. = FALSE)
  }

  # Keep historical behavior: user should choose one mode only.
  reo_supplied <- !missing(reo) && !is.null(reo) && !ceodata_is_na_scalar(reo)
  series_supplied <- !missing(series) && !is.null(series) && !ceodata_is_na_scalar(series)

  if (isTRUE(reo_supplied) && isTRUE(series_supplied)) {
    stop("Provide either 'reo' or 'series', but not both.", call. = FALSE)
  }

  # ---- Mode 1: Single-study download by REO ----
  if (isTRUE(reo_supplied)) {
    reo_norm <- ceodata_norm_scalar(reo, "reo")
    if (is.na(reo_norm) || !nzchar(reo_norm)) {
      stop("'reo' must be a non-empty scalar (e.g. '1145').", call. = FALSE)
    }

    meta <- CEOmetadata()
    if (is.null(meta) || !is.data.frame(meta) || nrow(meta) == 0L) {
      stop("Could not load survey metadata from CEOmetadata().", call. = FALSE)
    }

    ceo_assert_cols(meta, c("REO", "Microdades 1"), "CEOmetadata()")

    # Some API payloads may represent REO as numeric/factor or include whitespace.
    reo_col <- trimws(as.character(meta$REO))
    idx <- !is.na(reo_col) & reo_col == reo_norm

    if (sum(idx) == 0L) {
      stop("There is no dataset available for REO ", reo_norm, ".", call. = FALSE)
    }

    rows <- meta[idx, , drop = FALSE]
    micro_col_2 <- if ("Microdades 2" %in% names(rows)) "Microdades 2" else NULL
    url_candidates <- ceodata_extract_urls(rows, c("Microdades 1", micro_col_2))
    url <- ceodata_pick_best_url(url_candidates, context = paste0("REO ", reo_norm))

    if (nrow(rows) > 1L) {
      message(
        "Multiple metadata rows found for REO ", reo_norm,
        ". Using first preferred supported link: ", url
      )
    }

    message("Downloading and reading REO dataset. This may take a while.")
    return(ceodata_download_and_read(url, raw = raw))
  }

  # ---- Mode 2: Accumulated-series download by codi_serie ----
  series_norm <- ceodata_norm_scalar(series, "series")
  if (is.na(series_norm) || !nzchar(series_norm)) {
    stop("'series' must be provided when 'reo' is NA.", call. = FALSE)
  }

  m <- CEOaccumulated_metadata()
  if (is.null(m) || !is.data.frame(m) || nrow(m) == 0L) {
    stop("Could not load accumulated-series metadata.", call. = FALSE)
  }

  # Defensive column resolution for minor schema changes / misspellings.
  series_col <- ceodata_guess_col(
    names(m),
    canonical = "codi_serie",
    aliases = c("codi serie", "codiserie", "codi_seriee", "codi_series", "codi series")
  )
  if (is.null(series_col)) {
    stop(
      "Accumulated metadata does not include a recognizable 'codi_serie' column.",
      call. = FALSE
    )
  }

  micro1_col <- ceodata_guess_col(
    names(m),
    canonical = "microdades_1",
    aliases = c("microdades 1", "microdades1", "microdada_1", "microdades_01")
  )
  micro2_col <- ceodata_guess_col(
    names(m),
    canonical = "microdades_2",
    aliases = c("microdades 2", "microdades2", "microdada_2", "microdades_02")
  )

  if (is.null(micro1_col) && is.null(micro2_col)) {
    stop(
      "Accumulated metadata does not include recognizable microdata link columns.",
      call. = FALSE
    )
  }

  series_col_chr <- trimws(as.character(m[[series_col]]))
  idx <- !is.na(series_col_chr) & series_col_chr == series_norm

  if (sum(idx) == 0L) {
    # Best-effort fallback to case-insensitive matching.
    idx <- !is.na(series_col_chr) &
      tolower(series_col_chr) == tolower(series_norm)
  }

  if (sum(idx) == 0L) {
    stop("Unknown accumulated series: ", series_norm, ".", call. = FALSE)
  }

  rows <- m[idx, , drop = FALSE]
  url_candidates <- ceodata_extract_urls(rows, c(micro1_col, micro2_col))
  url <- ceodata_pick_best_url(
    url_candidates,
    context = paste0("accumulated series ", series_norm)
  )

  if (nrow(rows) > 1L) {
    message(
      "Multiple metadata rows found for accumulated series ", series_norm,
      ". Using first preferred supported link: ", url
    )
  }

  message("Downloading and reading accumulated series. This may take a while.")
  ceodata_download_and_read(url, raw = raw)
}
