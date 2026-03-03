# Shared internal helpers
# These functions centralize defensive parsing/validation logic used across
# data importers. Keeping them here reduces duplication and makes schema/API
# updates easier to maintain.

# Expected columns for accumulated-series metadata endpoint.
ceo_acc_expected_cols <- function() {
  c(
    "codi_serie", "titol_serie", "mode_admin",
    "data_inici", "data_fi", "reo", "estat", "univers",
    "microdades_1", "microdades_2"
  )
}

# Return TRUE only when a cached object is a data.frame with all required cols.
ceo_acc_cache_valid <- function(x, expected_cols = ceo_acc_expected_cols()) {
  is.data.frame(x) && all(expected_cols %in% names(x))
}

# Use package cache env `the` when available, otherwise use a temporary env.
ceo_cache_env <- function() {
  if (exists("the", inherits = TRUE) && is.environment(the)) {
    return(the)
  }
  new.env(parent = emptyenv())
}

# Warn when endpoint schema no longer matches expected columns.
ceo_warn_missing_cols <- function(d, expected_cols, source_name) {
  missing_cols <- setdiff(expected_cols, names(d))
  if (length(missing_cols) > 0L) {
    warning(
      paste0(
        source_name, " is missing expected columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  invisible(missing_cols)
}

# Ensure all expected columns exist so downstream mutate/select calls are stable.
ceo_ensure_cols <- function(d, expected_cols) {
  for (nm in expected_cols) {
    if (!(nm %in% names(d))) d[[nm]] <- rep(NA, nrow(d))
  }
  d
}

# Extract URL-like values robustly from scalars, lists, and nested objects.
ceo_extract_url_column <- function(x, n, field_name = "url_field") {
  has_content <- function(el) {
    if (is.null(el)) return(FALSE)
    if (is.atomic(el)) return(length(el) > 0L && any(!is.na(el)))
    if (is.data.frame(el)) return(nrow(el) > 0L && ncol(el) > 0L)
    if (is.list(el)) return(length(el) > 0L)
    FALSE
  }

  if (is.null(x)) return(rep(NA_character_, n))

  if (is.data.frame(x)) {
    if ("url" %in% names(x)) return(as.character(x$url))
    if ("href" %in% names(x)) return(as.character(x$href))
    warning(
      paste0("Column '", field_name, "' is a data.frame without a 'url' field."),
      call. = FALSE
    )
    return(rep(NA_character_, n))
  }

  if (is.atomic(x)) {
    y <- as.character(x)
    if (length(y) == n) return(y)
    if (length(y) == 0L) return(rep(NA_character_, n))
    return(rep(y[1], n))
  }

  if (is.list(x)) {
    has_input <- vapply(x, has_content, logical(1))
    out <- vapply(
      x,
      function(el) {
        if (is.null(el)) return(NA_character_)
        if (is.data.frame(el)) {
          if (nrow(el) == 0L || ncol(el) == 0L) return(NA_character_)
          if ("url" %in% names(el)) return(as.character(el$url[[1]]))
          if ("href" %in% names(el)) return(as.character(el$href[[1]]))
          return(NA_character_)
        }
        if (is.list(el)) {
          if (!is.null(el$url)) return(as.character(el$url[[1]]))
          if (!is.null(el$href)) return(as.character(el$href[[1]]))
          vals <- unlist(el, use.names = FALSE)
          if (length(vals) == 0L) return(NA_character_)
          return(as.character(vals[[1]]))
        }
        if (is.atomic(el)) {
          if (length(el) == 0L) return(NA_character_)
          return(as.character(el[[1]]))
        }
        NA_character_
      },
      FUN.VALUE = character(1)
    )
    if (length(out) != n) return(rep(NA_character_, n))

    # Keep this as message() to avoid failing pipelines on non-critical URL noise.
    na_rows <- which(has_input & is.na(out))
    if (length(na_rows) > 0L) {
      shown <- paste(utils::head(na_rows, 20L), collapse = ", ")
      if (length(na_rows) > 20L) shown <- paste0(shown, ", ...")
      message("Rows with missing URL extraction in '", field_name, "': ", shown)
    }
    return(out)
  }

  rep(NA_character_, n)
}

# Parse API date vectors robustly:
# 1) ymd for ISO-like values, 2) dmy fallback for localized strings.
ceo_to_date_safe <- function(x, n) {
  if (is.null(x)) return(as.Date(rep(NA_character_, n)))

  x_chr <- as.character(x)
  if (length(x_chr) == 0L) x_chr <- rep(NA_character_, n)
  x_chr <- trimws(x_chr)
  x_chr[x_chr == ""] <- NA_character_
  x_chr <- sub("T.*$", "", x_chr)

  out <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  still_na <- is.na(out) & !is.na(x_chr)
  if (any(still_na)) {
    out[still_na] <- suppressWarnings(lubridate::dmy(x_chr[still_na], quiet = TRUE))
  }

  if (length(out) == 1L && n > 1L) out <- rep(out, n)
  out
}

# Guard for required columns with consistent, informative errors.
ceo_assert_cols <- function(d, required_cols, context) {
  missing_cols <- setdiff(required_cols, names(d))
  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        context, " missing required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
