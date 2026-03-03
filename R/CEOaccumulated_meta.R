#' Internal cache for accumulated-series metadata
#'
#' @keywords internal
CEOaccumulated_metadata <- function() {
  # Cache entry is reused only when both object type and schema are valid.
  expected_cols <- ceo_acc_expected_cols()
  cache_env <- ceo_cache_env()

  cached <- cache_env$CEOaccumulated_metadata
  cache_invalid <- is.null(cached) || !ceo_acc_cache_valid(cached, expected_cols)

  if (isTRUE(cache_invalid)) {
    cache_env$CEOaccumulated_metadata <- getCEOaccumulated_metadata()
  }
  cache_env$CEOaccumulated_metadata
}

#' Download and prepare the metadata of accumulated microdata series (Dades Obertes)
#'
#' Source dataset (Socrata):
#' https://analisi.transparenciacatalunya.cat/resource/gp4k-sxxn.json
#'
#' @return A tibble with the accumulated series metadata.
#' @keywords internal
getCEOaccumulated_metadata <- function() {

  url.acc <- "https://analisi.transparenciacatalunya.cat/resource/gp4k-sxxn.json"

  acc <- tryCatch(
    jsonlite::fromJSON(url.acc),
    error = function(e) e
  )

  if (inherits(acc, "error")) {
    message(
      paste0(
        "A problem downloading the accumulated-series metadata has occurred. ",
        "The server may be temporarily down, or the API has changed. ",
        "Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating ",
        "'Problem with accumulated metadata file'.\n\n",
        "Underlying error: ", conditionMessage(acc)
      )
    )
    return(NULL)
  }

  # The endpoint usually returns a data.frame. If the shape changes but is still
  # list-like, attempt to coerce; otherwise abort gracefully.
  d <- tryCatch(
    tibble::as_tibble(acc),
    error = function(e) NULL
  )

  if (is.null(d)) {
    message(
      paste0(
        "Unexpected response type from accumulated-series API. ",
        "Could not coerce response to a tibble. Received classes: ",
        paste(class(acc), collapse = ", ")
      )
    )
    return(NULL)
  }

  # Continue with a warning when non-critical columns are missing, but fail
  # early if the key identifier no longer exists.
  expected <- ceo_acc_expected_cols()
  ceo_warn_missing_cols(d, expected, "Accumulated metadata")

  if (!("codi_serie" %in% names(d))) {
    message(
      "The accumulated metadata endpoint does not include 'codi_serie'. ",
      "The schema may have changed."
    )
    return(NULL)
  }

  # Ensure stable schema (create missing columns as NA)
  d <- ceo_ensure_cols(d, expected)

  # Normalize URL fields before generic character conversion.
  d$microdades_1 <- ceo_extract_url_column(
    d$microdades_1, n = nrow(d), field_name = "microdades_1"
  )
  d$microdades_2 <- ceo_extract_url_column(
    d$microdades_2, n = nrow(d), field_name = "microdades_2"
  )

  # Type normalization
  d <- d |>
    dplyr::mutate(
      codi_serie   = as.character(codi_serie),
      titol_serie  = as.character(titol_serie),
      mode_admin   = as.character(mode_admin),
      reo          = as.character(reo),
      estat        = as.character(estat),
      univers      = as.character(univers),
      microdades_1 = as.character(microdades_1),
      microdades_2 = as.character(microdades_2),
      data_inici   = ceo_to_date_safe(data_inici, n = nrow(d)),
      data_fi      = ceo_to_date_safe(data_fi, n = nrow(d))
    )

  ceo_assert_cols(
    d, "codi_serie", "Accumulated metadata after normalization"
  )

  d <- d |>
    dplyr::filter(!is.na(codi_serie) & nzchar(codi_serie))

  d
}


#' Import metadata for accumulated microdata series (Dades Obertes)
#'
#' Returns the catalogue of "microdades acumulades" series published in
#' Dades Obertes (Socrata). Each row corresponds to a series, identified by
#' `codi_serie` (e.g. "BOP_presencial").
#'
#' @param series Optional character vector. If provided, filters results to those `codi_serie`.
#' @param active_only Logical. If TRUE, keeps only series marked as active (best-effort; depends on `estat` field).
#' @export
#' @return A tibble with the metadata of the accumulated microdata series.
#' @examples
#' \dontrun{
#' m <- CEOaccumulated_meta()
#' unique(m$codi_serie)
#'
#' CEOaccumulated_meta(series = "BOP_presencial")
#' }
CEOaccumulated_meta <- function(series = NULL, active_only = FALSE) {

  d <- CEOaccumulated_metadata()

  if (is.null(d)) {
    return(NULL)
  }

  ceo_assert_cols(d, "codi_serie", "Accumulated metadata")

  if (!is.null(series)) {
    if (!is.character(series)) {
      stop("`series` must be a character vector.", call. = FALSE)
    }
    d <- d |>
      dplyr::filter(codi_serie %in% series)
  }

  if (isTRUE(active_only)) {
    # Best-effort: the portal seems to use strings like "Serie activa" / "Serie inactiva"
    if (!("estat" %in% names(d))) {
      d$estat <- NA_character_
    }
    estat_chr <- tolower(as.character(d$estat))
    d <- d[
      stringr::str_detect(estat_chr, "\\bactiva\\b") %in% TRUE,
      ,
      drop = FALSE
    ]
  }

  d
}
