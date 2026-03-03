#' Import datasets / microdata from the Centre d'Estudis d'Opinió
#'
#' Easy and convenient access to the datasets / microdata of the "Centre
#' d'Estudis d'Opinio", the Catalan institution for polling and public opinion.
#' The package uses the data stored in the open data platform of "Generalitat de 
#' Catalunya", the catalan government, and returns it in a tidy format (tibble). 
#' The function can return either:
#' (1) an accumulated microdata series (identified by `series`), or
#' (2) a single study microdata dataset (identified by `reo`).
#'
#' Accumulated series are obtained from the Dades Obertes catalogue and are
#' referenced by `codi_serie` (e.g. "BOP_presencial").
#'
#' @encoding UTF-8
#' @param series Character scalar indicating the accumulated microdata series to download
#'   (a `codi_serie` value returned by `CEOaccumulated_meta()`). Defaults to "BOP_presencial".
#'   Ignored if `reo` is provided.
#' @param reo Character scalar indicating the REO code of a single study to download.
#'   Default is NA (meaning: use `series`).
#' @param raw Logical. If FALSE (default), converts SPSS labelled vectors into R factors.
#'   If TRUE, returns the raw haven-labelled format.
#' @export
#' @return A tibble with individuals' responses to the requested dataset.
#' @examples
#' \dontrun{
#' # Default: accumulated microdata series (BOP_presencial)
#' d <- CEOdata()
#'
#' # Load another accumulated series by codi_serie
#' d_tel <- CEOdata(series = "BOP_telefonica")
#'
#' # Load a single study by REO
#' d1145 <- CEOdata(reo = "1145")
#' }

CEOdata <- function(series = "BOP_presencial",
                    reo = NA,
                    raw = FALSE) {

  # Validate `raw`
  if (!is.logical(raw) || length(raw) != 1L || is.na(raw)) {
    stop("'raw' must be a single logical value.", call. = FALSE)
  }

  # Validate mutual exclusivity:
  if (!missing(reo) && !missing(series) && !is.na(reo) && !is.na(series)) {
    stop("Provide either 'reo' or 'series', but not both.", call. = FALSE)
  }


  # ---- Mode 1: Single study (REO) ----
  if (!is.na(reo)) {

    if (!is.character(reo) || length(reo) != 1L || !nzchar(reo)) {
      stop("'reo' must be a character scalar (e.g. '1145').", call. = FALSE)
    }

    meta <- CEOmetadata()

    idx <- !is.na(meta$REO) & meta$REO == reo

    if (sum(idx) == 0) {
      stop(paste0("There is no dataset available for REO ", reo, "."), call. = FALSE)
    }
    if (sum(idx) > 1) {
      stop(paste0("Multiple entries found for REO ", reo, " in CEOmetadata()."), call. = FALSE)
    }

    url.reo <- meta$`Microdades 1`[idx]

    if (is.na(url.reo) || !nzchar(url.reo)) {
      stop(paste0("There is no dataset link available for REO ", reo, "."), call. = FALSE)
    }

    url.reo.low <- tolower(url.reo)
    if (!grepl("\\.sav($|\\?)", url.reo.low)) {
      if (grepl("\\.csv($|\\?)", url.reo.low)) {
        stop(
          paste0(
            "REO ", reo, " points to a CSV in 'Microdades 1' (", url.reo, "). ",
            "CEOdata currently supports only SPSS .sav links for single-study REO downloads."
          ),
          call. = FALSE
        )
      }
      stop(
        paste0(
          "REO ", reo, " has an unsupported format in 'Microdades 1' (", url.reo, "). ",
          "Expected a .sav link."
        ),
        call. = FALSE
      )
    }

    message("Downloading and reading REO dataset. This may take a while.")
    d <- ceodata_download_and_read(url.reo, raw = raw)

    return(d)
  }

  # ---- Mode 2: Accumulated series ----
  if (is.na(series)) {
    stop("'series' must be provided when 'reo' is NA.", call. = FALSE)
  }
  if (!is.character(series) || length(series) != 1L || !nzchar(series)) {
    stop("'series' must be a character scalar (e.g. 'BOP_presencial').", call. = FALSE)
  }

  m <- CEOaccumulated_meta(series = series)

  if (is.null(m) || nrow(m) == 0) {
    stop(paste0("Unknown accumulated series: ", series, "."), call. = FALSE)
  }
  if (nrow(m) > 1) {
    stop(paste0("Multiple entries found for accumulated series: ", series, "."), call. = FALSE)
  }

  url <- m$microdades_1[1]
  if (is.na(url) || !nzchar(url)) {
    stop(paste0("No 'microdades_1' link available for accumulated series: ", series, "."), call. = FALSE)
  }

  message("Downloading and reading accumulated series. This may take a while.")
  d <- ceodata_download_and_read(url, raw = raw)

  return(d)
}
