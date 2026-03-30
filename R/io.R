# Internal I/O helpers for CEOdata
# These functions are not exported.

#' Download a remote file to a temporary location
#'
#' @param url Character scalar. Remote URL to download.
#' @param quiet Logical. If TRUE, suppress download progress.
#' @param timeout Numeric. Timeout in seconds (best-effort; depends on download method).
#' @return Character scalar path to the downloaded file.
#' @keywords internal
ceodata_download <- function(url, quiet = TRUE, timeout = 60) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    stop("`url` must be a non-empty character scalar.", call. = FALSE)
  }

  # Guess file extension from URL (for nicer temp file names)
  ext <- tolower(sub(".*\\.", "", url))
  if (!nzchar(ext) || nchar(ext) > 5) ext <- "bin"

  dest <- tempfile(pattern = "ceodata_", fileext = paste0(".", ext))

  # Best-effort timeout (download.file uses different methods across platforms)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(timeout, 1))

  status <- tryCatch(
    utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet),
    error = function(e) e
  )

  if (inherits(status, "error")) {
    # Clean up partial file if any
    if (file.exists(dest)) unlink(dest, force = TRUE)
    stop(
      sprintf("Failed to download '%s': %s", url, conditionMessage(status)),
      call. = FALSE
    )
  }

  if (!is.numeric(status) || status != 0) {
    if (file.exists(dest)) unlink(dest, force = TRUE)
    stop(
      sprintf("Failed to download '%s' (download.file exit status: %s).", url, status),
      call. = FALSE
    )
  }

  if (!file.exists(dest) || file.info(dest)$size == 0) {
    if (file.exists(dest)) unlink(dest, force = TRUE)
    stop(sprintf("Downloaded file from '%s' is missing or empty.", url), call. = FALSE)
  }

  dest
}

#' Detect input type from a path or URL
#'
#' @param x Character scalar. File path or URL.
#' @return One of "sav" or "zip".
#' @keywords internal
ceodata_detect_type <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`x` must be a non-empty character scalar.", call. = FALSE)
  }
  x_low <- tolower(x)

  if (grepl("\\.sav($|\\?)", x_low)) return("sav")
  if (grepl("\\.zip($|\\?)", x_low)) return("zip")

  stop(sprintf("Unsupported file type for '%s'. Expected .sav or .zip.", x), call. = FALSE)
}

#' Extract a single .sav file from a zip archive
#'
#' @param zip_path Character scalar. Path to a zip file.
#' @return Character scalar path to the extracted .sav file (temporary).
#' @keywords internal
ceodata_sav_from_zip <- function(zip_path) {
  if (!is.character(zip_path) || length(zip_path) != 1L || is.na(zip_path) || !nzchar(zip_path)) {
    stop("`zip_path` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!file.exists(zip_path)) {
    stop(sprintf("Zip file does not exist: '%s'.", zip_path), call. = FALSE)
  }

  listing <- tryCatch(
    utils::unzip(zip_path, list = TRUE),
    error = function(e) e
  )
  if (inherits(listing, "error")) {
    stop(sprintf("Unable to list zip contents '%s': %s", zip_path, conditionMessage(listing)), call. = FALSE)
  }

  sav_files <- listing$Name[grepl("\\.sav$", tolower(listing$Name))]
  if (length(sav_files) == 0L) {
    stop(sprintf("Zip '%s' does not contain any .sav file.", zip_path), call. = FALSE)
  }
  if (length(sav_files) > 1L) {
    stop(
      sprintf(
        "Zip '%s' contains multiple .sav files (%s). Expected exactly one.",
        zip_path,
        paste(basename(sav_files), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Extract to a dedicated temp directory
  out_dir <- tempdir()
  extracted <- tryCatch(
    utils::unzip(zip_path, files = sav_files, exdir = out_dir),
    error = function(e) e
  )
  if (inherits(extracted, "error")) {
    stop(sprintf("Unable to extract .sav from '%s': %s", zip_path, conditionMessage(extracted)), call. = FALSE)
  }

  # unzip() returns the path(s) extracted (may be relative to out_dir)
  sav_path <- extracted[1]
  if (!file.exists(sav_path)) {
    # Some unzip methods return names; build full path
    candidate <- file.path(out_dir, sav_files[1])
    if (file.exists(candidate)) sav_path <- candidate
  }

  if (!file.exists(sav_path)) {
    stop(sprintf("Extraction succeeded but .sav file not found on disk for '%s'.", zip_path), call. = FALSE)
  }

  sav_path
}

#' Read an SPSS .sav file and optionally convert labelled vectors to factors
#'
#' @param sav_path Character scalar. Path to a .sav file.
#' @param raw Logical. If TRUE, return raw haven-labelled vectors without conversion.
#' @return A data.frame/tibble as returned by haven::read_spss(), optionally post-processed.
#' @keywords internal
ceodata_read_sav <- function(sav_path, raw = FALSE) {
  if (!is.character(sav_path) || length(sav_path) != 1L || is.na(sav_path) || !nzchar(sav_path)) {
    stop("`sav_path` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!file.exists(sav_path)) {
    stop(sprintf("SAV file does not exist: '%s'.", sav_path), call. = FALSE)
  }

  d <- tryCatch(
    haven::read_spss(sav_path),
    error = function(e) e
  )
  if (inherits(d, "error")) {
    stop(sprintf("Failed to read .sav file '%s': %s", sav_path, conditionMessage(d)), call. = FALSE)
  }

  if (!isTRUE(raw)) {
    # Convert haven_labelled to factors (similar logic to existing CEOdata())
    is_haven_labelled <- function(x) inherits(x, "haven_labelled")
    d <- dplyr::mutate_if(d, is_haven_labelled, haven::as_factor, levels = "default")
  }

  d
}

#' Download and read a dataset from a URL (.sav or .zip containing a single .sav)
#'
#' @param url Character scalar. URL to .sav or .zip.
#' @param raw Logical. If TRUE, do not convert labelled vectors.
#' @return A data.frame/tibble.
#' @keywords internal
ceodata_download_and_read <- function(url, raw = FALSE) {
  local <- ceodata_download(url)
  on.exit({
    if (file.exists(local)) unlink(local, force = TRUE)
  }, add = TRUE)

  type <- ceodata_detect_type(url)

  if (type == "zip") {
    sav <- ceodata_sav_from_zip(local)
    on.exit({
      if (file.exists(sav)) unlink(sav, force = TRUE)
    }, add = TRUE)
  } else {
    sav <- local
  }

  ceodata_read_sav(sav_path = sav, raw = raw)
}
