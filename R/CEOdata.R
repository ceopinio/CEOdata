#' Import datasets / microdata from the "Centre d'Estudis d'Opinio"
#'
#' Easy and convenient access to the datasets / microdata of the "Centre
#' d'Estudis d'Opinio", the Catalan institution for polling and public opinion.
#' The package uses the data stored in the servers of the CEO and returns it in
#' a tidy format (tibble).
#' 
#' It works either by specifying the kind of merged barometer (using the
#' \code{kind} argument), or either providing a singular study (using the
#' \code{reo} argument).
#'
#' @encoding UTF-8
#' @param kind Character vector with the sort of microdata required. Defaults to "barometer", that contains the whole set of Barometers from 2014 (presential interviews). "barometer_until_2013" contains the interviews performed by phone until 2013, with a somewhat different questionnaire and structure. For such dataset you need a third-party software installed in your computer to be able to uncompress the RAR original file. It is the option by default. But if a specific reo study is requested in the \code{reo} argument, then the \code{kind} argument does not apply anymore and only a specific study is retrieved.
#' @param reo Character vector of length one that allows to get the dataset of a specific REO study (Registre d'Estudis d'Opinio, the internal register ID used by the CEO) to download. By default (when \code{reo = NA}) it uses the \code{kind} argument. Not all the studies carried on by the CEO (and therefore listed in the \code{CEOmeta()} function call) have microdata available. Only the ones that return TRUE to the column \code{microdata_available} in \code{CEOmeta()}.
#' @param raw Logical value to indicate if SPSS labels are transformed into factors. Defaults to FALSE. Otherwise, when TRUE, it returns the matrices as imported by haven::read_spss() without modification. Does not apply to data from singular REOs, only to barometers retrieved using \code{kind}.
#' @param extra_variables Logical value as to whether include (default) complementary variables such as date (Data). Defaults to TRUE. Names of such new variables only use upper case in the first letter. Extra variables are added at the end. Does not apply to data from singular REOs, only to barometers retrieved using \code{kind}.
#' @param date_start Character vector with a starting date ("YYYY-MM-DD") for the data. It only applies to the barometers retrieved using \code{kind}, not to other studies.
#' @param date_end Character vector with an end date ("YYYY-MM-DD") for the data. It only applies to the barometers retrieved using \code{kind}, not to other studies.
#' @export
#' @return A tibble with the individuals' responses to the questionnaire retrieved.
#' @examples
#'\dontrun{
#' Get the merged barometer from 2014, by default (assume kind = "barometer").
#' d <- CEOdata()
#'
#' # Get the number of individuals surveyed and the number of variables recorded.
#' dim(d)
#'
#' # Get the identifiers of the different Barometers retrieved
#' unique(d$BOP_NUM)
#'
#' Get a specific study
#' d746 <- CEOdata(reo = "746")
#'}
CEOdata <- function(kind = "barometer",
                    reo = NA,
                    raw = FALSE,
                    extra_variables = TRUE,
                    date_start = NA, date_end = NA) {
  if (is.na(reo)) {
    #
    # Define URLs
    #
    url.phone.barometer <- "https://ceo.gencat.cat/web/.content/20_barometre/Matrius_BOP/2013_Microdades_anonimitzades_fusio_cine_telf.zip"
    file.phone.barometer.rar <- "2013_Microdades_anonimitzades_fusio_cine_telf.zip"
    file.phone.barometer <- "Microdades anonimitzades fusio cine telf.sav"
    url.presential.barometer <- "https://ceo.gencat.cat/web/.content/20_barometre/Matrius_BOP/Microdades_Des20.zip.zip"
    file.presential.barometer <- "Microdades anonimitzada fusio presencial.sav"
    # Process barometer merged from 2014
    if (kind == "barometer") {
      message("Downloading the barometer.")
      tmp <- tempfile()
      try({download.value <- download.file(url.presential.barometer, tmp)}, silent = TRUE)
      if (exists(quote(download.value))) {
        if (download.value == 0) { # success downloading the file
          file <- unzip(tmp, file.presential.barometer)
          message("Converting the original data into R. This may take a while.")
          d <- haven::read_spss(file)
          if (file.exists(file)) {
            unlink(file)
          }
        }
      } else {
        message("A problem downloading the barometer file has occurred. The server may be temporarily down, or the file name has changed. Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating 'Problem with barometer'")
        return(NULL)
      }
    }
    # Process barometer merged until 2013
    if (kind == "barometer_until_2013") {
      message("Downloading the barometer until 2013.")
      try({download.value <- download.file(url.phone.barometer, file.phone.barometer.rar)}, silent = TRUE)
      if (exists(quote(download.value))) {
        if (download.value == 0) { # success downloading the file
          # This must be fixed because the original file as of 211027 is not a zip file, but a RAR file
          message("Uncompressing the original downloaded file. This may take a while.")
          system("unrar e 2013_Microdades_anonimitzades_fusio_cine_telf.zip")
          file <- file.phone.barometer
          message("Converting the original data into R. This may take a while.")
          d <- haven::read_spss(file)
          names(d) <- toupper(names(d))
          # Add variable REO
          d <- d |>
            dplyr::mutate(REO = as.numeric(stringr::str_extract(BOP_NUM, "...$")))
          if (file.exists(file)) {
            unlink(file)
          }
          if (file.exists(file.phone.barometer.rar)) {
            unlink(file.phone.barometer.rar)
          }
        }
      } else {
        message("A problem downloading the barometer until 2013 file has occurred. The server may be temporarily down, or the file name has changed. Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating 'Problem with barometer until 2013'")
        return(NULL)
      }
    }
    message("Post-processing the data. This may take a while.")
    # Arrange the barometer to process
    # Arrange factors
    if (!raw) { # Transform SPSS labels into proper R factors
      is_haven_labelled <- function(x) {
        ifelse(length(which(class(x) %in% "haven_labelled")) > 0,
               TRUE, FALSE)
      }
      d <- d |>
        dplyr::mutate_if(is_haven_labelled, haven::as_factor, levels = "default")
    }
    # Add extra variables (date, ...)
    if (extra_variables) {
      if (kind == "barometer_until_2013") {
        d <- d |>
          dplyr::mutate(Data = as.Date(paste(ANY,
                                             sprintf("%02d", MES),
                                             28, sep = "-")))
      } else {
        d <- d |>
          dplyr::mutate(Data = as.Date(paste(ANY,
                                             sprintf("%02d", MES),
                                             ifelse(is.na(DIA), 28, DIA),
                                             sep = "-")))
      }
    }
    #
    # Filter by dates
    #
    if (!is.na(date_start)) {
      d <- d |>
        dplyr::filter(Data >= date_start)
    }
    if (!is.na(date_end)) {
      d <- d |>
        dplyr::filter(Data <= date_end)
    }
  } else {
    #
    # Serve only a single, untreated REO
    #
    if (is.character(reo)) {
      if (length(reo) == 1) {
        url.reo <- CEOmetadata()$`Enllac matriu de dades`[CEOmetadata()$REO == reo]
        if (!is.na(url.reo)) {
          try({d <- haven::read_spss(url.reo)}, silent = TRUE)
          if (!exists(quote(d))) {
            message("A problem downloading the specific barometer file has occurred. The server may be temporarily down, or the file name has changed. Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating 'Problem with barometer'")
            return(NULL)
          }
        } else {
          message(paste0("There is no dataset available for REO ", reo))
          return(NULL)
        }
      } else {
        message("'reo' must pass only a single REO.")
        return(NULL)
      }
    } else {
      stop("'reo' must be a character vector.")
    }
  }
  #
  return(d)
}

