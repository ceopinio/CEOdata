#' Internal function to get the metadata of CEO surveys into cache
#'
#' Used when loading the package, it gets the last update of the available
#' meta data of CEO surveys, cleans it and makes it ready for the rest
#' of the functions in the package.
#' @keywords internal
#' @encoding UTF-8
the <- new.env(parent = emptyenv())
CEOmetadata <- function() {
  if (is.null(the$CEOmetadata)) {
    the$CEOmetadata <- getCEOmetadata()
  }
  return(the$CEOmetadata)
}
getCEOmetadata <- function() {
  #
  # Define URL with the main table that contains the register of all surveys
  #
  #  # CSV, TSV, ... all fail because there are newlines in the data
  #  # Therefore a more structured system is required: JSON
  url.ceo.table <- "https://analisi.transparenciacatalunya.cat/api/views/m5mb-xt5e/rows.json?accessType=DOWNLOAD&sorting=true"
  try({ceo.meta <- jsonlite::fromJSON(url.ceo.table)}, silent = TRUE)
  if (exists(quote(ceo.meta))) {
    ceo.meta.table <- ceo.meta[[1]]
    ceo.table <- ceo.meta[[2]]
    ceo.table <- t(as.data.frame(lapply(ceo.table, read.ceo.json)))
    ceo.table <- as_tibble(as.data.frame(ceo.table))
    names(ceo.table) <- ceo.meta.table[[1]]$columns$name
    # Manually transform non-ASCII column names so that this can be packaged
    # and pass through CRAN
    # But only for variables that are going to be somewhat transformed
    names(ceo.table)[grep("Enlla.$", names(ceo.table))] <- "Enllac"
    names(ceo.table)[grep("Enlla. matriu de dades$", names(ceo.table))] <- "Enllac matriu de dades"
    names(ceo.table)[grep("M.tode de recollida de dades$", names(ceo.table))] <- "Metode de recollida de dades"
    names(ceo.table)[grep(".mbit territorial", names(ceo.table))] <- "Ambit territorial"
    names(ceo.table)[grep("T.tol enquesta", names(ceo.table))] <- "Titol enquesta"
    names(ceo.table)[grep("T.tol estudi", names(ceo.table))] <- "Titol estudi"
    CEOmeta <- ceo.table |>
      dplyr::select(-c(sid, id, position, created_at,
                       created_meta, updated_at,
                       updated_meta, meta)) |>
      dplyr::mutate(REO = factor(REO, levels = rev(REO))) |>
      dplyr::mutate(`Metodologia enquesta` = factor(`Metodologia enquesta`)) |>
      dplyr::mutate(`Metode de recollida de dades` = factor(`Metode de recollida de dades`)) |>
      dplyr::mutate(`Ambit territorial` = factor(`Ambit territorial`)) |>
      dplyr::mutate(`Dia inici treball de camp` = as.Date(stringr::str_sub(`Dia inici treball de camp`, 1L, 10L), format = "%Y-%m-%d")) |>
      dplyr::mutate(`Dia final treball de camp` = as.Date(stringr::str_sub(`Dia final treball de camp`, 1L, 10L), format = "%Y-%m-%d")) |>
      dplyr::mutate(`Any d'entrada al REO` = as.integer(`Any d'entrada al REO`, format = "")) |>
      dplyr::mutate(`Data d'alta al REO` = as.Date(stringr::str_sub(`Data d'alta al REO`, 1L, 10L), format = "%Y-%m-%d")) |>
      dplyr::mutate(`Mostra estudis quantitatius` = as.numeric(`Mostra estudis quantitatius`)) |>
      dplyr::mutate(Cost = as.numeric(Cost)) |>
      dplyr::mutate(microdata_available = ifelse(is.na(`Enllac matriu de dades`), FALSE, TRUE)) 
    return(CEOmeta)
  } else {
    message("A problem downloading the metadata has occurred. The server may be temporarily down, or the file name has changed. Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating 'Problem with metadata file'")
    return(NULL)
  }
}


#' Import metadata from the "Centre d'Estudis d'Opinio"
#'
#' Easy and convenient access to the metadata of the "Centre
#' d'Estudis d'Opinio", the Catalan institution for polling and public opinion.
#' It allows to search for specific terms to obtain the details of the datasets available
#'
#' @encoding UTF-8
#' @param reo Character vector of length one that allows to get the metadata only of a specific REO (Registre d'Estudis d'Opinio, the internal register ID used by the CEO) to download. When not NULL it has precedence with the search, date_start and date_end arguments.
#' @param search Character vector with keywords to look for within several columns of the CEO metadata (title, summary, objectives and tags -descriptors-). Each element of the vector is strictly evaluated (all words are considered to be found in the format they appear, like in "AND"), while by using several elements in the vector the search works like an "OR" clause. Lower or upper cases are not considered.
#' @param date_start Character vector with a starting date ("YYYY-MM-DD") for the data.
#' @param date_end Character vector with an end date ("YYYY-MM-DD") for the data.
#' @param browse Logical value. When turned to TRUE, the browser opens the URLs of the required surveys. Only a maximum of 10 entries are opened.
#' @param browse_translate When opening the relevant entries in the browser (browse must be TRUE), use automatic translation to the language specified using Google Translate ('oc' for Occitan/Aranese, 'de' to German, 'en' to English, 'eu' to Basque, 'gl' for Galician or 'sp' to Spanish).
#' @param browse_force Logical value. When TRUE it overcomes the limitation of only opening a maximum of 10 URLs. Use it with caution.
#' @export
#' @return A tibble with the metadata of the surveys produced by the CEO.
#' @examples
#'\dontrun{
#' # Retrieve the metadata of the surveys ever produced by the CEO:
#' meta <- CEOmeta()
#' dim(meta)
#'
#' # Search for specific terms in any of the metadata fields
#' # in this case, "internet".
#' CEOmeta(search = "internet")
#'
#' # now for the combination of "Medi" AND "Ambient"
#' CEOmeta(search = "Medi ambient")
#'
#' # now for the combination of ("Medi" AND "Ambient") OR "Municipi"
#' CEOmeta(search = c("Medi ambient", "Municipi"))
#'
#' # Search for all registers starting in 2020
#' CEOmeta(date_start = "2020-01-01")
#'
#' # Get the entry for a specific study (REO) and open its description in a browser
#' CEOmeta(reo = "746", browse = TRUE)
#'}
CEOmeta <- function(
  reo = NULL,
  search = NULL, date_start = NA, date_end = NA,
  browse = FALSE, browse_translate = NULL, browse_force = FALSE) {
  # If search is not empty, return parts according to searched fields
  # If search is empty, just return all the metadata
  # If browse, then open the URLs in the browser
  # Start with the whole cached data, and keep on subsetting
  d <- CEOmetadata()
  #
  # Limit by search
  #
  if (!is.null(reo)) {
    if (!is.character(reo)) {
      stop("The 'reo' argument must be character.")
    }
    d <- d |>
      filter(REO == reo)
  } else if (!is.null(search)) {
    if (!is.character(search)) {
      stop("The 'search' argument must be character.")
    }
    search <- tolower(search)
    search.strings <- stringr::str_trim(search)
    message(paste0("Looking for entries with: ", paste(search.strings, collapse = " OR ")))
    columns.to.search <- c("Titol enquesta", "Titol estudi",
                           "Objectius", "Resum",
                           "Descriptors")
    # Get the REO values that match the given string of text in any of
    # the columns considered
    reo.match <- d |>
      dplyr::select(REO, columns.to.search) |>
      dplyr::mutate_at(columns.to.search, tolower) %>% #|>
      #{function(x) dplyr::filter_all(dplyr::any_vars(stringr::str_detect(x, pattern = paste(search.strings, collapse = "|"))))}() |>
      dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = paste(search.strings, collapse = "|")))) |>
      dplyr::select(REO) |>
      {function(x) unlist(x, use.names = FALSE)}()
    if (length(reo.match) < 1) {
      stop(paste0("There are no entries with the string '",
                  search,
                  "'.\nYou may want to reduce the scope or change the text."))
    }
    d <- d |>
      dplyr::filter(REO %in% reo.match)
  }
  #
  # Limit by date
  #
  if (!is.na(date_start)) {
    d <- d |>
      dplyr::filter(`Data d'alta al REO` >= date_start)
  }
  if (!is.na(date_end)) {
    d <- d |>
      dplyr::filter(`Data d'alta al REO` <= date_end)
  }
  #
  # Open the URLs of the matches
  # Deal with translations if necessary
  #
  if (browse) {
    if (dim(d)[1] <= 10 | (browse_force)) {
      for (i in 1:(dim(d)[1])) {
        url.to.open <- d$`Enllac`[i]
        if (!is.null(browse_translate)) {
          if (browse_translate == "oc") { # For occitan, use apertium
            url.to.open <- paste0("https://www.apertium.org/index.eng.html#webpageTranslation?dir=cat-oci&qW=", url.to.open)
          } else { # Use google translate
            url.to.open <- paste0("https://",
                                  gsub("\\.", "-", urltools::domain(url.to.open)),
                                  ".translate.goog/",
                                  sub("http.+//[^/]*", "", url.to.open),
                                  "&_x_tr_sl=ca&_x_tr_tl=",
                                  browse_translate)
          }
        }
        browseURL(url.to.open)
        Sys.sleep(0.05)
      }
    }
  }
  #
  return(d)
}

#' Internal function to be able to properly read the JSON from CEO
#'
#' Used to address the limitations of the JSON format provided
#'
#' @keywords internal
#' @encoding UTF-8
#' @param x JSON data structure
read.ceo.json <- function(x) {
  row <- rep(NA, length(x))
  for (i in 1:length(x)) {
    element <- x[[i]]
    if (length(element) == 0) {
      # if there is nothing, return NA
      row[i] <- NA
    } else if (length(element) == 1) {
      # if there is only one element, get it
      row[i] <- element
    } else {
      # if there is more than one element,
      # only take care of the first element
      row[i] <- element[1]
    }
  }
  return(row)
}

