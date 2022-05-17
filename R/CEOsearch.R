#' Search for keywords in the labels of variables and responses of the survey data
#'
#' Easy and convenient access to the metadata of the "Centre
#' d'Estudis d'Opinio", the Catalan institution for polling and public opinion.
#' It allows to search for specific terms to obtain the details of the datasets available
#'
#' @encoding UTF-8
#' @param d Microdata retrieved from the CEO using the CEOdata() function. It is a data frame with variable labels.
#' @param keyword The character string defining the word / concept to look for within the microdata.
#' @param where A character vector specifying if the function should look amongst variable labels ("variables", default), or amongst value labels ("values").
#' @param translate Logical. When TRUE, it opens a browser with an automatic translation to English of the variable names and labels using Google Translate. Given the specificity of the terms, only the English translation is provided. Defaults to FALSE.
#' @export
#' @return A tibble with the set of variables that match the keyword ("Variable"). If the variables are requested, the second variable is their labels ("Label"), and if the values are required the second on is the value labels ("Value").
#' @examples
#'\dontrun{
#' # Retrieve a dataset to use the function
#' d <- CEOdata()
#'
#' # Get the whole set of variable labels
#' CEOsearch(d)
#'
#' # Get the whole set of value labels
#' CEOsearch(d, where = "values")
#'
#' # Search for specific variable names and variable labels with the string "edat" (age).
#' CEOsearch(d, keyword = "edat")
#'
#' # Search for specific variable names and variable labels with the string "edat" (age),
#' # and translate the results to English.
#' CEOsearch(d, keyword = "edat", translate = TRUE)
#'
#' # now for the combination of "valoracio" OR "covid" OR "govern".
#' CEOsearch(d, keyword = c("valoració", "covid",  "govern"))
#'}
CEOsearch <- function(
  d, keyword = NULL, where = "variables", translate = FALSE) {
  # If search is empty, just return all the entries
  # If browse, then open the automatic translation in the browser
  #
  # Limit by search
  #
  if (is.null(d) | !inherits(d, "data.frame")) {
    stop("The dataset must where to look for does not exist or is not a data frame.")
  }
  # Get either variable or value labels, all of them
  if (where == "variables") {
    v <- NULL
    for (i in 1:(dim(d)[2])) {
      v <- bind_rows(v,
        dplyr::as_tibble(data.frame(Variable = names(d)[i],
               Label = ifelse(!is.null(attr(d[[names(d)[i]]], "label")), 
                             attr(d[[names(d)[i]]], "label"), 
                             NA))))
    }
  } else if (where == "values") {
    v <- NULL
    for (i in 1:(dim(d)[2])) {
      if (!is.null(attr(d[[names(d)[i]]], "levels"))) {
        v <- bind_rows(v,
          dplyr::as_tibble(expand.grid(Variable = names(d)[i],
                 Value = attr(d[[names(d)[i]]], "levels"))))
      }
    }
  }
  #v <- dplyr::mutate_if(v, is.character, as.factor)
  # Search terms or return the whole set of variable/value labels
  if (is.null(keyword)) {
    return(v)
  } else { 
    if (!is.character(keyword)) {
      stop("The 'keyword' argument must be character.")
    }
    keyword <- tolower(keyword)
    keyword.strings <- stringr::str_trim(keyword)
    message(paste0("Looking for entries with: ", paste(keyword.strings, collapse = " OR ")))
    # Get the values that match the given string of text in any of
    # the columns
    columns.to.search <- names(v)
    v.match <- v |>
      dplyr::mutate(Original.Variable = Variable) |>
      dplyr::mutate_at(columns.to.search, tolower) %>% #|>
      #{function(x) dplyr::filter_all(dplyr::any_vars(stringr::str_detect(x, pattern = paste(search.strings, collapse = "|"))))}() |>
      dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = paste(keyword.strings, collapse = "|")))) |>
      dplyr::select(Original.Variable) |>
      {function(x) unlist(x, use.names = FALSE)}()
    if (length(v.match) < 1) {
      message(paste0("There are no entries with the string '",
                  keyword,
                  "'.\nYou may want to reduce the scope or change the text."))
      v <- v[0,]
    }
    v <- v |>
      dplyr::filter(Variable %in% v.match)
  }
  # Open the URLs of the matches
  # Deal with translations if necessary
  #
  if (translate) {
    if (is.null(dim(v))) {
      message("No entries to translate.")
    } else {
      text.translate <- NULL
      if (dim(v)[1] > 1) {
        for (i in 1:(dim(v)[1])) {
          if (i == 1) {
            text.translate <- toString(data.frame(v[i,]))
          } else {
            text.translate <- paste0(text.translate, "%0A", toString(data.frame(v[i,])))
          }
        }
      } else if (dim(v)[1] == 1) {
        text.translate <- toString(data.frame(v[1,]))
      }
      url.to.open <- paste0("https://translate.google.com/?sl=ca&tl=en&text=", text.translate, "&op=translate")
      browseURL(url.to.open)
    }
  }
  #
  return(v)
}

