#' CEOdata: CEO microdata access from the Generalitat open data platform
#'
#' Provides easy and convenient access to microdata from the Centre d'Estudis d'Opinio (CEO).
#' Datasets are retrieved from the open data platform of the Generalitat de Catalunya
#' and returned as tidy tibbles.
#'
#' @encoding UTF-8
#' @importFrom haven read_spss as_factor
#' @importFrom dplyr mutate_if %>% mutate filter select as_tibble bind_rows
#' @importFrom utils download.file unzip browseURL
#' @importFrom stringr str_detect str_extract str_sub str_trim
#' @importFrom urltools domain
#' @importFrom jsonlite fromJSON
#' @keywords internal
"_PACKAGE"