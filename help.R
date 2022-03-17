#' Datasets of the CEO (Centre d'Estudis d'Opinio). Opinion polls in Catalonia.
#'
#' Easy and convenient access to the datasets / microdata of the "Centre
#' d'Estudis d'Opinio", the catalan institution for polling and public opinion.
#' The package uses the data stored in the servers of the CEO and returns it in
#' a tidy format (tibble).
#'
#' @encoding UTF-8
#' @references \url{http://xavier-fim.net/packages/CEOdata/}.
#' @importFrom haven read_spss as_factor
#' @importFrom dplyr mutate_if %>% mutate filter select as_tibble bind_rows
#' @importFrom utils download.file unzip browseURL
#' @importFrom stringr str_detect str_extract str_sub str_trim
#' @importFrom urltools domain
#' @importFrom jsonlite fromJSON
#' @docType package
#' @name CEOdata
NULL
