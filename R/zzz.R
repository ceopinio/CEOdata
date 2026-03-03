CEOdataStartupMessage <- function()
{
  msg <- c(paste0("CEOdata version ", 
           utils::packageVersion("CEOdata")),
           "\nThis package needs a working Internet connection to effectively run.",
           "\nPlease acknowledge the CEO in your publications.\nType \"vignette('using_CEOdata')\" or \"vignette('cheatsheet')\" for basic help.",
           "\n\nThis package, by default, transforms the data gathered from the CEO\ninto pure-R factors. If you want to keep the SPSS labelled format\nyou can use 'raw = TRUE' when calling its functions.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- CEOdataStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'CEOdata' version", utils::packageVersion("CEOdata"))
  packageStartupMessage(msg)      
  invisible()
#  options(encoding = "UTF-8")
}

# To avoid notes in R CMD Check
if (getRversion() >= "2.15.1")  utils::globalVariables(
  c(
  "codi_serie", "estat", "titol_serie", "mode_admin", 
  "reo", "univers", "microdades_1", "microdades_2", 
  "data_inici", "data_fi",
  "REO", ".", "Data d'alta al REO", 
  "Variable", "Original.Variable",
  "sid", "id", "position", "created_at", "created_meta",
  "updated_at", "updated_meta", "meta",
  "Metodologia enquesta", "Metode de recollida de dades", 
  "Ambit territorial", "Dia inici treball de camp", 
  "Dia final treball de camp", "Any d'entrada al REO", 
  "Mostra estudis quantitatius", "Cost", 
  "Enllac matriu de dades"
  )
)