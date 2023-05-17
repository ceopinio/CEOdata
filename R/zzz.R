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
