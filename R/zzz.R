CEOdataStartupMessage <- function()
{
  msg <- c(paste0("CEOdata version ", 
           utils::packageVersion("CEOdata")),
           "\nThis package needs a working Internet connection to effectively run.",
           "\nPlease acknowledge the CEO in your publications.\nType \"vignette('using_CEOdata')\" or \"vignette('cheatsheet')\" for basic help.")
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
  options(encoding = "UTF-8")
}
