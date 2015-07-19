## Windows installation
getlibrary <- function(...){
  if(sessionInfo()$R.version$major==3 & sessionInfo()$R.version$minor>=0.2){
    librarynames <- (...)
    alreadyInstalled <- installed.packages()[,"Package"]
    needToInstall <- !librarynames %in% alreadyInstalled
    if (any(needToInstall)) {
      installResults <- sapply(librarynames[needToInstall],install.packages)
      paste0("installed ",librarynames[needToInstall])
    }
    else(paste0("already installed ",alreadyInstalled[librarynames]))
  }
  else("You need version R 3.0.2 or higher")
}

getlibrary(c("Rtools", "devtools", "swirl"))

devtools::install_github("dgrtwo/rparse") # install rparse
devtools::install_github("dimagor/socraticswirl", ref = "fsi") # install SocraticSwril
