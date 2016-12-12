arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 3)

LIB <- arguments[1]
setwd(arguments[2])
FAOCRAN <- arguments[3]

# Get dependencies from description file
deps <- paste0(read.dcf("DESCRIPTION", fields=c("Depends", "Imports", "Suggests"))[1,], collapse = "")
#Remove R dependency
deps <- gsub("\nR | ?\\(.*?\\)", "", deps)
deps <- strsplit(deps, ",?\n")[[1]]
deps <- deps[deps != ""]

# Add devtools if we're testing
  # devtools and testthat as well
  # testthat has devtools as suggests.
  deps <- c(deps, "devtools", "testthat", "roxygen2")


# hardcode the fao-sws-cran repo for CRAN
options(repos = c("CRAN" = FAOCRAN))

# Get non-base packages
installed <- as.data.frame(installed.packages(lib.loc=.Library))

packs <- setdiff(deps, 
                   installed[installed[,"Priority" ] %in% c("base", "recommended") , "Package"])

# Only install non-base packages
to_install <- setdiff(packs, installed.packages(lib.loc=LIB)[,"Package"])

if(length(to_install) > 0){
  # If packages aren't available, there's a warning. Convert it into an error.  
  withCallingHandlers(
    install.packages(to_install, lib = LIB), 
    warning = function(w) stop(w, "Package install failed")
    )
  #Report results
  message(sprintf("Package(s) %s installed", paste(to_install, collapse = ", ")))
}

#Report results
not_installed <- setdiff(packs, to_install)
if(length(not_installed) > 0){
  message(sprintf("Package(s) %s not installed (already installed)", paste(not_installed, collapse = ", ")))
}

# RUN TESTS

library(devtools, lib.loc = LIB)
check(document=FALSE)
library(testthat, lib.loc = LIB)
test(pkg = ".", reporter = JunitReporter$new(file = "test-results.xml"))
#library(testthat, lib.loc = "C:/Users/campbells/Documents/R/win-library/3.2")
#test(pkg = ".")

