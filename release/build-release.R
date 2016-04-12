arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 2)

LIB <- arguments[1]
setwd(arguments[2])

# Get dependencies from description file
deps <- paste0(read.dcf("DESCRIPTION", fields=c("Depends", "Imports", "Suggests"))[1,], collapse = "") 
#Remove R dependency
deps <- gsub("\nR |\\(.*?\\)", "", deps)
deps <- strsplit(deps, ",?\n")[[1]]
deps <- deps[deps != ""]

# hardcode the fao-sws-cran repo for CRAN
local({r = getOption("repos") 
r["CRAN"] = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/"
options(repos = r)
})

# Get non-base packages
packs <- intersect(deps, available.packages()[, "Package"])

# Only install non-base packages
to_install <- setdiff(packs, installed.packages(lib.loc=LIB)[,"Package"])

# If packages aren't available, there's a warning. Convert it into an error.
options(warn=2)
if(length(to_install) > 0){
  install.packages(to_install, lib = LIB)
  #Report results
  message(sprintf("Package(s) %s installed", paste(to_install, collapse = ", ")))
  }

#Report results
not_installed <- setdiff(packs, to_install)
if(length(not_installed) > 0){
  message(sprintf("Package(s) %s not installed (already installed)", paste(not_installed, collapse = ", ")))
}