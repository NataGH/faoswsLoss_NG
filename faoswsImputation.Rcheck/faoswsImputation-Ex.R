pkgname <- "faoswsImputation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('faoswsImputation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("getObservedExtrapolationRange")
### * getObservedExtrapolationRange

flush(stderr()); flush(stdout())

### Name: getObservedExtrapolationRange
### Title: Get Observed Extrapolation Range
### Aliases: getObservedExtrapolationRange

### ** Examples

x = c(NA, NA, NA, 1:7, NA, NA)
getObservedExtrapolationRange(x)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
