pkgname <- "faoswsAupus"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('faoswsAupus')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("defineElementVariables")
### * defineElementVariables

flush(stderr()); flush(stdout())

### Name: defineElementVariables
### Title: Define Element Variables
### Aliases: defineElementVariables

### ** Examples

## Not run: 
##D exists("element51Num")
##D exists("element51Symb")
##D exists("ratio51Num")
##D defineElementVariables(51, faoswsAupus::aupusParam)
##D exists("element51Num")
##D exists("element51Symb")
##D exists("ratio51Num")
## End(Not run)



cleanEx()
nameEx("numberOfMissingElement")
### * numberOfMissingElement

flush(stderr()); flush(stdout())

### Name: numberOfMissingElement
### Title: Number of Missing Elements
### Aliases: numberOfMissingElement

### ** Examples

numberOfMissingElement(c(1,2,NA,NA,5),
                       c(1,NA,2,3,NA),
                       c(1,2,NA,4,NA))



cleanEx()
nameEx("numberOfTrendingElement")
### * numberOfTrendingElement

flush(stderr()); flush(stdout())

### Name: numberOfTrendingElement
### Title: Number of Trended Elements
### Aliases: numberOfTrendingElement

### ** Examples

numberOfTrendingElement(c("M","","","T",""),
                        c("M", "T", "M", "T", ""),
                        c("C", "C", "T", "T", "T"))



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
