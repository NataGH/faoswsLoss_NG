pkgname <- "faosws"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('faosws')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("GetCodeList")
### * GetCodeList

flush(stderr()); flush(stdout())

### Name: GetCodeList
### Title: Get Code List
### Aliases: GetCodeList

### ** Examples

## Not run: 
##D GetCodeList(domain = "agriculture", dataset = "agriculture",
##D             dimension = "geographicAreaM49", codes = c("4", "8", "112"))
## End(Not run)



cleanEx()
nameEx("GetCodeTree")
### * GetCodeTree

flush(stderr()); flush(stdout())

### Name: GetCodeTree
### Title: Get Code Tree
### Aliases: GetCodeTree

### ** Examples

## Not run: 
##D GetCodeTree(domain = "agriculture", dataset = "agriculture",
##D                      dimension = "geographicAreaM49", roots = "953")
## End(Not run)



cleanEx()
nameEx("GetData")
### * GetData

flush(stderr()); flush(stdout())

### Name: GetData
### Title: Get Data
### Aliases: GetData

### ** Examples

## Not run: 
##D # swsContext files are necessary for GetData to run (token may need to be updated)
##D GetTestEnvironment(
##D    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##D    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##D )
##D 
##D # Use GetCodeList to find all countries and commodities
##D areaCodes = GetCodeList("agriculture", "agriculture", "geographicAreaM49")
##D itemCodes = GetCodeList("agriculture", "agriculture", "measuredItemCPC")
##D 
##D # Pull data for one country and all commodities
##D dim1 = Dimension(name = "geographicAreaM49", keys = "12")
##D dim2 = Dimension(name = "measuredElement", keys = "5510")
##D dim3 = Dimension(name = "measuredItemCPC", keys = itemCodes[, code])
##D dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##D key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##D                  dimensions = list(dim1, dim2, dim3, dim4))
##D GetData(key)
##D 
##D # Pull data for all countries and one commodity
##D dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[, code])
##D dim2 = Dimension(name = "measuredElement", keys = "5510")
##D dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##D dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##D key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##D                  dimensions = list(dim1, dim2, dim3, dim4))
##D GetData(key)
## End(Not run)



cleanEx()
nameEx("Pivoting-class")
### * Pivoting-class

flush(stderr()); flush(stdout())

### Name: Pivoting-class
### Title: Pivoting Class Definition
### Aliases: Pivoting Pivoting-class

### ** Examples

pivot1 = Pivoting(code = "geographicAreaM49", ascending = TRUE)
pivot2 = Pivoting(code = "timePointYears", ascending = FALSE)
pivot3 = Pivoting(code = "measuredElement", ascending = FALSE)
pivot4 = Pivoting(code = "measuredItemCPC", ascending = FALSE)

## Not run: 
##D ##' # swsContext files are necessary for GetData to run (token may need to be updated)
##D GetTestEnvironment(
##D    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##D    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##D )
##D 
##D # Pull data for one country and all commodities
##D dim1 = Dimension(name = "geographicAreaM49", keys = c("12", "40"))
##D dim2 = Dimension(name = "measuredElement", keys = "5510")
##D dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##D dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##D key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##D                  dimensions = list(dim1, dim2, dim3, dim4))
##D 
##D GetData(key, pivoting = c(pivot1, pivot2, pivot3, pivot4))
##D # Effects are more visible if normalized = FALSE
##D GetData(key, pivoting = c(pivot1, pivot2, pivot3, pivot4), normalized = F)
##D GetData(key, pivoting = c(pivot2, pivot3, pivot4, pivot1), normalized = F)
## End(Not run)



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
