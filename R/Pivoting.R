##' Pivoting Class Definition
##' 
##' This class represents a single dimension used for pivoting specification.
##' It is used in data retrieval methods to indicate the requested data
##' pivoting.
##' 
##' In order to properly specify a valid pivoting, a vector of Pivoting objects
##' has to be passed to the data retrieval code. The vector must include all
##' the dimensions that are defined for the target dataset. The sequence of the
##' Pivoting objects describe the requested order of extraction of the
##' corresponding dimensions and, in case of denormalized extractions, it puts
##' on the columns of the returned data.table the values corresponding to the
##' last Pivoting dimension.
##' 
##' @slot code A character value containing the descriptive string of the
##' referred dimension (i.e. "geographicAreaM49").
##' @slot ascending Logical, indicates the sort direction to be applied to the
##' specified dimension.
##' 
##' @return An object of class Pivoting
##' 
##' @examples
##' 
##' \dontrun{
##' pivot1 = Pivoting(code = "geographicAreaM49", ascending = TRUE)
##' pivot2 = Pivoting(code = "timePointYears", ascending = FALSE)
##' pivot3 = Pivoting(code = "measuredElement", ascending = FALSE)
##' pivot4 = Pivoting(code = "measuredItemCPC", ascending = FALSE)
##' 
##' ##' # swsContext files are necessary for GetData to run (token may need to be updated)
##' GetTestEnvironment(
##'    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##'    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##' )
##' 
##' # Pull data for one country and all commodities
##' dim1 = Dimension(name = "geographicAreaM49", keys = c("12", "40"))
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' 
##' GetData(key, pivoting = c(pivot1, pivot2, pivot3, pivot4))
##' # Effects are more visible if normalized = FALSE
##' GetData(key, pivoting = c(pivot1, pivot2, pivot3, pivot4), normalized = F)
##' GetData(key, pivoting = c(pivot2, pivot3, pivot4, pivot1), normalized = F)
##' }
##' 
##' @export

Pivoting <- setClass("Pivoting", 
	representation(code = "character", ascending = "logical"),
	prototype(ascending = TRUE))

setValidity("Pivoting", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code attribute was not properly set.")
	}

	if(length(object@ascending) != 1) {
		valid <- FALSE
		msg <- c(msg, "The ascending attribute was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
