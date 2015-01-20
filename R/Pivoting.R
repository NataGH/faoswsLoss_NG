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
##' @param code A character value containing the descriptive string of the
##' referred dimension (i.e. "geographicAreaM49").
##' @param ascending Logical, indicates the sort direction to be applied to the
##' specified dimension.
##' 

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
