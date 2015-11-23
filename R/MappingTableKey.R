##' Mapping Table Key
##' 
##' @slot mappingTable Must be a character of length 1
##' @slot dimensions List of \code{\link{Dimension}} objects.
##' 
##' @export MappingTableKey
##' @export

MappingTableKey <- setClass("MappingTableKey", 
	representation(
		mappingTable = "character",
		dimensions = "list"))

setValidity("MappingTableKey", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@mappingTable) != 1) {
		valid <- FALSE
		msg <- c(msg, "The mappingTable of the key was not properly set.")
	}
	
	for(d in object@dimensions) {
		if(!class(d) == "Dimension" || !validObject(d)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed dimensions is not a valid instance of the class Dimension.")
		}
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
