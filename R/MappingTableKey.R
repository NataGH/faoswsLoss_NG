##' Mapping Table Key
##' 
##' @slot mappingTable
##' @slot dimensions
##' 
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
