##' Block Metadata Class
##' 
##' This S4 object represents a single block metadata.
##' 
##' @param blockId Numeric value that holds the unique ID of the block metadata.
##' The GetBlockMetadata call returns this information in order to allow update
##' operation on block metadata, by specifying an existing ID in a
##' SaveBlockMetadata call.  Omitting this information in a SaveBlockMetadata
##' call will create a new block metadata.
##' @param selection A list of instances of the Dimension class.
##' @param metadata An instance of the Metadata class.
##' 
##' @export

BlockMetadata <- setClass(
	"BlockMetadata", 
	representation(
		blockId = "numeric", 
		selection = "list", 
		metadata = "Metadata"))


setValidity("BlockMetadata", function(object) {
	
	msg <- NULL
	valid <- TRUE


	if(length(object@selection) == 0) {
		valid <- FALSE
		msg <- c(msg, "The block metadata must have at least one dimension in the selection field.")
	}
	
	for(d in object@selection) {
		if(!class(d) == "Dimension" || !validObject(d)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed dimensions is not a valid instance of the class Dimension.")
		}
	}

	if(length(object@metadata) != 1) {
		valid <- FALSE
		msg <- c(msg, "The block metadata must have a metadata field.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
