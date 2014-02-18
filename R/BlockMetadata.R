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
