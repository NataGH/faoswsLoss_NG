Metadata <- setClass("Metadata", 
	representation(
		code = "character", 
		description = "character",
		language = "character",
		elements = "list"))

setValidity("Metadata", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code field of the metadata was not properly set.")
	}

	if(length(object@language) != 1) {
		valid <- FALSE
		msg <- c(msg, "The language field of the metadata was not properly set.")
	}
	
	for(d in object@elements) {
		if(!class(d) == "MetadataElement" || !validObject(d)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed metadata elements is not a valid instance of the class MetadataElement.")
		}
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
