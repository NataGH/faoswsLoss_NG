MetadataElement <- setClass("MetadataElement", 
	representation(
		code = "character", 
		description = "character",
		value = "character"))

setValidity("MetadataElement", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code field of the metadata element was not properly set.")
	}

	if(length(object@value) != 1) {
		valid <- FALSE
		msg <- c(msg, "The value field of the metadata element was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
