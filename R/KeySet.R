KeySet <- setClass("KeySet", 
	representation(
		domain = "character", 
		dataset = "character",
		dimensions = "list",
		sessionId = "integer"))

setValidity("KeySet", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@domain) != 1) {
		valid <- FALSE
		msg <- c(msg, "The domain of the keyset was not properly set.")
	}

	if(length(object@dataset) != 1) {
		valid <- FALSE
		msg <- c(msg, "The dataset of the keyset was not properly set.")
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
