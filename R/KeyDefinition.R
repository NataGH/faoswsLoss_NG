# S4 class modeling a key definition.
#
KeyDefinition <- setClass("KeyDefinition", representation(code = "character", description = "character", type = "character"))

# A key definition need to define at least its code. The rest of the fields is optional.
#
setValidity("KeyDefinition", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code of the key definition was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
