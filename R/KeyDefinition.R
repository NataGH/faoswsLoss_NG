##' Definition for KeyDefinition Class
##' 
##' @slot code A character value holding the name of the dimension in the
##' system (for example, "geographicAreaM49").
##' @slot description A character value holding the description of the
##' dimension.
##' @slot type A character value.  Valid values are "normal",
##' "measurementUnit", and "time".
##' 

KeyDefinition <- setClass("KeyDefinition",
    representation(
        code = "character",
        description = "character",
        type = "character"))

# A key definition need to define at least its code. The rest of the fields is optional.
#
setValidity("KeyDefinition", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code of the key definition was not properly set.")
	}
    
    if(!object@type %in% c("normal", "measurementUnit", "time")) {
        valid <- FALSE
        msg <- c(msg,
            'The type must be one of "normal", "measurementUnit", or "time"')
    }

	if (valid) {
		TRUE
	} else {
		msg
	}
})
