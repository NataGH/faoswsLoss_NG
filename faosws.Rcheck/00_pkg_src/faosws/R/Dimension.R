##' Dimension Class
##' 
##' A dimension is represented by its name and by the vector of selected
##' codes/keys.
##' 
##' @param name The name of the dimension to access, some examples are
##' "timePointYears", "measuredElement", or "geographicAreaM49".  These
##' correspond to the dimensions on the SWS, but the names are slightly
##' different.  Currently, if you want to find the name of a dimension, you
##' need to ask CIO.
##' @param keys The key codes specifying what values should be pulled for the
##' dimension.
##' 

Dimension <- setClass("Dimension",
    representation(
        name = "character",
        keys = "character"))

# A dimension is valid only if it has the name properly set up.
# The list of keys is not mandatory.
#
setValidity("Dimension", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@name) != 1) {
		valid <- FALSE
		msg <- c(msg, "The name of the dimension was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})

# The addKey method can be used to add a key to the dimension.
#
setGeneric("addKey<-", function(object, value) standardGeneric("addKey<-"))
setReplaceMethod(
	"addKey",
	"Dimension",	
	function(object, value) {
		object@keys <- c(object@keys, value)
		object
	}
)
