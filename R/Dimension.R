# S4 class modeling a dimension in a key set.
# A dimension is represented by its name and by the vector of selected codes.
#
Dimension <- setClass("Dimension", representation(name = "character", keys = "character"))

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
