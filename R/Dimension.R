Dimension <- setClass("Dimension", representation(name = "character", keys = "character"))

setValidity("Dimension", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@name) != 1) {
		valid <- FALSE
		msg <- c(msg, "The name of the dimension was not properly set.")
	}

	if(length(object@keys) <= 0) {
		valid <- FALSE
		msg <- c(msg, "At least one key needs to be specified.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})

setGeneric("addKey<-", function(object, value) standardGeneric("addKey<-"))

setReplaceMethod(
	"addKey",
	"Dimension",	
	function(object, value) {
		object@keys <- c(object@keys, value)
		object
	})
