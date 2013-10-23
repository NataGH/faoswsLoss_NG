Pivoting <- setClass("Pivoting", 
	representation(code = "character", ascending = "logical"),
	prototype(ascending = TRUE))

setValidity("Pivoting", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@code) != 1) {
		valid <- FALSE
		msg <- c(msg, "The code attribute was not properly set.")
	}

	if(length(object@ascending) != 1) {
		valid <- FALSE
		msg <- c(msg, "The ascending attribute was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
