##' Dataset Key Class
##' 
##' @param domain A character value specifying the domain of interest, such as
##' "agriculture".
##' @param dataset A character value specifying the dataset within the domain,
##' such as "agriculture".
##' @param dimensions A list of objects, each of class Dimension.
##' @param sessionId The ID of the session from which data should be accessed.
##' If NULL, the database is used directly.
##' 
##' @note Many of these variables (domain, dataset, and dimensions) can be
##' found by examining some of the swsContext objects which are created in a
##' GetTestEnvironment call (in a debug session).  Moreover, after executing
##' GetTestEnvironment, the swsContext.datasets object will contain a list of
##' objects of type DatasetKey.
##' 

DatasetKey <- setClass("DatasetKey", 
	representation(
		domain = "character", 
		dataset = "character",
		dimensions = "list",
		sessionId = "integer"))

setValidity("DatasetKey", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@domain) != 1) {
		valid <- FALSE
		msg <- c(msg, "The domain of the key was not properly set.")
	}

	if(length(object@dataset) != 1) {
		valid <- FALSE
		msg <- c(msg, "The dataset of the key was not properly set.")
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
