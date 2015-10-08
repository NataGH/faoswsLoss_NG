##' Metadata Class Definition
##' 
##' This object represents the metadata associated to the block of data
##' specified by the keys held by the "selection" slot for BlockMetaData.
##' 
##' @slot code A character value identifying the metadata type represented
##' (for instance ACCURACY or GENERAL).
##' @slot description A character value of the description of the metadata
##' type.
##' @slot language Should be one of the following character values:
##' \itemize{
##'     \item "ar": Arabic
##'     \item "en": English
##'     \item "es": Spanish
##'     \item "fr": French
##'     \item "ru": Russian
##'     \item "zh": Chinese
##' }
##' @slot elements A list of objects of class MetadataElement.
##' 

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
    
    if(!object@language %in% c("ar", "en", "es", "fr", "ru", "zh")){
        valid <- FALSE
        msg <- c(msg, "The language field specified is invalid!")
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
