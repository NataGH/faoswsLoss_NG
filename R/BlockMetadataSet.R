BlockMetadataSet <- setClass("BlockMetadataSet", 
	representation(
		keyDefinitions = "list",
		blockMetadata = "list"))

setValidity("BlockMetadataSet", function(object) {
	
	msg <- NULL
	valid <- TRUE

	for(k in object@keyDefinitions) {
		if(!class(k) == "KeyDefinition" || !validObject(k)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed key definitions is not a valid instance of the class KeyDefinition")
		}
	}

	for(b in object@blockMetadata) {
		if(!class(b) == "BlockMetadata" || !validObject(b)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed block metadata is not a valid instance of the class BlockMetadata")
		}
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
