##' Save Block Metadata
##' 
##' @param domain A character value specifying the domain for which the code list is
##' required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param blockMetadata An object of type BlockMetadata which should be saved
##' back to the user's session.
##' 
##' @export SaveBlockMetadata

SaveBlockMetadata <- function(domain, dataset, blockMetadata) {

	# Validate passed arguments.
	#
	SaveBlockMetadata.validate(domain, dataset, blockMetadata)

	# Prepare JSON for REST call.
	#
	json <- SaveBlockMetadata.buildJSON(domain, dataset, blockMetadata)

	# Perform REST call.
	#
	url <- paste0(
		swsContext.baseRestUrl, 
		"/r/blockMetadata/", 
		swsContext.executionId,
		"/",
		domain, 
		"/",
		dataset, 
		"?token=", 
		swsContext.token)
	response <- PutRestCall(url, json)

	# Check result.
	#
	if(!response[["success"]]) {
		msg <- paste("The server REST call reported an error: ", response[["message"]], "[")

		map <- list()
		for(i in response[["details"]]) {
			map[[i$message]] <- ifelse(is.null(map[[i$message]]), 1, map[[i$message]] + 1)
		}

		for(i in names(map)) {
			msg <- paste(msg, paste0(i, " (", map[[i]], ")"), sep = "\n")
		}
		msg <- paste(msg, "]", sep = "\n")
		stop(msg)
	}
}


SaveBlockMetadata.validate <- function(domain, dataset, blockMetadata) {

	# Validate passed domain.
	#
	if(missing(domain)) {
		stop("The domain argument is mandatory.")
	}
	if(class(domain) != "character") {
		stop("The passed domain argument is not a character value.")
	}

	# Validate passed dataset
	#
	if(missing(dataset)) {
		stop("The dataset argument is mandatory.")
	}
	if(class(dataset) != "character") {
		stop("The passed dataset argument is not a character value.")
	}

	# Block metadata argument is mandatory.
	#
	if(missing(blockMetadata)) {
		stop("The blockMetadata argument is mandatory.")
	}

	# Block metadata argument can be an object of BlockMetadataSet class or
	# a list or an array of BlockMetadata objects.
	#
	if(class(blockMetadata) != "BlockMetadataSet") {
		for(i in unlist(blockMetadata)) {
			if(class(i) != "BlockMetadata") {
				stop("The blockMetadata argument must contain a list or a vector of BlockMetadata objects")
			}
		}
	}
}


SaveBlockMetadata.buildJSON <- function(domain, dataset, blockMetadata) {

	if(class(blockMetadata) == "BlockMetadataSet") {
		bmd <- unlist(blockMetadata@blockMetadata)
	} else {
		bmd <- unlist(blockMetadata)
	}


	json <- list()

	json[["blockMetadata"]] <- lapply(bmd, function(x) {
		a <- list()
		if(!is.null(x@blockId) && length(x@blockId) > 0) {
			a[["blockId"]] <- x@blockId
		}

		b <- list()
		for(i in x@selection) {
			b[[i@name]] <- I(i@keys)
		}
		a[["selection"]] <- b

		c <- list()
		c[["typeCode"]] <- x@metadata@code
		c[["typeDescription"]] <- x@metadata@description
		c[["language"]] <- x@metadata@language
		c[["elements"]] <- lapply(x@metadata@elements, function(y) {
			d <- list()
			d[["typeCode"]] <- y@code
			d[["typeDescription"]] <- y@description
			d[["value"]] <- y@value
			d
		})
		a[["metadata"]] <- c

		a
	})

	json
}
