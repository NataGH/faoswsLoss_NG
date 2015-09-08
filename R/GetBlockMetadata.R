##' Get Block Metadata
##' 
##' @param key A key object, as contained in a BlockMetadata object.
##' 
##' @export GetBlockMetadata

GetBlockMetadata <- function(key) {

	# Validate passed arguments.
	#
	GetBlockMetadata.validate(key)

	# Prepare JSON for REST call.
	#
	json <- GetBlockMetadata.buildJSON(key)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/blockMetadata/", swsContext.executionId)
	data <- PostRestCall(url, json)

	# Create result data structure.
	#
	GetBlockMetadata.processResult(data)
}


GetBlockMetadata.validate <- function(key) {

	# Validate passed key.
	#
	if(missing(key)) {
		stop("The key argument is mandatory.")
	}
	if(class(key) != "DatasetKey") {
		stop("The passed key argument is not an instance of the DatasetKey class.")
	}
	if(!validObject(key)) {
		stop("The passed key argument is not valid.")
	}

	# Validate that at least one key per dimension has been specified.
	#
	for(d in key@dimensions) {
		if(is.null(d@keys)) {
			stop(paste("The passed dimension", d@name, "has a null key array. It is necessary to specify at least one key for every dimension of the target dataset."))
		}
		if(length(d@keys) == 0) {
			stop(paste("The passed dimension", d@name, "has an empty key array. It is necessary to specify at least one key for every dimension of the target dataset."))
		}
	}
}


GetBlockMetadata.buildJSON <- function(key) {
	
	# Build JSON for REST call.
	#
	json <- list(
		token = swsContext.token,
		domain = key@domain,
		dataSet = key@dataset)

	# Set up dimensions and selected keys.
	#
	json[["dimension2codes"]] <- list()
	for(d in key@dimensions) {
		json[["dimension2codes"]][[d@name]] <- I(d@keys)
	}

	json
}


GetBlockMetadata.processResult <- function(data) {

	keyDefinitions <- sapply(data$keyDefinitions, function(x) {
		KeyDefinition(code = x[["code"]], description = x[["description"]], type = x[["type"]])
	})

	blockMetadata <- sapply(data$blockMetadata, function(x) {
		selection <- sapply(names(x$selection), function(y) {
			Dimension(name = y, keys = x$selection[[y]])
		})

		metadata <- Metadata(
			code = x$metadata$typeCode, 
			description = x$metadata$typeDescription,
			language = x$metadata$language, 
			elements = sapply(x$metadata$elements, function(y) {
				MetadataElement(code = y[["typeCode"]], description = y[["typeDescription"]], value = y[["value"]])
			})
		)

		BlockMetadata(blockId = x[["blockId"]], selection = selection, metadata = metadata)
	})

	BlockMetadataSet(keyDefinitions = keyDefinitions, blockMetadata = blockMetadata)
}
