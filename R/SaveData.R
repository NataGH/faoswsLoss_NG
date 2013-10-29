SaveData <- function(domain, dataset, data, metadata, normalized = TRUE) {

	# Validate passed arguments.
	#
	SaveData.validate(domain, dataset, data, metadata, normalized)

	# Prepare JSON for REST call.
	#
	json <- SaveData.buildJSON(domain, dataset, data, metadata, normalized)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.id, domain, dataset, "?token=", swsContext.token, "?normalized=", normalized) 
	PutRestCall(url, json)
}


GetData.validate <- function(domain, dataset, data, metadata, normalized) {
}


SaveData.buildJSON <- function(domain, dataset, data, metadata, normalized) {

	json <- list()

	if(!missing(metadata)) {
		json[["metadata"]] <- SaveData.buildMetadataJSON(metadata)
	}

	json
}


SaveData.buildMetadataJSON <- function(metadata) {

	# Save the original key of the passed data table.
	#
	origKey <- key(metadata)

	# Extract key column names.
	#
	if(length(which(colnames(metadata) == "Metadata")) <= 0) {
		stop("Unexpected data table structure detected: could not locate Metadata column.")
	}
	index <- tail(which(colnames(metadata) == "Metadata"), 1)
	if(index <= 0) {
		stop("Unexpected data table structure detected: Metadata column located before any key column")
	}
	keys <- colnames(metadata)[1:index - 1]

	# Set data table key.
	#
	setkeyv(metadata, keys, verbose = FALSE)


	# Prepare list to hold JSON data.
	#
	json <- list()

	# Extract the set of unique keys for external loop.
	#
	uniqueKeys <- unique(metadata)[,keys, with = FALSE]
	for(i in 1:nrow(uniqueKeys)) {

		jsonElement <- list()
		jsonElement[["keys"]] <- as.character(uniqueKeys[i])
		jsonElement[["metadata"]] <- list()

		slicedByKey <- metadata[uniqueKeys[i]]
		setkeyv(slicedByKey, c("Metadata_Group", "Metadata_Language", "Metadata"))
		uniqueMetadata <- unique(slicedByKey)[, c("Metadata", "Metadata_Language", "Metadata_Group"), with = FALSE]

		for(j in 1:nrow(uniqueMetadata)) {

			jsonElement[["metadata"]][[j]] <- list()
			jsonElement[["metadata"]][[j]][["typeCode"]] <- uniqueMetadata[j, Metadata]
			jsonElement[["metadata"]][[j]][["language"]] <- uniqueMetadata[j, Metadata_Language]
			jsonElement[["metadata"]][[j]][["elements"]] <- list()

			metadataElements <- slicedByKey[uniqueMetadata[j]]
			for(k in 1:nrow(metadataElements)) {
				jsonElement[["metadata"]][[j]][["elements"]][[k]] <- list()
				jsonElement[["metadata"]][[j]][["elements"]][[k]][["typeCode"]] <- metadataElements[k, Metadata_Element]
				jsonElement[["metadata"]][[j]][["elements"]][[k]][["value"]] <- metadataElements[k, Metadata_Value]
			}
		}

		json[[i]] <- jsonElement
	}

	# Restore original key.
	#
	setkeyv(metadata, origKey, verbose = FALSE)

	json
}
