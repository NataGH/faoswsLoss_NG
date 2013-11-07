GetData <- function(key, flags = TRUE, normalized = TRUE, metadata = FALSE, pivoting) {

	# Validate passed arguments.
	#
	GetData.validate(key, flags, normalized, metadata, pivoting)

	# Prepare JSON for REST call.
	#
	json <- GetData.buildJSON(key, flags, normalized, metadata, pivoting)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.executionId) 
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	if(normalized) {
		GetData.processNormalizedResult(data, metadata)
	} else {
		GetData.processDenormalizedResult(data)
	}
}


GetData.validate <- function(key, flags, normalized, metadata, pivoting) {

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

	# Validate pivoting, if present.
	#
	if(!missing(pivoting)) {
		if(is.list(pivoting)) {
			for(p in pivoting) {
				if(class(p) != "Pivoting") {
					stop("At least one of the objects in the list passed for the pivoting argument is not an instance of the Pivoting class.")
				}
				if(!validObject(p)) {
					stop("At least one of the objects in the list passed for the pivoting argument is not valid.")
				}
			}
		} else {
			if(class(pivoting) != "Pivoting") {
				stop("The pivoting parameter passed is not an instance of the Pivoting class.")
			}
			if(!validObject(pivoting)) {
				stop("The pivoting parameter passed is not valid.")
			}
		}
	}

	# Denormalized format with metadata is not supported.
	#
	if(!normalized && metadata) {
		stop("Denormalized data format with metadata is not supported.")
	}
}


GetData.buildJSON <- function(key, flags, normalized, metadata, pivoting) {
	
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

	# Add pivoting parameters, if requested.
	#
	if(!missing(pivoting) && !is.na(pivoting) && length(pivoting) > 0) {
		json[["pivotingDimensions"]] <- pivoting
	}

	# Add parameter controlling the inclusion of flags.
	#
	json[["includeFlags"]] <- flags

	# Add parameter controlling the inclusion of metadata.
	#
	json[["includeMetadata"]] <- metadata

	# Add parameter used to request normalized or denormalized data.
	#
	json[["denormalized"]] <- !normalized

	json
}


GetData.processNormalizedResult <- function(data, metadata) {

	columns <- list()

	# Extract key columns.
	#
	i <- 0
	for(col in data$keyDefinitions) {
		i <- i + 1
		column <- unlist(sapply(data$data, function(x) { 

			if(length(x[["metadata"]]) > 0) {
				
				len <- 0
				for(metadata in x[["metadata"]]) {
					len <- len + length(metadata$elements)
				}
				rep(x[["keys"]][i], len)

			} else {

				x[["keys"]][i] 

			}
		}))
		dim(column) <- NULL
		columns[[col["code"]]] <- column
	}

	# Extract metadata if needed.
	#
	if(metadata) {
		column <- unlist(sapply(data$data, function(x) { 

				if(length(x[["metadata"]]) > 0) {
					
					tmp <- NULL
					for(metadata in x[["metadata"]]) {
						tmp <- c(tmp, rep(metadata$typeCode, length(metadata$elements)))
					}
					tmp

				} else {

					NA

				}
		}))
		dim(column) <- NULL
		columns[["Metadata"]] <- column

		column <- unlist(sapply(data$data, function(x) { 

				if(length(x[["metadata"]]) > 0) {
					
					tmp <- NULL
					for(metadata in x[["metadata"]]) {
						tmp <- c(tmp, rep(metadata$language, length(metadata$elements)))
					}
					tmp

				} else {

					NA

				}
		}))
		dim(column) <- NULL
		columns[["Metadata_Language"]] <- column

		column <- unlist(sapply(data$data, function(x) { 

				if(length(x[["metadata"]]) > 0) {
					
					tmp <- NULL
					group <- 0
					for(metadata in x[["metadata"]]) {
						group <- group + 1
						tmp <- c(tmp, rep(group, length(metadata$elements)))
					}
					tmp

				} else {

					NA

				}
		}))
		dim(column) <- NULL
		columns[["Metadata_Group"]] <- column

		column <- unlist(sapply(data$data, function(x) { 

				if(length(x[["metadata"]]) > 0) {
					
					tmp <- NULL
					for(metadata in x[["metadata"]]) {
						tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
							y[["typeCode"]]
						})))
					}
					tmp

				} else {

					NA

				}
		}))
		dim(column) <- NULL
		columns[["Metadata_Element"]] <- column

		column <- unlist(sapply(data$data, function(x) { 

				if(length(x[["metadata"]]) > 0) {
					
					tmp <- NULL
					for(metadata in x[["metadata"]]) {
						tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
							y[["value"]]
						})))
					}
					tmp

				} else {

					NA

				}
		}))
		dim(column) <- NULL
		columns[["Metadata_Value"]] <- column
	}

	# Extract value column.
	#
	column <- unlist(sapply(data$data, function(x) { 

			tmp <- ifelse(is.null(x[["value"]]), NA, x[["value"]])

			if(length(x[["metadata"]]) > 0) {
				
				len <- 0
				for(metadata in x[["metadata"]]) {
					len <- len + length(metadata$elements)
				}
				rep(tmp, len)

			} else {

				tmp

			}
	}))
	dim(column) <- NULL
	columns[["Value"]] <- column

	# Extract flag columns.
	#
	i <- 0
	for(col in data$flagDefinitions) {
		i <- i + 1
		column <- unlist(sapply(data$data, function(x) 
		{ 

			if(length(x[["metadata"]]) > 0) {
				
				len <- 0
				for(metadata in x[["metadata"]]) {
					len <- len + length(metadata$elements)
				}
				rep(x[["flags"]][i], len)

			} else {

				x[["flags"]][i] 

			}
		}))
		dim(column) <- NULL
		columns[[col["code"]]] <- column
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}


GetData.processDenormalizedResult <- function(data) {

	columns <- list()

	# Extract grouping key columns.
	#
	i <- 0
	for(col in data$groupingKeyDefinitions) {
		i <- i + 1
		columns[[col["code"]]] <- sapply(data$data, function(x) { x[["groupingKeys"]][i] })
	}

	# Extract denormalized column keys.
	#
	i <- 0
	for(col in data$columnKey$codes) {
		i <- i + 1
		columns[[paste0("Value_", col)]] <- sapply(data$data, function(x) { x[["content"]][[i]][["value"]] })

		# Extract flag columns.
		#
		j <- 0
		for(flag in data$flagDefinitions) {
			j <- j + 1
			columns[[paste0(flag["code"], "_", col)]] <- sapply(data$data, function(x) { x[["content"]][[i]][["flags"]][j] })
		}
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
