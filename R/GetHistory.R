GetHistory <- function(key, pivoting) {

	# Validate passed arguments.
	#
	GetHistory.validate(key, pivoting)

	# Prepare JSON for REST call.
	#
	json <- GetHistory.buildJSON(key, pivoting)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.executionId) 
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	GetHistory.processNormalizedResult(data)
}


GetHistory.validate <- function(key, pivoting) {

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
}


GetHistory.buildJSON <- function(key, pivoting) {
	
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

	# Add parameter controlling the inclusion of flags (always true in
	# GetHistory call).
	#
	json[["includeFlags"]] <- TRUE

	# Add parameter controlling the inclusion of metadata (always true
	# in GetHistory call).
	#
	json[["includeMetadata"]] <- TRUE

	# Request historical data.
	#
	json[["includeHistory"]] <- TRUE

	# Add parameter to force normalized data generation.
	#
	json[["denormalized"]] <- FALSE

	json
}


GetHistory.processNormalizedResult <- function(data) {

	columns <- list()

	# Extract key columns.
	#
	i <- 0
	for(col in data$keyDefinitions) {
		i <- i + 1
		column <- unlist(sapply(data$data, function(x) { 

			key <- x[["keys"]][[i]]

			tmp <- NULL

			if(length(x[["metadata"]]) > 0) {
				for(metadata in x[["metadata"]]) {
					tmp <- c(tmp, rep(key, length(metadata$elements)))
				}
			} else {
				tmp <- key
			}

			for(history in x[["history"]]) {

				if(length(history[["metadata"]]) > 0) {
					for(metadata in history[["metadata"]]) {
						tmp <- c(tmp, rep(key, length(metadata$elements)))
					}
				} else {
					tmp <- c(tmp, key)
				}
			}

			tmp
		}))
		dim(column) <- NULL
		columns[[col["code"]]] <- column
	}


	# Extract version column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(x[["version"]], length(metadata$elements)))
			}
		} else {
			tmp <- c(tmp, x[["version"]])
		}

		for(history in x[["history"]]) {
			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(history[["version"]], length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, history[["version"]])
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Version"]] <- column


	# Extract start date column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		val <- x[["startDate"]] / 1000

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(val, length(metadata$elements)))
			}
		} else {
			tmp <- c(tmp, val)
		}

		for(history in x[["history"]]) {

			val <- history[["startDate"]] / 1000

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(val, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, val)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	class(column) <- "POSIXct"
	columns[["StartDate"]] <- column


	# Extract end date column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		val <- ifelse(is.null(x[["endDate"]]), NA, x[["endDate"]] / 1000)

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(val, length(metadata$elements)))
			}
		} else {
			tmp <- c(tmp, val)
		}

		for(history in x[["history"]]) {

			val <- ifelse(is.null(history[["endDate"]]), NA, history[["endDate"]] / 1000)

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(val, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, val)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	class(column) <- "POSIXct"
	columns[["EndDate"]] <- column


	# Extract metadata column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(metadata$typeCode, length(metadata$elements)))
			}
		} else {
			tmp <- NA
		}

		for(history in x[["history"]]) {

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(metadata$typeCode, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, NA)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Metadata"]] <- column


	# Extract metadata language.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(metadata$language, length(metadata$elements)))
			}
		} else {
			tmp <- NA
		}

		for(history in x[["history"]]) {

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(metadata$language, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, NA)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Metadata_Language"]] <- column


	# Extract metadata group.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			group <- 0
			for(metadata in x[["metadata"]]) {
				group <- group + 1
				tmp <- c(tmp, rep(group, length(metadata$elements)))
			}
		} else {
			tmp <- NA
		}

		for(history in x[["history"]]) {

			if(length(history[["metadata"]]) > 0) {
				group <- 0
				for(metadata in history[["metadata"]]) {
					group <- group + 1
					tmp <- c(tmp, rep(group, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, NA)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Metadata_Group"]] <- column


	# Extract metadata element.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
					y[["typeCode"]]
				})))
			}
		} else {
			tmp <- NA
		}

		for(history in x[["history"]]) {

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
						y[["typeCode"]]
					})))
				}
			} else {
				tmp <- c(tmp, NA)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Metadata_Element"]] <- column


	# Extract metadata value.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
					y[["value"]]
				})))
			}
		} else {
			tmp <- NA
		}

		for(history in x[["history"]]) {

			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, unlist(sapply(metadata$elements, function(y) {
						y[["value"]]
					})))
				}
			} else {
				tmp <- c(tmp, NA)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Metadata_Value"]] <- column


	# Extract value column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		tmp <- NULL

		val <- ifelse(is.null(x[["value"]]), NA, x[["value"]])
		if(length(x[["metadata"]]) > 0) {
			for(metadata in x[["metadata"]]) {
				tmp <- c(tmp, rep(val, length(metadata$elements)))
			}
		} else {
			tmp <- c(tmp, val)
		}

		for(history in x[["history"]]) {
			val <- ifelse(is.null(history[["value"]]), NA, history[["value"]])
			if(length(history[["metadata"]]) > 0) {
				for(metadata in history[["metadata"]]) {
					tmp <- c(tmp, rep(val, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, val)
			}
		}

		tmp
	}))
	dim(column) <- NULL
	columns[["Value"]] <- column

	# Extract flag columns.
	#
	i <- 0
	for(col in data$flagDefinitions) {
		i <- i + 1
		column <- unlist(sapply(data$data, function(x) { 

			tmp <- NULL

			val <- x[["flags"]][i]
			if(length(x[["metadata"]]) > 0) {
				for(metadata in x[["metadata"]]) {
					tmp <- c(tmp, rep(val, length(metadata$elements)))
				}
			} else {
				tmp <- c(tmp, val)
			}

			for(history in x[["history"]]) {
				val <- history[["flags"]][i]
				if(length(history[["metadata"]]) > 0) {
					for(metadata in history[["metadata"]]) {
						tmp <- c(tmp, rep(val, length(metadata$elements)))
					}
				} else {
					tmp <- c(tmp, val)
				}
			}

			tmp
		}))
		dim(column) <- NULL
		columns[[col["code"]]] <- column
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
