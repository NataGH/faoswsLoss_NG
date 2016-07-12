##' Get History
##' 
##' @param key An object of class DatasetKey.  Often, this will be one of the
##' list elements of swsContext.datasets (if running in a debug/local session,
##' create this object with GetTestEnvironment).
##' @param pivoting A vector, each of whose elements must be an object of type
##' Pivoting.  If omitted, no pivoting is performed on the dataset.  Using this
##' argument can allow for convenient reshaping of the data prior to pulling it
##' into R.
##' 
##' @return A data table of observation objects containing the values and flags
##' through history with the associated history metadata.
##' 
##' @export GetHistory

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
	query <- GetHistory.NEW_processNormalizedResult(data)
	
	# normalizes result transforming columns from list of NULLs to vector of NAs
	as.data.table(
	  lapply(query,
            FUN = function(x){
              if(is.list(x))
                x = NullToNa(x)
              x
            }
	  )
	)
	
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

GetHistory.NEW_processNormalizedResult <- function(data) {

	keyNames <- sapply(data$keyDefinitions, function(x) x[1])
	flagNames <- sapply(data$flagDefinitions, function(x) x[1])

	cols <- c(keyNames, "Value", "Version", "StartDate", "EndDate", flagNames, "Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value")
	offset <- length(keyNames) + 4 + length(flagNames)

	result = lapply(data$data, function(listElement) {
		currList <- listElement[1:offset]
		currMeta <- list()
		if (length(listElement[[offset+1]]) > 0) {
			currMeta <- listElement[[offset+1]]
		}
		currList[[11]] <- currMeta
		currList <- list(currList)
		totalList <- NULL
		if (length(listElement[[offset+2]]) > 0) {
			histList = lapply(listElement[[offset+2]], function(listElement) {
				out <- append(currList[[1]][1:length(keyNames)], listElement)
				return(out)
			})
			totalList <- append(currList, histList)
		} else {
			totalList <- currList
		}
		out = lapply(totalList, function(listElement) {
			out = NULL
			elem <- listElement[1:offset]
			lapply(1:offset, function(i) {
				if (is.null(elem[[i]])) {
					elem[[i]] <<- NA
				} 
			})
			elem <- list(elem)
			# Check if there is metadata
			if (length(listElement) >= (offset + 1) && length(listElement[[offset + 1]]) > 0) {
			## alternatively: 
			## if (length(listElement) > offset && length(listElement[[offset+1]]) > 0) {
			    meta1 = lapply(listElement[[offset+1]], function(listElement) {
					meta2 = lapply(listElement[[4]], function(listElement) {
						out = data.frame(list(0, listElement[[1]], listElement[[3]]))
						colnames(out) = c(cols[(offset+3):length(cols)])
						return(out)
					})
					lapply(1:length(meta2), function(i) {
						meta2[[i]]$Metadata_Group <<- i
					})
					out = data.frame(list(listElement[[1]], listElement[[3]]))
					out = merge(out, do.call(rbind, meta2))
					colnames(out) = c(cols[(offset+1):length(cols)])
					return(out)
				})
				out = data.frame(elem)
				out = merge(out, do.call(rbind, meta1))
			} else {
				lapply(1:5, function(i) {
					elem[[1]][[offset+i]] <<- NA
				})
				out = data.frame(elem)
			}
			colnames(out) = cols
			return(out)
		})
		out = do.call("rbind", out)
		colnames(out) = cols
		return(out)
	})
	result = do.call("rbind", result)
	result = data.table(result)
  # If empty, don't break when trying to set col order
	if(nrow(result) == 0){
	  result <- as.data.table(setNames(replicate(length(cols),logical()), cols))
	}
	setcolorder(result, c(keyNames, "Version", "StartDate", "EndDate", "Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value", "Value", flagNames))
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
