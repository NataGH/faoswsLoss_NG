##' Get Data
##' 
##' This function provides an interface between an R session and the database.
##' Note that swsContext files must exist in your session, so you should run
##' GetTestEnvironment before calling this function.
##' 
##' If the denormalized value is not set then the data is normalized; if set the
##' data is denormalized along the axis specified.
##' 
##' If the pivoting vector is present, the dimensions are extracted in the
##' specified order and applying the requested sort direction. This affects
##' both normalized and denormalized extractions. For normalized extractions
##' only the order of the dimensions is influenced, while for denormalized
##' extraction the effect is more evident since the last dimension specified in
##' the pivoting vector gets its values developed along the column of the
##' generated data.table result object.
##' 
##' @param key An object of class DatasetKey.  Often, this will be one of the
##' list elements of swsContext.datasets (if running in a debug/local session,
##' create this object with GetTestEnvironment).
##' @param flags Logical, indicating if flags should be returned (TRUE)
##' otherwise not returned (FALSE).
##' @param normalized Logical, if true then data are returned in normalized
##' format, otherwise the format is denormalized.
##' @param metadata Logical, if true then metadata are returned otherwise not.
##' @param pivoting A vector, each of whose elements must be an object of type
##' Pivoting.  If omitted, no pivoting is performed on the dataset.  Using this
##' argument can allow for convenient reshaping of the data prior to pulling it
##' into R.  Note: if this argument is included, then all of the dimensions in
##' key must be included in this vector.  See ?Pivoting for a description on
##' creating this argument and for some examples on how to use it.
##' 
##' @return A data table containing the data matching the key (may be empty).
##' 
##' @examples
##' \dontrun{
##' # swsContext files are necessary for GetData to run (token may need to be updated)
##' GetTestEnvironment(
##'    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##'    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##' )
##' 
##' # Use GetCodeList to find all countries and commodities
##' areaCodes = GetCodeList("agriculture", "agriculture", "geographicAreaM49")
##' itemCodes = GetCodeList("agriculture", "agriculture", "measuredItemCPC")
##' 
##' # Pull data for one country and all commodities
##' dim1 = Dimension(name = "geographicAreaM49", keys = "12")
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = itemCodes[, code])
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetData(key)
##' 
##' # Pull data for all countries and one commodity
##' dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[, code])
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetData(key)
##' }
##' 

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
	if(normalized){
	  query <- GetData.processNormalizedResult(data, metadata)
	} else {
	  query <- GetData.processDenormalizedResult(data)
	}
	
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
	
	# Validate pivoting, if present.
	#
	if(!missing(pivoting)) {
		if(!is.list(pivoting))
            stop("The pivoting argument must be a list of Pivoting objects.")
		for(p in pivoting) {
			if(class(p) != "Pivoting") {
				stop("At least one of the objects in the list passed for the pivoting argument is not an instance of the Pivoting class.")
			}
			if(!validObject(p)) {
				stop("At least one of the objects in the list passed for the pivoting argument is not valid.")
			}
		}
        dimensionNames = sapply(key@dimensions, slot, "name")
        pivotNames = sapply(pivoting, slot, "code")
        if(!setequal(pivotNames, dimensionNames))
            stop("pivoting must contain all the same elements as dimensions (specified in key), and no more.")
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
	denormalizedDimension <- data$columnKey$definition[["code"]]
	i <- 0
	for(col in data$columnKey$codes) {
		i <- i + 1
		columns[[paste0("Value_", denormalizedDimension, "_", col)]] <- sapply(data$data, function(x) { 
			y <- x[["content"]][[i]][["value"]] 
			ifelse(is.null(y), NA, y)
		})

		# Extract flag columns.
		#
		j <- 0
		for(flag in data$flagDefinitions) {
			j <- j + 1
			columns[[paste0(flag["code"], "_", denormalizedDimension, "_", col)]] <- sapply(data$data, function(x) { x[["content"]][[i]][["flags"]][j] })
		}
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
