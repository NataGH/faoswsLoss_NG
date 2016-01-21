##' Get Mapping
##' 
##' This is expected to be used only by advanced script writers. The key will
##' be a custom key layout matching the axes of the mapping table.
##' 
##' The key may make use of wildcards to widen the number of results matched.
##' 
##' Map tables are always read-only
##' 
##' @param key A MappingTableKey class object.
##' 
##' @return A data table containing the map entries matching the key (may be
##' empty).
##' 
##' @export GetMapping

GetMapping <- function(key) {

	# Validate passed arguments.
	#
	GetMapping.validate(key)

	# Prepare JSON for REST call.
	#
	json <- GetMapping.buildJSON(key)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/conversionTables/", swsContext.executionId) 
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	GetMapping.processResult(data)
}


GetMapping.validate <- function(key) {

	# Validate passed key.
	#
	if(missing(key)) {
		stop("The key argument is mandatory.")
	}
	if(class(key) != "MappingTableKey") {
		stop("The passed key argument is not an instance of the MappingTableKey class.")
	}
	if(!validObject(key)) {
		stop("The passed key argument is not valid.")
	}
}


GetMapping.buildJSON <- function(key) {
	
	# Build JSON for REST call.
	#
	json <- list(
		token = swsContext.token,
		mappingTableName = key@mappingTable)

	# Set up dimensions and selected keys.
	#
	if(!is.null(key@dimensions) & length(key@dimensions) > 0) {
		json[["dimension2codes"]] <- list()
		for(d in key@dimensions) {
			json[["dimension2codes"]][[d@name]] <- I(d@keys)
		}
	}

	json
}


GetMapping.processResult <- function(data) {

	columns <- list()

	# Extract source dimension column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		x$source

	}))
	dim(column) <- NULL
	columns[[data$sourceDimension]] <- column

	# Extract target dimension column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		x$target

	}))
	dim(column) <- NULL
	columns[[data$targetDimension]] <- column


	# Extract filter columns.
	#
	i <- 0
	for(col in data$filterDimensions) {
		i <- i + 1
		column <- unlist(sapply(data$data, function(x) { 

			x[["filters"]][i]

		}))
		dim(column) <- NULL
		columns[[col]] <- column
	}


	# Extract split factor column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		ifelse(is.null(x$splitFactor), NA, x$splitFactor)

	}))
	dim(column) <- NULL
	columns[["SplitFactor"]] <- column


	# Extract conversion factor column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		ifelse(is.null(x$conversionFactor), NA, x$conversionFactor)

	}))
	dim(column) <- NULL
	columns[["ConversionFactor"]] <- column


	# Extract comment column.
	#
	column <- unlist(sapply(data$data, function(x) { 

		ifelse(is.null(x$comment), NA, x$comment)

	}))
	dim(column) <- NULL
	columns[["Comment"]] <- column

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
