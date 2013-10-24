GetData <- function(keyset, flags = TRUE, normalized = TRUE, pivoting) {

	# Validate passed arguments.
	#
	GetData.validate(keyset, flags, normalized, pivoting)

	# Prepare JSON for REST call.
	#
	json <- GetData.buildJSON(keyset, flags, normalized, pivoting)

	# Perform REST call.
	#
	url <- paste0("https://swsrm:8181/sws/rest/r/data/", swsContext.id) 
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	if(normalized) {
		GetData.processNormalizedResult(data)
	} else {
		GetData.processDenormalizedResult(data)
	}
}


GetData.validate <- function(keyset, flags, normalized, pivoting) {

	# Validate passed keyset.
	#
	if(missing(keyset)) {
		stop("The keyset argument is mandatory.")
	}
	if(class(keyset) != "KeySet") {
		stop("The passed keyset argument is not an instance of the KeySet class.")
	}
	if(!validObject(keyset)) {
		stop("The passed keyset argument is not valid.")
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


GetData.buildJSON <- function(keyset, flags, normalized, pivoting) {
	
	# Build JSON for REST call.
	#
	json <- list(
		token = swsContext.token,
		domain = keyset@domain,
		dataSet = keyset@dataset)

	# Set up dimensions and selected keys.
	#
	json[["dimension2codes"]] <- list()
	for(d in keyset@dimensions) {
		json[["dimension2codes"]][[d@name]] <- I(d@keys)
	}

	# Pivoting still missing
	#
	if(!missing(pivoting) && !is.na(pivoting) && length(pivoting) > 0) {
		json[["pivotingDimensions"]] <- pivoting
	}

	# Add parameter controlling the inclusion of flags.
	#
	json[["includeFlags"]] <- flags

	# Add parameter used to request normalized or denormalized data.
	#
	json[["denormalized"]] <- !normalized

	json
}


GetData.processNormalizedResult <- function(data) {

	columns <- list()

	# Extract key columns.
	#
	index <- 0
	for(col in data$keyDefinitions) {
		index <- index + 1
		columns[[col["code"]]] <- sapply(data$data, function(x) { x[["keys"]][index] })
	}

	# Extract value column.
	#
	columns[["value"]] <- sapply(data$data, function(x) { x[["value"]] })

	# Extract flag columns.
	#
	index <- 0
	for(col in data$flagDefinitions) {
		index <- index + 1
		columns[[col["code"]]] <- sapply(data$data, function(x) { x[["flags"]][index] })
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}


GetData.processDenormalizedResult <- function(data) {

	columns <- list()

	# Extract grouping key columns.
	#
	index <- 0
	for(col in data$groupingKeyDefinitions) {
		index <- index + 1
		columns[[col["code"]]] <- sapply(data$data, function(x) { x[["groupingKeys"]][index] })
	}

	# Extract denormalized column keys.
	#
	index <- 0
	for(col in data$columnKey$codes) {
		index <- index + 1
		columns[[paste0("Value_", col)]] <- sapply(data$data, function(x) { x[["content"]][[index]][["value"]] })

		# Need to process flags, if requested.
		#
	}

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
