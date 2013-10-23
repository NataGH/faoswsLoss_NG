GetData <- function(keyset, flags = TRUE, pivoting) {

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
	json[["includeFlags"]] = flags


	# Perform REST call.
	#
	url <- paste0("https://swsrm:8181/sws/rest/r/data/", swsContext.id) 
	data <- PostRestCall(url, json)
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
