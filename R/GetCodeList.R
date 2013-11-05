GetCodeList <- function(domain, dataset, dimension, codes) {

	# Validate passed arguments.
	#
	GetCodeList.validate(domain, dataset, dimension, codes)

	# Prepare JSON for REST call.
	#
	json <- GetCodeList.buildJSON(domain, dataset, dimension, codes)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/dimensionValues/", domain, "/", dataset, "/", dimension, "/plain?plainDescription=true")
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	GetCodeList.processResult(data)
}


GetCodeList.validate <- function(domain, dataset, dimension, codes) {

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

	# Validate passed dimension
	#
	if(missing(dimension)) {
		stop("The dimension argument is mandatory.")
	}
	if(class(dimension) != "character") {
		stop("The passed dimension argument is not a character value.")
	}
}


GetCodeList.buildJSON <- function(domain, dataset, dimension, codes) {
	
	# Build JSON for REST call.
	#
	json <- list()

	# Add the codes if they have been specified otherwise simply add a null field.
	#
	if(missing(codes)) {
		json[["codes"]] <- NA
	} else {
		json[["codes"]] <- I(codes)
	}

	json
}


GetCodeList.processResult <- function(data) {

	columns <- list()

	# Extract codes column.
	#
	columns[["code"]] <- sapply(data$result, function(x) { x[["code"]] })

	# Extract description column.
	#
	columns[["description"]] <- sapply(data$result, function(x) { x[["description"]] })

	# Extract selection-only flag column.
	#
	columns[["selectionOnly"]] <- sapply(data$result, function(x) { x[["subtreeSelectionOnly"]] })

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
