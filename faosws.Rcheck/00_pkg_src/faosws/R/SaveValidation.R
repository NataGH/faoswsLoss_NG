##' Save Validation
##' 
##' @param domain A character value specifying the domain for which the code
##' list is required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param validation A data.table object containing keys and validation data.
##' 

SaveValidation <- function(domain, dataset, validation) {

	# Validate passed arguments.
	#
	SaveValidation.validate(domain, dataset, validation)

	# Prepare JSON for REST call.
	#
	json <- SaveValidation.buildJSON(domain, dataset, validation)

	# Perform REST call.
	#
	url <- paste0(
		swsContext.baseRestUrl, 
		"/r/validations/", 
		swsContext.executionId,
		"/",
		domain, 
		"/",
		dataset, 
		"?token=", swsContext.token)
	PutRestCall(url, json)
}


SaveValidation.validate <- function(domain, dataset, validation) {

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

	# The validation object is mandatory and cannot be empty.
	#
	if(missing(validation)) {
		stop("No validation argument has been specified.")
	}
	if(!is.data.table(validation)) {
		stop("The passed validation argument is not a data table.")
	}
	if(nrow(validation) == 0) {
		stop("The passed validation argument was empty.")
	}
}


SaveValidation.buildJSON <- function(domain, dataset, validation) {

	# Prepare list to hold JSON data.
	#
	json <- list()
	json[["validations"]] <- list()

	# Extract key column names. We assume that only the Severity and the Description
	# extra columns are present in the data.table, apart the key columns.
	#
	keyColumnsFilter <- colnames(validation) != "Severity"
	keyColumnsFilter <- keyColumnsFilter & colnames(validation) != "Description"
	keys <- colnames(validation[, keyColumnsFilter, with = FALSE])

	# Declare dimension in first element of JSON.
	#
	jsonElement <- list()
	jsonElement[["dimensions"]] <- keys
	json[["validations"]][[1]] <- jsonElement


	# Iterate on passed validation table rows.
	#
	for(i in 1:nrow(validation)) {

		k <- c()
		for(j in 1:length(keys)) {
			k <- c(k, as.character(validation[i, keys, with = FALSE][[j]]))
		}

		jsonElement <- list()
		jsonElement[["keys"]] <- k
		jsonElement[["severity"]] <- validation[i, Severity]
		jsonElement[["description"]] <- validation[i, Description]

		json[["validations"]][[i + 1]] <- jsonElement
	}

	json
}
