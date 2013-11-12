SaveValidation <- function(domain, dataset, validation) {

	# Validate passed arguments.
	#
	SaveValidation.validate(domain, dataset, validation)

	# Prepare JSON for REST call.
	#
	json <- SaveValidation.buildJSON(domain, dataset, validation)
	cat(toJSON(json))

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

	# The validation object is mandatory.
	#
	if(missing(validation)) {
		stop("No validation argument has been specified.")
	}
	if(!is.data.table(validation)) {
		stop("The passed validation argument is not a data table.")
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

	# Iterate on passed validation table rows.
	#
	for(i in 1:nrow(validation)) {

		jsonElement <- list()
		jsonElement[["keys"]] <- as.character(validation[i, keys, with = FALSE])
		jsonElement[["severity"]] <- validation[i, Severity]
		jsonElement[["description"]] <- validation[i, Description]

		json[["validations"]][[i]] <- jsonElement
	}

	json
}
