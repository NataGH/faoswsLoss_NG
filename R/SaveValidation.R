##' Save Validation
##' 
##' @param domain A character value specifying the domain for which the code
##' list is required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param validation A data.table object containing keys and validation data.
##' 
##' @return The data passed to this function is saved to the database, and
##' nothing is explicitly returned by this function.
##' 

SaveValidation <- function(domain, dataset, validation) {

	# Validate passed arguments.
	#
	SaveValidation.validate(domain, dataset, validation)

	# Prepare JSON for REST call.
	#
	json <- SaveValidation.buildJSON.new(domain, dataset, validation)
	
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
    
    expectedColumnNames = c(names(swsContext.datasets[[1]]@dimensions),
                            "Severity", "Description")
    missingColumns = !expectedColumnNames %in% colnames(validation)
    if(any(missingColumns))
        warning("Based on swsContext.datasets[[1]], we expect the following ",
                "columns which aren't in the validation dataset:\n",
                paste(expectedColumnNames[missingColumns], collapse = "\n"))
    additionalColumns = !colnames(validation) %in% expectedColumnNames
    if(any(additionalColumns))
        warning("Based on swsContext.datasets[[1]], we don't expect the ",
                "following columns which are in the validation dataset:\n",
                paste(colnames(validation)[additionalColumns], collapse = "\n"))
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

	# Extract data from passed validation table rows using apply.
	#
    json = c(list(jsonElement), apply(data.frame(validation), 1, function(x){
        currentKeys = x[keys]
        names(currentKeys) = NULL
        list(keys = currentKeys,
             severity = as.numeric(x["Severity"]),
             description = as.character(x["Description"]))
        }))

    ## Coerce to a list of lists to match previous format requirements
	list(validations = json)
}
