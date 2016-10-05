##' Get Code List
##' 
##' This function returns information about certain codes for a dimension.  For
##' example, one may wish to check if a particular country has a startDate or
##' endDate in the system before attempting to write a particular value.  See
##' example below for a query that pulls the startDates for three countries.
##' Note that Belarus(1992-) is not defined prior to 1992.
##' 
##' @param domain A character value specifying the domain for which the code
##' list is required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param dimension A character value specifying the name of the key for which
##' the code list is required.
##' @param codes (optional) A list of codes for which the key data is required.
##' 
##' @return A data table containing the key codes and matching labels. Will have
##'   the following columns:
##' \itemize{
##' \item code - Alphanumeric code in SWS
##' \item description - Human-readable description
##' \item selectionOnly - Boolean stating whether this item is a hierarchical
##' category or a bottom level one containing data
##' \item type - Depends on dimension
##' \item startDate - Mainly for countries, start date for the country's existence
##' \item endDate - As for startData, end date of existence
##' }
##' 
##' @examples
##' \dontrun{
##' GetCodeList(domain = "agriculture", dataset = "aproduction",
##'             dimension = "geographicAreaM49", codes = c("4", "8", "112"))
##' }
##' 
##' @import data.table
##' @export GetCodeList

GetCodeList <- function(domain, dataset, dimension, codes) {

	# Validate passed arguments.
	#
	GetCodeList.validate(domain, dataset, dimension, codes)

  if(!missing(codes) && !is.null(codes) && length(codes) == 0){
    # If request is *actually* length 0 then no need to make a request
    
    data.table(code = character(),
               description = character(),
               selectionOnly = logical(),
               type = character(),
               startDate = character(),
               endDate = character())
  } else {

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
  if(length(domain) != 1L){
    stop("Only one domain is allowed")
  }

	# Validate passed dataset
	#
	if(missing(dataset)) {
		stop("The dataset argument is mandatory.")
	}
	if(class(dataset) != "character") {
		stop("The passed dataset argument is not a character value.")
	}
  if(length(dataset) != 1L){
    stop("Only one dataset is allowed")
  }

	# Validate passed dimension
	#
	if(missing(dimension)) {
		stop("The dimension argument is mandatory.")
	}
	if(class(dimension) != "character") {
		stop("The passed dimension argument is not a character value.")
	}
  if(!missing(dimension) && length(dimension) != 1L){
    stop("Only one dimension at a time.")
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
	columns[["code"]] <- NullToNa(sapply(data$result, function(x) { x[["code"]] }))

	# Extract description column.
	#
	columns[["description"]] <- NullToNa(sapply(data$result, function(x) { x[["description"]] }))

	# Extract selection-only flag column.
	#
	columns[["selectionOnly"]] <- NullToNa(sapply(data$result, function(x) { x[["subtreeSelectionOnly"]] }))

	# Extract type column.
	#
	columns[["type"]] <- NullToNa(sapply(data$result, function(x) { x[["type"]] }))

	# Extract start date column.
	#
	columns[["startDate"]] <- NullToNa(sapply(data$result, function(x) { x[["startDate"]] }))

	# Extract end date column.
	#
	columns[["endDate"]] <- NullToNa(sapply(data$result, function(x) { x[["endDate"]] }))

	# Bind columns into a data table object.
	#
	do.call("data.table", columns)
}
