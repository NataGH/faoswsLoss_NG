##' Get Dataset Names
##' 
##' Invokes a SWS RESTful GET to query for the list of all datasets in a domain
##' Note: if the user of the request is not allowed for a dataset, this will not be
##' included in the resulting list
##' 
##' @param domainCode A string containing the domain code
##' 
##' @return A list of dataset names
##' 

GetDatasetNames <- function(domainCode) {

	url <- paste0(swsContext.baseRestUrl, "/r/configuration/", swsContext.executionId, "/", domainCode, '?reqToken=', swsContext.token) 

  jsonOut <- GetRestCall(url)
	
	if (!jsonOut[["success"]]) {

    stop(paste("An error occurred: ", jsonOut[["message"]]))
	
	} else {
	
	  jsonOut[["results"]]
    
	}
}
