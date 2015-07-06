##' Get Domain Names
##' 
##' Invokes a SWS RESTful GET to query for the list of all datasets in a domain
##' Note: if the user of the request is not allowed for any of the datasets in a domain, 
##' this domain will not be included in the resulting list
##' 
##' @return A list of domain names
##' 

GetDomainNames <- function() {

	url <- paste0(swsContext.baseRestUrl, "/r/configuration/", swsContext.executionId, '?reqToken=', swsContext.token) 

  jsonOut <- GetRestCall(url)
	
	if (!jsonOut[["success"]]) {

    stop(paste("An error occurred: ", jsonOut[["message"]]))
	
	} else {
	
	  jsonOut[["results"]]
    
	}
}
