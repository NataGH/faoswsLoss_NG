##' Get Dataset Config
##' 
##' Invokes a SWS RESTful GET to query for the details of a given dataset configuration
##' Note: if the user of the request is not allowed for the dataset, this will not be
##' included in the resulting list
##' 
##' @param domainCode A string containing the domain code
##' @param datasetCode A string containing the dataset code
##' 
##' @return A object containing the full information
##' 
##' @export GetDatasetConfig

GetDatasetConfig <- function(domainCode, datasetCode) {

	url <- paste0(swsContext.baseRestUrl, "/r/configuration/", swsContext.executionId, "/", domainCode, "/", datasetCode, '?reqToken=', swsContext.token) 

  warning(  url)
  
  jsonOut <- GetRestCall(url)
	
	if (!jsonOut[["success"]]) {

    stop(paste("An error occurred: ", jsonOut[["result"]]))
	
	} else {
	
	  jsonOut[["result"]]
    
	}
}
