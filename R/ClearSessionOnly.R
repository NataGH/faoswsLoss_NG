#' Clear session of session-only data
#' 
#' @export

ClearSessionOnly <-  function(domain, dataset){
  
  url <- paste0(swsContext.baseRestUrl, "/r/data/session_only/", swsContext.executionId)
  
  json <- list(token = swsContext.token,
               domain = domain,
               dataSet = dataset)
  
  PostRestCall(url, json)
}