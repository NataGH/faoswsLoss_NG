#' Clear session of session-only data
#' 
#' In a dataset, certain values can be marked as session-only. These values 
#' cannot be saved to the database and can be cleared with a button press in the
#' UI as well as with this function.
#' 
#' @param domain character. Name of domain (e.g. "agriculture")
#' @param dataset character. Name of dataset (e.g. "aproduction")
#' 
#' @return A list with two elements: 
#' \itemize{ 
#' \item message - A response
#'   message describing if they were successfully cleared
#' \item   success - Boolean for success status
#' }
#'   
#' @export

ClearSessionOnly <-  function(domain, dataset){
  
  stopifnot(is.character(domain), 
            is.character(dataset), 
            length(domain) == 1, 
            length(dataset) == 1)
  
  url <- paste0(swsContext.baseRestUrl, "/r/data/session_only/", swsContext.executionId)
  
  json <- list(token = swsContext.token,
               domain = domain,
               dataSet = dataset)
  
  PostRestCall(url, json)
}