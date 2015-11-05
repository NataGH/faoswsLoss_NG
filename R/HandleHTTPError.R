#' HTTP Error handling
#' 
#' Gives a more verbose error message when an HTTP Error is received.
#' 
#' @param status HTTP status (numeric)
#' @param response Server response
#' 
#' @author Sebastian Campbell <sebastian.campbell@@fao.org>
#' 
#' @keywords internal
#' 

HandleHTTPError <- function(status, response){
    erresponse <- fromJSON(response)
    message <- ifelse(exists("message", erresponse), paste0("\nError message: ", erresponse[["message"]]), "")
    details <- ifelse(exists("details", erresponse), paste0("\nDetails: ", erresponse[["details"]]), "")
    
    stop(paste("Unable to perform REST call to SWS server. Status code was", status, message, details))
  
}