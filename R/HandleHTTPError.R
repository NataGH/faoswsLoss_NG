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
  
  if (missing(status)){
    status <- "unavailable"
  }
  if(missing(response)){
    stop(paste0("SWS could not return reason for error. HTTP status: ", status))
  } 
  erresponse <- try(jsonlite::fromJSON(response), silent = TRUE)
  if(inherits(erresponse, "try-error")) stop("SWS could not return reason for error. HTTP error " , status, "\n", erresponse)
  message <- ifelse(exists("message", erresponse), paste0("\nError message: ", erresponse[["message"]]), "")
  details <- ifelse(exists("details", erresponse), paste0("\nDetails: ", erresponse[["details"]]), "")
  
  stop(paste("Unable to perform REST call to SWS server. Status code was", status, message, details))
  
}
