##' Get Test Environment
##' 
##' @param baseUrl The url for the SWS server:
##' \itemize{
##'   \item QA: \code{https://hqlqasws1.hq.un.fao.org:8181/sws}
##'   \item Production: \code{https://hqlprswsas1.hq.un.fao.org:8181/sws}
##' }
##' 
##' @param token A token which tells the SWS system what dataset to access. This
##'   token can be obtained from the system by
##' 
##' \itemize{
##'     \item Opening a session with the relevant data.
##'     \item Clicking "R Plugins"
##'     \item Selecting the relevant script to analyze.
##'     \item Clicking "New debug session".
##' }
##' 
##' If the module does not currently exist on the system, you will need to
##' upload a zipped file with an xml file specifying the dataset configurations.
##'   
##' @return This function silently returns a list containing a DatasetKey object and creates 
##'   "swsContext" objects, such as \code{swsContext.datasets}, \code{swsContexts.token}, 
##'   etc., including \code{swsContext.datasets} which is identical to the object 
##'   returned.
##'   
##' @export GetTestEnvironment

GetTestEnvironment <- function(baseUrl, token) {
    # Validate passed token
    #
    if (missing(token)) {
      stop("The token argument is mandatory.")
    }
    
    # Perform REST call.
    #
    url <- paste0(baseUrl, "/rest/r/computationParameters/", token)
    config <- GetRestCall(url)
    
    swsContext.baseRestUrl <<- config$result$baseRestUrl
    swsContext.userId <<- config$result$userDto$id
    swsContext.username <<- config$result$userDto$username
    swsContext.userEmail <<- config$result$userDto$email
    swsContext.token <<- config$result$token
    swsContext.executionId <<- config$result$id
    
    swsContext.computationParams <<-
      lapply(config$result$parameters, function(x) {x})
    
    swsContext.datasets <<-
      sapply(config$result$datasets, function(x) {
        dk <- DatasetKey(domain = x$domainCode,
                         dataset = x$dataSetCode)
        
        if (!is.null(x$sessionId)) {
          dk@sessionId <- as.integer(x$sessionId)
        }
        
        dimensions <- sapply(names(x$dimensions2Codes), function(y) {
          
          keys <- unlist(x$dimensions2Codes[y], use.names = FALSE)
          #If there are no keys, don't assign NULL (S4 doesn't allow it)
          if(is.null(keys)){
            keys <- character()
          }
          
          Dimension(name = y,
                    keys = keys)
          
        })
        
        dk@dimensions <- dimensions
        
        dk
      })
}
