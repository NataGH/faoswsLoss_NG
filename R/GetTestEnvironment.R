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
##' @section Credentials on Windows and Linux:
##' 
##' In order to work the the SWS, credentials (certificates, keys, etc.) must be
##' downloaded from Sharepoint 
##' (\url{https://workspace.fao.org/tc/sws/userspace}). In this project it can 
##' be found under 'Shared Documents/R Development'. Copy the contents of the 
##' credentials for the server you wish to access into ~/.R on your local 
##' machine. If you wish to place them somewhere else or you wish to access 
##' different servers at different times, use \code{\link{SetClientFiles}} in
##' your script to nominate a folder. The certificate should appear with a red
##' flag (untrusted), change the certificate to be always trusted. To test, run
##' the code in the example.
##' 
##' @section Credentials on MacOS:
##' 
##' R developers that connect to SWS from MacOS need to use a different set of 
##' credentials. This is due to different assumptions in the implementation of 
##' the underlying libraries for secure communication. The credentials for Mac 
##' users are in a single file - \code{client.p12} - which is distributed along 
##' with all the other credentials under a \code{macos} folder. It needs to be 
##' installed in the KeyChain (e.g. by double- clicking on the file). The
##' password is 'changeme'.
##'   
##' @return This function silently returns a list containing a DatasetKey object
##'   and creates "swsContext" objects, such as \code{swsContext.datasets},
##'   \code{swsContexts.token}, etc., including \code{swsContext.datasets} which
##'   is identical to the object returned.
##'   
##' @examples
##' \dontrun{
##' 
##' library(faosws) 
##' library(data.table) 
##' if (CheckDebug()) { 
##'   SetClientFiles("~/certificates/qa") 
##'   GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "98a2d80b-e55b-424a-af62-05890a9bcb6b") 
##' } 
##' 
##' }
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
