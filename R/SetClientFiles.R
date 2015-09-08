#' Set Client File locations
#' 
#' This function allows changing of the the locations of the client files from 
#' their default location in '~/.R'. The client files consist of at minimum, a 
#' client key and a client certificate. Run this command before running
#' \code{\link{GetTestEnvironment}} in order to set or change client file
#' locations. Any parameter that is not explicitly set will be reset to
#' defaults. As a result running \code{SetClientFiles()} will reset all
#' parameters to their default values.
#' 
#' @param certificate
#'   
#'   
#' @export SetClientFiles  

SetClientFiles <- function(certificate = "~/.R/client.crt", 
                           key = "~/.R/client.key", 
                           p12 = "~/.R/client.p12", 
                           p12password = "changeme",
                           servercertificate = "~/.R/s1as.pem",
                           noproxy = "*"){
  
  .swsenv$swsContext.clientCertificate <- certificate
  .swsenv$swsContext.clientKey <- key
  .swsenv$swsContext.clientP12 <- p12
  .swsenv$swsContext.p12Password <- p12password
  .swsenv$swsContext.serverCertificate <- servercertificate
  .swsenv$swsContext.noProxy <- noproxy
}

# Set environment in which authentication variables reside
.swsenv <- new.env()
# Populates it with default variables
SetClientFiles()