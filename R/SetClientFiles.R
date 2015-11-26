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
#' @param dir Directory where the client certificates are installed
#' @param certificate Path to "client.crt" file
#' @param key Path to "client.key" file
#' @param p12 path to "client.p12" file
#' @param p12password Password relating to logging in using Mac? 
#' @param servercertificate See above.
#' @param noproxy ?
#' 
#'   
#' @author Sebastian Campbell
#' @export SetClientFiles  

SetClientFiles <- function(dir = "~/.R",
                           certificate = file.path(dir, "client.crt"), 
                           key = file.path(dir, "client.key"), 
                           p12 = file.path(dir, "client.p12"), 
                           p12password = "changeme",
                           servercertificate = file.path(dir, "s1as.pem"),
                           noproxy = "*"){
  
  .swsenv$swsContext.clientCertificate <- certificate
  .swsenv$swsContext.clientKey <- key
  .swsenv$swsContext.clientP12 <- p12
  .swsenv$swsContext.p12Password <- p12password
  .swsenv$swsContext.serverCertificate <- servercertificate
  .swsenv$swsContext.noProxy <- noproxy
}
