##' swsContext
##' 
##' These objects specify the location of important files for data access.
##' 
##' @name swsContext
##' @aliases swsContext.clientCertificate
##' @aliases swsContext.clientKey
##' @aliases swsContext.serverCertificate
##' 
##' @details Most of these objects are created by a call to GetTestEnvironment.
##' 

swsContext.clientCertificate <- "~/.R/client.crt"
swsContext.clientKey <- "~/.R/client.key"
swsContext.clientP12 <- "~/.R/client.p12"
swsContext.p12Password <- "changeme"
swsContext.serverCertificate <- "~/.R/s1as.pem"
swsContext.noProxy <- "*"
