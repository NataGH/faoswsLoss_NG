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

.swsenv <- new.env()

.swsenv$swsContext.clientCertificate <- "~/.R/client.crt"
.swsenv$swsContext.clientKey <- "~/.R/client.key"
.swsenv$swsContext.clientP12 <- "~/.R/client.p12"
.swsenv$swsContext.p12Password <- "changeme"
.swsenv$swsContext.serverCertificate <- "~/.R/s1as.pem"
.swsenv$swsContext.noProxy <- "*"
