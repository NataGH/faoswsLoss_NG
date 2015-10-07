##' Get Rest Call
##' 
##' @param url
##' 
##' @import RJSONIO
##' @import RCurl

GetRestCall <- function(url) {

  ch <- RCurl::getCurlHandle()
  if (Sys.info()['sysname'] == 'Darwin') { 
    response <- RCurl::getURL(
      url = url,
      curl = ch,
      verbose = FALSE,
      noproxy = .swsenv$swsContext.noProxy,
      ssl.verifypeer = FALSE, 
      sslcert = path.expand(.swsenv$swsContext.clientP12),
      sslcertpasswd = .swsenv$swsContext.p12Password,
      ssl.verifyhost = 2,
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"))
  } else {
    response <- RCurl::getURL(
      url = url,
      curl = ch,
      verbose = FALSE,
      noproxy = .swsenv$swsContext.noProxy,
      ssl.verifypeer = FALSE, 
      sslcert = path.expand(.swsenv$swsContext.clientCertificate),
      sslkey = path.expand(.swsenv$swsContext.clientKey),
      ssl.verifyhost = 2,
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"))
  }

	# Check returned status code.
	#
	status <- RCurl::getCurlInfo(ch, which = "response.code")
	if(status != 200) {
	  erresponse <- fromJSON(response)
	  message <- ifelse(exists("message", erresponse), paste0("\nError message: ", erresponse[["message"]]), "")
	  details <- ifelse(exists("details", erresponse), paste0("\nDetails: ", erresponse[["details"]]), "")
	  
		stop(paste("Unable to perform REST call to SWS server. Status code was", status, message, details))
	}

	RJSONIO::fromJSON(response)
}
