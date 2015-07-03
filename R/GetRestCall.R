##' Get Rest Call
##' 
##' @param url
##' 

GetRestCall <- function(url) {

  ch <- RCurl::getCurlHandle()
  if (Sys.info()['sysname'] == 'Darwin') { 
    response <- RCurl::getURL(
      url = url,
      curl = ch,
      verbose = FALSE,
      noproxy = swsContext.noProxy,
      ssl.verifypeer = FALSE, 
      sslcert = path.expand(swsContext.clientP12),
      sslcertpasswd = swsContext.p12Password,
      ssl.verifyhost = 2,
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"))
  } else {
    response <- RCurl::getURL(
      url = url,
      curl = ch,
      verbose = FALSE,
      noproxy = swsContext.noProxy,
      ssl.verifypeer = FALSE, 
      sslcert = path.expand(swsContext.clientCertificate),
      sslkey = path.expand(swsContext.clientKey),
      ssl.verifyhost = 2,
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"))
  }

	# Check returned status code.
	#
	status <- RCurl::getCurlInfo(ch, which = "response.code")
	if(status != 200) {
		stop(paste("Unable to perform REST call to SWS server. Status code was", status))
	}

	RJSONIO::fromJSON(response)
}
