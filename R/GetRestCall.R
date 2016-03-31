##' Get Rest Call
##' 
##' @param url URL to which to make a GET request.
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
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"),
      .encoding = "UTF-8")
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
      httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"),
      .encoding = "UTF-8")
  }

	# Check returned status code.
	#
	status <- RCurl::getCurlInfo(ch, which = "response.code")
	if(status != 200) {
	  HandleHTTPError(status, response)
	}

	RJSONIO::fromJSON(response)
}
