##' Get Rest Call
##' 
##' @param url URL to which to make a GET request.
##' @param nullValue Value that should be used for missingValue. See
##'   \link{fromJSON} in RJSONIO for details.
##'   
##' @import RJSONIO
##' @import RCurl
##' 
##' @keywords internal

GetRestCall <- function(url, nullValue = NULL) {

  ch <- RCurl::getCurlHandle()
  
  withCallingHandlers(if (Sys.info()['sysname'] == 'Darwin') { 
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
  },
  SSL_CONNECT_ERROR = function(e){
    stop("Incorrect certificates. Either use 'SetClientFiles' or put the correct certificates in ", 
         dirname(.swsenv$swsContext.clientCertificate), call. = FALSE)
  })

	# Check returned status code.
	#
	status <- RCurl::getCurlInfo(ch, which = "response.code")
	if(status != 200) {
	  HandleHTTPError(status, response)
	}

	# Prevent error on blank response
	if(response == ""){
	  return(invisible(""))
	} else {
	  return(RJSONIO::fromJSON(response, nullValue = nullValue))
	}
	
}
