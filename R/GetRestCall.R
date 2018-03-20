##' Get Rest Call
##' 
##' @param url URL to which to make a GET request.
##' @param nullValue Value that should be used for missingValue. See
##'   \link{fromJSON} in RJSONIO for details.
##'   
##' @import RJSONIO
##' @import curl
##' 
##' @keywords internal

GetRestCall <- function(url, nullValue = NULL) {

  # TODO check .encoding = "UTF-8" parameter that was passed to RCurl::getURL()

  ch <- curl::new_handle()
  curl::handle_setheaders(ch,
    .list = list(Accept = "application/json", `Content-Type` = "application/json"))
  
  withCallingHandlers({
    if (Sys.info()['sysname'] == 'Darwin') { 
      curl::handle_setopt(ch,
                          verbose = FALSE,
                          noproxy = .swsenv$swsContext.noProxy,
                          ssl_verifypeer = FALSE,
                          sslcert = path.expand(.swsenv$swsContext.clientP12),
                          sslcertpasswd = .swsenv$swsContext.p12Password,
                          ssl_verifyhost = 2)
    } else {
      curl::handle_setopt(ch,
                          verbose = FALSE,
                          noproxy = .swsenv$swsContext.noProxy,
                          ssl_verifypeer = FALSE,
                          sslcert = path.expand(.swsenv$swsContext.clientCertificate),
                          sslkey = path.expand(.swsenv$swsContext.clientKey),
                          ssl_verifyhost = 2)
    }
    response <- curl_fetch_memory(url, handle = ch)
  },
  SSL_CONNECT_ERROR = function(e){
    stop("Incorrect certificates. Either use 'SetClientFiles' or put the correct certificates in ", 
         dirname(.swsenv$swsContext.clientCertificate), call. = FALSE)
  })

	# Check returned status code.
	#
	status <- response$status_code
  content <- rawToChar(response$content)

	if (status != 200) {
	  HandleHTTPError(status, content)
	}

	# Prevent error on blank content
	if (content == "") {
	  return(invisible(""))
	} else {
	  return(RJSONIO::fromJSON(content, nullValue = nullValue))
	}
	
}
