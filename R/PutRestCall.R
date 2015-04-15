##' Put Rest Call
##' 
##' @param url
##' @param data
##' 

PutRestCall <- function(url, data) {

	ch <- RCurl::getCurlHandle()
	response <- RCurl::httpPUT(
		url = url,
		curl = ch,
		verbose = FALSE,
		noproxy = swsContext.noProxy,
		ssl.verifypeer = FALSE, 
		sslcert = path.expand(swsContext.clientCertificate),
		sslkey = path.expand(swsContext.clientKey),
		#cacert = path.expand(swsContext.serverCertificate),
		ssl.verifyhost = 2,
		httpheader = c(Accept = "application/json", 'Content-Type' = "application/json", Expect = ""),
		content = RJSONIO::toJSON(data, digits = 30))

	# Check returned status code.
	#
	status <- RCurl::getCurlInfo(ch, which = "response.code")
	if(status != 200) {
		stop(paste("Unable to perform REST call to SWS server. Status code was", status))
	}

	RJSONIO::fromJSON(response)
}
