PutRestCall <- function(url, data) {

	ch <- getCurlHandle()
	response <- httpPUT(
		url = url,
		curl = ch,
		verbose = FALSE,
		noproxy = "swsrm", # Remove this!
		ssl.verifypeer = FALSE, 
		sslcert = swsContext.clientCertificate,
		sslkey = swsContext.clientKey,
		#cacert = swsContext.serverCertificate,
		ssl.verifyhost = 2,
		httpheader = c(Accept = "application/json", 'Content-Type' = "application/json", Expect = ""),
		content = toJSON(data))

	# Check returned status code.
	#
	status <- getCurlInfo(ch, which = "response.code")
	if(status != 200) {
		stop(paste("Unable to perform REST call to SWS server. Status code was", status))
	}

	fromJSON(response)
}
