PostRestCall <- function(url, data) {

	ch <- getCurlHandle()
	response <- getURL(
		url = url,
		curl = ch,
		verbose = FALSE,
		noproxy = swsContext.noProxy,
		ssl.verifypeer = FALSE, 
		sslcert = path.expand(swsContext.clientCertificate),
		sslkey = path.expand(swsContext.clientKey),
		#cacert = path.expand(swsContext.serverCertificate),
		ssl.verifyhost = 2,
		httpheader = c(Accept = "application/json", 'Content-Type' = "application/json"),
		post = 1,
		postfields = toJSON(data, digits = 30))

	# Check returned status code.
	#
	status <- getCurlInfo(ch, which = "response.code")
	if(status != 200) {
		stop(paste("Unable to perform REST call to SWS server. Status code was", status))
	}

	fromJSON(response)
}
