GetTestEnvironment <- function(baseUrl, token) {

	# Validate passed token
	#
	if(missing(token)) {
		stop("The token argument is mandatory.")
	}

	# Perform REST call.
	#
	url <- paste0(baseUrl, "/rest/r/computationParameters/", token) 
	config <- GetRestCall(url)

	swsContext.baseRestUrl <<- config$result$baseRestUrl
	swsContext.userId <<- config$result$userDto$id
	swsContext.username <<- config$result$userDto$username
	swsContext.userEmail <<- config$result$userDto$email
	swsContext.token <<- config$result$token
	swsContext.executionId <<- config$result$id

	swsContext.computationParams <<- lapply(config$result$parameters, function(x) { x })

	swsContext.datasets <<- sapply(config$result$datasets, function(x) {
		dk <- DatasetKey(
			domain = x$domainCode,
			dataset = x$dataSetCode)

		if(!is.null(x$sessionId)) {
			dk@sessionId <- as.integer(x$sessionId)
		}

		dimensions <- sapply(names(x$dimensions2Codes), function(y) {
			Dimension(
				name = y, 
				keys = unlist(x$dimensions2Codes[y], use.names = FALSE))
		})

		dk@dimensions <- dimensions

		dk
	})
}
