GetCodeTree <- function(domain, dataset, dimension, roots) {

	# Validate passed arguments.
	#
	GetCodeTree.validate(domain, dataset, dimension, roots)

	# Prepare JSON for REST call.
	#
	json <- GetCodeTree.buildJSON(domain, dataset, dimension, roots)

	# Perform REST call.
	#
	url <- paste0("https://swsrm:8181/sws/rest/r/dimensionValues/", domain, "/", dataset, "/", dimension, "/tree")
	data <- PostRestCall(url, json)

	# Create result data table.
	#
	GetCodeTree.processResult(data)
}


GetCodeTree.validate <- function(domain, dataset, dimension, roots) {

	# Validate passed domain.
	#
	if(missing(domain)) {
		stop("The domain argument is mandatory.")
	}
	if(class(domain) != "character") {
		stop("The passed domain argument is not a character value.")
	}

	# Validate passed dataset
	#
	if(missing(dataset)) {
		stop("The dataset argument is mandatory.")
	}
	if(class(dataset) != "character") {
		stop("The passed dataset argument is not a character value.")
	}

	# Validate passed dimension
	#
	if(missing(dimension)) {
		stop("The dimension argument is mandatory.")
	}
	if(class(dimension) != "character") {
		stop("The passed dimension argument is not a character value.")
	}
}


GetCodeTree.buildJSON <- function(domain, dataset, dimension, roots) {
	
	# Build JSON for REST call.
	#
	json <- list()

	# Add the roots if they have been specified otherwise simply add a null field.
	#
	if(missing(roots)) {
		json[["codes"]] <- NULL
	} else {
		json[["codes"]] <- I(roots)
	}

	json
}


GetCodeTree.processResult <- function(data) {

	GetCodeTree.recurseTree(data$children)
}


GetCodeTree.recurseTree <- function(nodes, dt = NULL) {

	for(node in nodes) {

		if(node$leaf) {
			next
		}

		str <- ""
		first <- TRUE
		for(child in node$children) {
			if(first) {
				first <- FALSE
			} else {
				str <- paste0(str, ", ")
			}
			str <- paste0(str, child$code)
		}

		if(missing(dt)) {
			dt <- data.table(parent = node$code, children = str)
		} else {
			dt <- rbind(dt, data.table(parent = node$code, children = str))
		}

		dt <- GetCodeTree.recurseTree(node$children, dt)
	}

	dt
}
