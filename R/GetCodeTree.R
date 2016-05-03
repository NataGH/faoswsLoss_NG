##' Get Code Tree
##' 
##' @param domain A character value specifying the domain for which the code list is
##' required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param dimension A character value specifying the name of the key for which
##' the code list is required.
##' @param roots [optional] A list of root codes for the tree. Default is all
##' those with no parent node.
##' 
##' @return A data table containing the parent codes and lists of child codes
##' in the tree (see the Code Tree structure above).
##' 
##' @examples
##' \dontrun{
##' GetCodeTree(domain = "agriculture", dataset = "agriculture",
##'                      dimension = "geographicAreaM49", roots = "953")
##' }
##' 
##' @export GetCodeTree

GetCodeTree <- function(domain, dataset, dimension, roots) {

	# Validate passed arguments.
	#
	GetCodeTree.validate(domain, dataset, dimension, roots)

	# Prepare JSON for REST call.
	#
	json <- GetCodeTree.buildJSON(domain, dataset, dimension, roots)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/dimensionValues/", domain, "/", dataset, "/", dimension, "/tree")
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
		json[["codes"]] <- NA
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

		strbuf <- ""
		first <- TRUE
		for(child in node$children) {
			if(first) {
				first <- FALSE
			} else {
				strbuf <- paste0(strbuf, ", ")
			}
			strbuf <- paste0(strbuf, child$code)
		}

		if(missing(dt)) {
			dt <- data.table(parent = node$code, children = strbuf)
		} else {
			dt <- rbind(dt, data.table(parent = node$code, children = strbuf))
		}

		dt <- GetCodeTree.recurseTree(node$children, dt)
	}

	dt
}
