##' Dataset Key Class
##' 
##' This class provides a mechanism for pulling data from the SWS.  You must
##' pass a DatasetKey object into the GetData function in order to read data
##' from the SWS.
##' 
##' @slot domain A character value specifying the domain of interest, such as 
##'   "agriculture".
##' @slot dataset A character value specifying the dataset within the domain, 
##'   such as "agriculture".
##' @slot dimensions A list of objects, each of class Dimension.
##' @slot sessionId The ID of the session from which data should be accessed. 
##'   If NULL, the database is used directly.
##'   
##' @note Many of these variables (domain, dataset, and dimensions) can be found
##'   by examining some of the swsContext objects which are created in a 
##'   GetTestEnvironment call (in a debug session).  Moreover, after executing 
##'   GetTestEnvironment, the swsContext.datasets object will contain a list of 
##'   objects of type DatasetKey.  Additionally, information about these objects
##'   can be found at the following url: 
##'   http://hqlqasws1.hq.un.fao.org:8080/dataset_configuration.html
##' 
##' @examples
##' \dontrun{
##' dim1 = Dimension(name = "geographicAreaM49", keys = c("12", "24", "204"))
##' dim2 = Dimension(name = "measuredElement", keys = c("51", "5510", "5607"))
##' dim3 = Dimension(name = "measuredItemCPC", keys = c("0111", "0112", "0113"))
##' dim4 = Dimension(name = "timePointYears", keys = as.character(1960:2010))
##' 
##' key = DatasetKey(domain = "agriculture", dataset = "agriculture",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' 
##' GetData(key = key)
##' }
##' 
##' @seealso \code{\link{Dimension}}, \code{\link{GetData}}
##' 
##' @export DatasetKey
##' @export

DatasetKey <- setClass("DatasetKey", 
	representation(
		domain = "character", 
		dataset = "character",
		dimensions = "list",
		sessionId = "integer"))

setValidity("DatasetKey", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@domain) != 1) {
		valid <- FALSE
		msg <- c(msg, "The domain of the key was not properly set.")
	}

	if(length(object@dataset) != 1) {
		valid <- FALSE
		msg <- c(msg, "The dataset of the key was not properly set.")
	}
	
	for(d in object@dimensions) {
		if(!class(d) == "Dimension" || !validObject(d)) {
			valid <- FALSE
			msg <- c(msg, "One of the passed dimensions is not a valid instance of the class Dimension.")
		}
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})
