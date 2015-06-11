##' Dimension Class
##' 
##' A dimension is represented by its name and by the vector of selected 
##' codes/keys.  This object is then placed in a list with other Dimension 
##' objects and passed to a DatasetKey object.
##' 
##' Note: for some reason, this function fails if the arguments are not named. 
##' So, you will need to always specify "name =" and "key =" in your call to 
##' Dimension.
##' 
##' @param name The name of the dimension to access, some examples are 
##'   "timePointYears", "measuredElement", or "geographicAreaM49".  These 
##'   correspond to the dimensions on the SWS, but the names are slightly 
##'   different.  Currently, if you want to find the name of a dimension, you 
##'   can check the following url: 
##'   http://hqlqasws1.hq.un.fao.org:8080/dataset_configuration.html.
##' @param keys The key codes specifying what values should be pulled for the 
##'   dimension.
##'   
##' @return An object of class Dimension.  This is used to pass onto the 
##'   DatasetKey class.
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
##' @seealso \code{\link{DatasetKey}}, \code{\link{GetData}}
##' 

Dimension <- setClass("Dimension",
    representation(
        name = "character",
        keys = "character"))

# A dimension is valid only if it has the name properly set up.
# The list of keys is not mandatory.
#
setValidity("Dimension", function(object) {
	
	msg <- NULL
	valid <- TRUE

	if(length(object@name) != 1) {
		valid <- FALSE
		msg <- c(msg, "The name of the dimension was not properly set.")
	}

	if (valid) {
		TRUE
	} else {
		msg
	}
})

# The addKey method can be used to add a key to the dimension.
#
setGeneric("addKey<-", function(object, value) standardGeneric("addKey<-"))
setReplaceMethod(
	"addKey",
	"Dimension",	
	function(object, value) {
		object@keys <- c(object@keys, value)
		object
	}
)
