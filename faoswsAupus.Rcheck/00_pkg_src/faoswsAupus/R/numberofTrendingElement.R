##' Number of Trended Elements
##' 
##' This function takes several inputs, typically vectors, and returns a vector
##' whose ith element specifies the number of input vectors with symbol equal
##' to "T" in the ith position (which denotes a trended estimate).
##'
##' @param ... The objects
##' 
##' @return A vector of the number of trended symbols in the ith position.
##' 
##' @examples
##' numberOfTrendingElement(c("M","","","T",""),
##'                         c("M", "T", "M", "T", ""),
##'                         c("C", "C", "T", "T", "T"))
##' 
##' @export
##' 

numberOfTrendingElement = function(...){
    elementMatrix = data.frame(...)
    rowSums(elementMatrix == "T", na.rm = "T")
}
