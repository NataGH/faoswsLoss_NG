##' Number of Missing Elements
##' 
##' This function takes several inputs, typically vectors, and returns a vector
##' whose ith element specifies the number of input vectors with missing values
##' in the ith position.
##'
##' @param ... The objects
##' 
##' @return A vector of the number of total inputs missing for each ith
##' element.
##' 
##' @examples
##' numberOfMissingElement(c(1,2,NA,NA,5),
##'                        c(1,NA,2,3,NA),
##'                        c(1,2,NA,4,NA))
##' 
##' @export
##' 

numberOfMissingElement = function(...){
    elementMatrix = data.frame(...)
    rowSums(is.na(elementMatrix))
}