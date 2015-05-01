##' Define Element Variables
##' 
##' Much of the AUPUS processing is done by computing functions of particular
##' elements and using those calculated variables to update another element.
##' Thus, it's often very important to have variables which specify which
##' columns are needed.  This specification can be very tedious, as often many
##' elements are needed to compute a new element (as well as their
##' flags/symbols and/or ratios).
##' 
##' To avoid this complication, this function takes as an input the elements of
##' interest and the aupusParam list (which defines how all the element value,
##' flag, and ratio columns are named) and assigns the needed variables back to
##' the calling environment (see example).  For each element, three variables
##' are created in the environment: elementXNum, elementXSymb, and ratioXNum
##' (where X is the passed element number).  These variables contain the column
##' names of the nodes dataset that correspond with the value and symbol column
##' for element X, respectively.
##' 
##' @param elements A numeric vector containing the element numbers of
##' interest.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param envir An environment which specifies where the variables should be
##' assigned.  This defaults to parent.frame(1), which gives the calling
##' environment, and thus should be sufficient for most (if not all) use cases.
##' 
##' @examples
##' \dontrun{
##' exists("element51Num")
##' exists("element51Symb")
##' exists("ratio51Num")
##' defineElementVariables(51, faoswsAupus::aupusParam)
##' exists("element51Num")
##' exists("element51Symb")
##' exists("ratio51Num")
##' }
##' 
##' @return No value is explicitly returned.  However, variables are assigned
##' in the calling environment of this function.
##' 
##' @export
##' 

defineElementVariables = function(elements, aupusParam, envir = parent.frame(1)){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is.numeric(elements))
    ## Ensure all codes passed are valid.  Note that elementCodeDescription
    ## has more codes than actually available in the database, so this doesn't
    ## completely guarantee the passed code is valid.
    stopifnot(elements %in% elementCodeDescription[, 1])
    
    for(element in elements){
        ## Create elementXXNum
        assign(x = paste0("element", element, "Num"), 
               value = paste0(aupusParam$keyNames$valuePrefix,
                          aupusParam$keyNames$elementName, "_", element),
               envir = envir)
        ## Create elementXXSymb
        assign(x = paste0("element", element, "Symb"), 
               value = paste0(aupusParam$keyNames$flagPrefix,
                          aupusParam$keyNames$elementName, "_", element),
               envir = envir)
        ## Create ratioXXNum
        assign(x = paste0("ratio", element, "Num"),
               value = paste0(aupusParam$keyNames$ratioPrefix,
                          aupusParam$keyNames$elementName, "_", element),
               envir = envir)
    }

}
