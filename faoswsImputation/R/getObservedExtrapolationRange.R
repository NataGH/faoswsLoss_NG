##' Get Observed Extrapolation Range
##' 
##' This function takes a vector of data (with posisbly missing values) and
##' returns a vector of the same length.  If the corresponding value of x is
##' not missing or is an interpolation, the returned value is 0.  If the
##' corresponding value of x is missing and is an extrapolation, then the
##' returned value is the "distance" (in terms of number of observations) to
##' the closest non-missing value.  See examples.
##' 
##' @param x The vector of data.
##' 
##' @return A vector of the same length as x.
##' 
##' @examples
##' x = c(NA, NA, NA, 1:7, NA, NA)
##' getObservedExtrapolationRange(x)
##' 
##' @export
##' 

getObservedExtrapolationRange = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))

    if(all(is.na(x)))
       return(as.numeric(rep(NA, length(x))))
    
    firstNaCount = min(which(!is.na(x))) - 1
    lastNaCount = length(x) - max(which(!is.na(x)))
    #Vector of the distance to the nearest observed point (for extrapolations)
    c(seq(firstNaCount, 1, length.out = firstNaCount),
      rep(0, length(x)-firstNaCount-lastNaCount),
      seq(1, lastNaCount, length.out = lastNaCount))
}