##' Calculate Element 63
##' 
##' This function calculates element 63 (Inflow Unit Value) using element 61
##' (Inflow) and element 62 (Inflow Value).  The calculation is simple: unit
##' value (63) should be the total value (62) divided by the total quantity
##' (61).  However, a multiplicative constant of 1000 is also used, so the
##' formula is 63 = 62 * 1000 / 61.
##' 
##' Note that if 61 or 62 are not available, and if 63 is replaceable, then it
##' is replaced with a missing value (0M).
##' 
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns a list (of length 2) of integer vectors
##' containing the row numbers of observations which were updated (the first
##' vector when a value was computed, the second for when 62 or 61 was
##' missing).  However, it also has a side effect: rows within the passed
##' data.table ("data") have element 63's value and symbol updated.
##' 
##' @export
##' 

calculateEle63 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Define columns of interest using aupusParam$
    defineElementVariables(elements = c(61, 62, 63), aupusParam = aupusParam)
    
    ## Calculate element 63 from element 61 and 62 if both are
    ## available.
    replaceIndex1 = data[, !is.na(get(element61Num)) &
              !is.na(get(element62Num)) &
              replaceable(get(element63Symb), get(element62Num))]
    data[replaceIndex1,
         `:=`(c(element63Num, element63Symb),
              appendSymbol(get(element62Num) *
                               computeRatio(1000, get(element61Num)), "C"))]
    
    ## If any one of them is missing, then the new calculation would
    ## be missing. Therefore, replace with a missing value.
    replaceIndex2 = !replaceIndex1 & data[, replaceable(element63Symb, 0)]
    data[replaceIndex2,
         `:=`(c(element63Num, element63Symb),
              list(NA, "M"))]
    
    list(which(replaceIndex1), which(replaceIndex2))
}
