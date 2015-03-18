##' Calculate Element 174 (Consumption per Caput, Sugar only)
##' 
##' This function computes consumption per caput for sugar.  It is simply the
##' ratio of total consumption over total population, and so the calculation
##' is element 171 divided by population 11.
##' 
##' @param population11Num The column corresponds to element 11 of the
##' population.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 174's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle174 = function(population11Num, data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(171, 174), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    replaceIndex1 = data[, get(itemTypeCol) == 57 &
                         replaceable(get(element174Symb),
                                     computeRatio(get(element171Num),
                                        get(population11Num)))]
    data[replaceIndex1,
         `:=`(c(element174Num, element174Symb),
              appendSymbol(computeRatio(get(element171Num),
                                        get(population11Num)), "C"))]
    
    which(replaceIndex1)
}
