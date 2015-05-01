##' Calculate Element 144 (Consumption Per Day)
##' 
##' The consumption per day is the ratio of total consumption to the
##' population.  This value is computed for all replaceable symbols (i.e. when
##' the current figure is not official).  For some commodity groups, an
##' adjustment is made by multiplying by 1000.
##' 
##' @param population11Num The column corresponds to element 11 of
##' population.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.
##' However, it also has a side effect: rows within the
##' passed data.table ("data") have element 144's value and symbol updated.
##' 
##' @export
##' 

calculateEle144 = function(population11Num, data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(141, 144), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    replaceIndex1 = data[, replaceable(get(element144Symb),
                            faoswsUtil::computeRatio(get(element141Num),
                                                     get(population11Num)))]
    data[replaceIndex1,
         `:=`(c(element144Num, element144Symb),
              appendSymbol(faoswsUtil::computeRatio(get(element141Num),
                                                    get(population11Num)) *
            ## Adjust by 1000 for certain commodity types
            ifelse(get(itemTypeCol) %in% c(46, 47, 48, 51,52, 58, 59, 60, 61),
                   1000, 1)
            , "C"))]

    which(replaceIndex1)
}
