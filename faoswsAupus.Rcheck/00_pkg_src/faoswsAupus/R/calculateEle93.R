##' Calculate Element 93
##' 
##' This function calculates element 93 (outflow unit value).  The logic is
##' very similar to the calculation for element 63 (inflow unit value) in that
##' unit value (93) should be the total value (92) divided by the total
##' quantity (91).  Also, the multiplicative constant of 1000 is still used in
##' this case, so the formula is 93 = 92 * 1000 / 91.
##' 
##' However, there is one difference between computation of element 93 and
##' element 63: computation is not done for elements 42 through 52.
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

calculateEle93 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(91, 92, 93), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## Calculate value
    replaceIndex1 = data[, !get(itemTypeCol) %in% c(42, 43, 44, 45, 46, 47,
                                                    48, 49, 50, 51, 52) &
                             !is.na(get(element91Num)) &
                             !is.na(get(element92Num)) &
                             replaceable(get(element93Symb),
                                         computeRatio(get(element92Num) * 1000,
                                                      get(element91Num)))]
    data[replaceIndex1,
         `:=`(c(element93Num, element93Symb),
              appendSymbol(get(element92Num) *
                               computeRatio(1000, get(element91Num)), "C"))]

    replaceIndex2 = data[, !get(itemTypeCol) %in% c(42, 43, 44, 45, 46, 47,
                                                    48, 49, 50, 51, 52) &
                             replaceable(get(element93Symb),
                                    get(element91Num) * get(element92Num)) &
                             (is.na(get(element91Num)) |
                              is.na(get(element92Num)))]
    data[replaceIndex2,
         `:=`(c(element93Num, element93Symb),
              list(NA, "M"))]
    
    list(which(replaceIndex1), which(replaceIndex2))
}
