##' Calculate Element 71
##' 
##' This function calculates element 71 (from initial existence).
##' 
##' Element 71 is only computed for a few commodity types: 57, 58, 59, 60, and
##' 61.  For the last three types, the formula is short: 
##' element 71 = element 161 - element 11 (i.e. Final Existence minus
##' Initial Existence).  For the first two types, the computation is
##' element 71 = element 51' + element 61 - element 91 -
##' element 101  - element 121 - element 131 - element 141 - element 151.
##'
##' Note: The documentation aupus_code_analysis.pdf has a typo.  It does not
##' mention how commodity type 57 should be handled, but after a conversation
##' with Nick it was confirmed that commodity type 57 should be handled in the
##' same way as commodity type 58.
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns a list (of length 2) of integer vectors
##' containing the row numbers of observations which were updated (the first
##' vector when a value was computed for commodity type 58, the second for when
##' the value was computed for commodity types 59, 60, or 61).
##' However, it also has a side effect: rows within the passed
##' data.table ("data") have element 63's value and symbol updated.
##' 
##' @export
##' 

calculateEle71 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Define columns of interest using aupusParam$
    defineElementVariables(elements = c(51, 61, 71, 91, 101, 121, 131, 141,
                                        151, 161),
                           aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## First group: item type of 58
    data[, newSummation := get(element51Num) + get(element61Num) -
                               get(element91Num) - get(element101Num) -
                               get(element121Num) - get(element131Num) -
                               get(element141Num) - get(element151Num)]
    replaceIndex1 = which(data[, get(itemTypeCol) %in% c(57, 58) &
                                   replaceable(get(element71Symb),
                                               newSummation)])
    data[replaceIndex1,
         `:=`(c(element71Num, element71Symb),
              appendSymbol(newSummation, "C"))]

    ## Second group: item type of 59, 60, or 61
    data[, newSummation := get(element161Num) - get(element101Num)]
    replaceIndex2 = which(data[, get(itemTypeCol) %in% c(59, 60, 61) &
                                    replaceable(get(element71Symb),
                                                newSummation)])
    data[replaceIndex2,
         `:=`(c("element71Num", "element71Symb"),
              appendSymbol(newSummation, "C"))]
    
    list(replaceIndex1, replaceIndex2)
}
