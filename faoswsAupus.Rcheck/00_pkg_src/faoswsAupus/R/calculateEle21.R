##' Calculate Element 21 (Potential Producing Factor)
##' 
##' The rule for replacement here is
##' (element 21) = (element 111) * 1000 / (ratio 171)
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.  However, it
##' also has a side effect: rows within the passed data.table ("data") have
##' element 21's value and symbol updated.
##' 
##' @export
##' 

calculateEle21 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(11, 21, 111, 171), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## If the item was population then copy from element 11
    replaceIndex = which(data[[aupusParam$keyNames$itemName]] == 1 &
                         replaceable(data[[element21Symb]],
                                     data[[element11Num]]))
    data[replaceIndex,
         `:=`(c(element21Num, element21Symb),
              appendSymbol(get(element11Num), "/"))]

#     ## NOTE (Josh): This element is not computed in the AUPUS if it is not
#     ## population.  The formula below assumes some calculation, and is not
#     ## consistent with the aupus_code_analysis.pdf document or the original
#     ## C++ code (see aupus.cpp lines 1495-1567.
#     
#     ## NOTE (Michael): This formula is derived from the formula of
#     ##                 element 111 which has the reverse calculation.
#     replaceIndex2 = which(data[, get(itemTypeCol) %in% c(2, 3, 9, 29, 30) &
#                          replaceable(get(element21Symb))])
#     data[replaceIndex2,
#          `:=`(c(element21Num, element21Symb),
#               appendSymbol(get(element111Num) *
#                     faoswsUtil::computeRatio(1000, get(ratio171Num)), "C"))]
#     list(replaceIndex, replaceIndex2)
    replaceIndex
}
