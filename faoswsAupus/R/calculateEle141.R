##' Calculate Element 141 (Consumption)
##' 
##' This function computes element 141 (Consumption)
##' 
##' The following AUPUS logic is used in computing this element:
##' 
##' \itemize{
##'     \item For commodity types in 58, 59, 60, and 61, element 141 is
##'     computed as E_11 + E_51 + E_61 - E_91 - E_95 - E_161.
##'     \item For commodity type 50 (Jute), the original AUPUS logic was very
##'     complex and also written in such a way as to cause potential errors.
##'     Thus, no processing is done for this commodity type: it is left
##'     unchanged by this function.
##'     \item For all other commodity types, the consumption is computed as a
##'     ratio of the total supply: E_141 = R_141 * (total supply) / 100
##' }
##' 
##' @param stotal The column name of data corresponding to total supply.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.
##' However, it also has a side effect: rows within the
##' passed data.table ("data") have element 141's value and symbol updated.
##' 
##' @export
##' 

calculateEle141 = function(stotal, data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(11, 51, 61, 91, 95, 141, 161),
                           aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName    
    
    ## First type of commodity: Consumption is defined as a ratio of total
    ## supply.
    replaceIndex1 = !data[, get(itemTypeCol) %in% c(50, 58, 59, 60, 61) &
                               replaceable(get(element141Symb),
                                           get(ratio141Num) * get(stotal))]
    data[replaceIndex1,
         `:=`(c(element141Num, element141Symb),
              appendSymbol(get(ratio141Num) * get(stotal) / 100, "C"))]

    ## NOTE(Michael): Calculation for commodity Jute (50) is not
    ##                replicated.
    
    ## Second type of commodity: Consumption is defined as a balance of several
    ## other elements.
    data[, newSummation := get(element11Num) + get(element51Num) +
                           get(element61Num) - get(element91Num) -
                           get(element95Num) - get(element161Num)]
    replaceIndex2 = data[, get(itemTypeCol) %in% c(58:61) &
                         replaceable(get(element141Symb), newSummation)]
    data[replaceIndex2,
         `:=`(c(element141Num, element141Symb),
              appendSymbol(newSummation, "C"))]
    
    data[, newSummation := NULL]
    
    which(replaceIndex1 | replaceIndex2)
}
