##' Calculate Element 11 (Initial Existence)
##' 
##' Initial existence (element 11) at time t is imputed with final existence
##' (element 161) at time t-1, but imputation only occurs if data is missing.
##' Note that not all item types are not updated in this way (see code).
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 11's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle11 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(11, 161), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    data[, `:=`(c("newValue", "newSymbol"),
                trendOnce(Num = get(element161Num), Symb = get(element161Symb),
                          transfer = TRUE)),
          by = c(aupusParam$keyNames$itemName)]
    ## NOTE (Michael): The item Code list for stock type is contained
    ##                 in appendix A
    replaceIndex1 =
        data[, get(itemTypeCol) %in% c(2:10, 13, 19:22, 25:28, 30, 57) &
                  replaceable(get(element11Symb), newValue)]
    data[replaceIndex1, `:=` (c(element11Num, element11Symb),
                              .(newValue, newSymbol))]
    data[, `:=` (c("newValue", "newSymbol"), NULL)]                         
    which(replaceIndex1)
}
