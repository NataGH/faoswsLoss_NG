##' Calculate Element 131 (Reemployment Same Sector)
##' 
##' Element 131 (Reemployment Same Sector) is calculated as a simple ratio of
##' total supply: E_131 = R_131 * (total supply) / 100.
##' Values are always updated in this fashion unless the symbol for element 131
##' is not replaceable (i.e. it's an official figure).
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
##' passed data.table ("data") have element 131's value and symbol updated.
##' 
##' @export
##' 

calculateEle131 = function(stotal, data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = 131, aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    replaceIndex1 = data[, replaceable(get(element131Symb),
                                       get(ratio131Num) * get(stotal) / 100)]

    data[replaceIndex1,
         `:=`(c(element131Num, element131Symb),
              appendSymbol(get(ratio131Num) * get(stotal) / 100, "C"))]
    
    which(replaceIndex1)
}
