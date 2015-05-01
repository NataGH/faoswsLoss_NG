##' Calculate Element 41 (Productivity Element)
##' 
##' Calculation of element 41 is done by simply multiplying ratio 41 by 100.
##' Values which are replaceable are updated by this function.
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 41's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle41 = function(data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = 41, aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    replaceIndex1 =
        which(data[, replaceable(get(element41Symb), get(ratio41Num) * 100) &
                        !is.na(get(ratio41Num))])
    data[replaceIndex1,
         `:=`(c(element41Num, element41Symb),
              appendSymbol(get(ratio41Num) * 100, "C"))]
    replaceIndex1
}
