##' Calculate Element 171 (Consumption, Sugar only)
##' 
##' This element is only calculated for sugar.  Moreover, it's just a summation
##' of 5 other commodities: 101, 121, 131, 141, and 151.
##' 
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 171's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle171 = function(data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(101, 121, 131, 141, 151, 171),
                           aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    ## Sum up 101, 121, 131, 141, and 151.  But, we want to remove NAs from the
    ## sum, so use rowSums on the .SD data.table.  We define .SDcols to be sure
    ## that the sum adds only the elements we want.
    data[, newValue := rowSums(.SD, na.rm = TRUE),
         .SDcols = c(element101Num, element121Num, element131Num,
                     element141Num, element151Num)]
    replaceIndex1 = data[, get(itemTypeCol) == 57 &
                         replaceable(get(element171Symb), newValue)]
    data[replaceIndex1,
         `:=`(c(element171Num, element171Symb),
              appendSymbol(newValue, "C"))]
    data[, newValue := NULL]

    which(replaceIndex1)
}
