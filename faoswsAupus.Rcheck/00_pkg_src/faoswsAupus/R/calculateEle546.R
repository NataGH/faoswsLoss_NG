##' Calculate Element 546 (Total Demand)
##' 
##' Element 546 (Total Demand) is computed as a summation of elements 541, 151,
##' and 91  If some of those values are missing, they are ignored in
##' the calculation.  However, if all three of the values are missing, then the
##' resulting calculation returns a 0M for element 546.
##' 
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.
##' However, it also has a side effect: rows within the passed
##' data.table ("data") have element 546's value and symbol updated.
##' 
##' @export
##' 

calculateEle546 = function(data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(151, 91, 541, 546), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    data[, newSummation := rowSums(.SD, na.rm = TRUE),
         .SDcols = c(element541Num, element151Num, element91Num)]
    data[, numberOfMissingElements :=
             numberOfMissingElement(get(element541Num), get(element151Num),
                                    get(element91Num))]
    replaceIndex1 = data[, replaceable(get(element546Symb), newSummation)]
    data[replaceIndex1,
         `:=`(c(element546Num, element546Symb),
              appendSymbol(newSummation, "C"))]
    replaceIndex2 = data[, numberOfMissingElements == 3 &
                             replaceable(get(element546Symb))]
    data[replaceIndex2,
         `:=`(c(element546Num, element546Symb), list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]
    data[, newSummation := NULL]

    which(replaceIndex1)
}
