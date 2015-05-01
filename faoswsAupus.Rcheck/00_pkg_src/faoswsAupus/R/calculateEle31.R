##' Calculate Element 31
##' 
##' The value of this element is set to the sum of all the input values
##' from the inputFromProcessing table (see getInputFromProcessData).
##' Otherwise, the value (if replaceable) is set to NA.
##'
##' @param inputNum The column name of data corresponding to inputs from
##' processing.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns a list (of length two) of integer vectors
##' containing the row numbers of observations which were updated (first by
##' computing as a function of inputs, then next by setting to NA).  However,
##' it also has a side effect: rows within the passed data.table ("data") have
##' element 31's value and symbol updated.
##' 
##' @export
##' 

calculateEle31 = function(inputNum, data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = 31, aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## NOTE (Michael): These are assumed from the names in Annex 3
    replaceIndex1 =
        which(data[get(itemTypeCol) %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                           28, 3, 32, 33, 5, 6, 7, 8, 9) &
              !is.na(get(inputNum)) & 
              replaceable(get(element31Symb), get(inputNum)), ])
    data[replaceIndex1, 
         `:=`(c(element31Num, element31Symb),
              appendSymbol(get(inputNum), "C"))]
    ## If neither input from processing or manual value is available,
    ## then replace with NA.
    replaceIndex2 = 
        which(data[itemTypeCol %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                      28, 3, 32, 33, 5, 6, 7, 8, 9) &
              is.na(get(inputNum)) & 
              replaceable(get(element31Symb), NA), ])
    data[replaceIndex2,
         `:=`(c(element31Num, element31Symb), list(NA, "M"))]
    list(replaceIndex1, replaceIndex2)
}
