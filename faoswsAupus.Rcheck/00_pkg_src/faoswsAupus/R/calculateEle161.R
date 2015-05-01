##' Calculate Element 161 (Final existence)
##' 
##' This element is only computed for commodity type 57 (sugar).  This element
##' is calculated as element 11 (Initial Existence) + element 71 (From Initial
##' Existence).
##' 
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 161's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle161 = function(data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(11, 71, 161), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    replaceIndex1 = data[, get(itemTypeCol) == 57 &
                         replaceable(get(element161Symb),
                                     get(element11Num) + get(element71Num))]
    data[replaceIndex1,
         `:=`(c(element161Num, element161Symb),
              appendSymbol(get(element11Num) + get(element71Num), "C"))]

    ## NOTE (Michael/JOsh): The documentation discusses more about what this
    ## function does.  But, it really just says that the next year is forced
    ## to be processed if it is of type trade and also
    ## element 11 was already calculated for the following year.
    
    which(replaceIndex1)
}
