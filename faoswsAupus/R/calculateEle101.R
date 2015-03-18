##' Calculate Element 101 (Use for Animals)
##' 
##' The amount used for animal feed is computed using a very simple formula:
##' (Element 101) = (Ratio 101) * (Total Supply) / 100.
##' Essentially, the amount used for animals is computed as a simple ratio of
##' the total supply.  This function performs that calculation.
##' 
##' @param stotal The column name of data corresponding to the total supply.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 101's value and
##' symbol updated.
##' 
##' @export
##'

calculateEle101 = function(stotal, data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(stotal %in% colnames(data))
    
    ## Define columns of interest using aupusParam$
    defineElementVariables(elements = 101, aupusParam = aupusParam)
    
    replaceIndex1 = which(data[, replaceable(get(element101Symb),
                                             get(ratio101Num) * get(stotal))])
    data[replaceIndex1,
         `:=`(c(element101Num, element101Symb),
              appendSymbol(get(ratio101Num) * get(stotal) / 100,
                           "C"))]
    replaceIndex1
}
