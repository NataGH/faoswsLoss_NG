##' Calculate Element 66 (standardized inflow)
##' 
##' This function does not seem to be run.  It contains functions which are not
##' defined, and the documentation states that computation of this element is
##' only for commodities that are generally not used within AUPUS, and that are
##' only trade commodity types.
##' 
##' @param shares The column name of the share variable in shareData (i.e. the
##' column containing the proportion of the parent commodity that is passed to
##' the child commodity).
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param shareData The data.table containing the shares data.
##'
##' @return This function is not currently working.
##' 
##' @export
##' 

calculateEle66 = function(shares, data, shareData){

    stop("This function does not appear to be used.  It calls a function ",
         "'standardizeCommodityNetwork' which does not seem to be defined.")
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(41, 61, 66), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    setnames(shareData,
             old = shares,
             new = "shares")
    ## NOTE (Michael): This can cause discrepancy if the manual data
    ##                 contradict each other.
    replaceIndex1 =
        which(data[itemTypeCol %in% c(2:13, 19:22, 25:30, 39) &
              replaceable(get(element66Symb)), ])
    data[replaceIndex1,
         `:=`(c(element66Num, element66Symb),
              appendSymbol(standardizeCommodityNetwork(shareData = shareData[Year == .BY[[1]], ],
                                                       aupus = data[Year == .BY[[1]], ],
                                                       extractionRate = element41Num,
                                                       standardizeElement = element61Num,
                                                       shares = "shares",
                                                       commodity = .SD$itemCode),
                           "C")),
         by = c(aupusParam$keyNames$yearName)]
    setnames(shareData,
             new = shares,
             old = "shares")    
    replaceIndex1
}
