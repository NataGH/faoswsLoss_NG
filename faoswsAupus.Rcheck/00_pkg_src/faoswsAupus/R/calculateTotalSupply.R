##' Calculate Total Supply
##' 
##' This function calculates total supply by summing up elements 51, 58, 61,
##' and 66 (see arguments below to understand what these items are).  Also,
##' the itemTypeCol is required as a handful of commodities are treated
##' differently when computing supply (see aupusGroups$initialAsSupply).
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return No value is returned, but a new column ("TOTAL_SUPPLY") is
##' appended to the passed data.table.  This column provides the total supply
##' for the balancing.
##' 
##' @export
##' 

calculateTotalSupply = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(11, 51, 58, 61, 66), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    data[, TOTAL_SUPPLY := rowSums(.SD, na.rm = TRUE),
         .SDcols = c(element51Num, element58Num, element61Num, element66Num)]

    ## Some item groups consider initial existence as supply
    invisible(data[get(itemTypeCol) %in% aupusGroups$initialAsSupply,
                   TOTAL_SUPPLY := rowSums(.SD, na.rm = TRUE),
                   .SDcols = c("TOTAL_SUPPLY", element11Num)])
    
}
