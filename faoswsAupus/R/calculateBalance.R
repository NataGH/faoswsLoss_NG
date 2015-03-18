##' Calculate Balance
##' 
##' The function will calculate the balance (in most cases, the difference
##' between supply and utilization) and fill this value into the balanceElement
##' column.  However, if this computed value is negative, or if the
##' balanceElement symbol is not replaceable (see ?replaceable) then this
##' computed value is placed in the statistical discrepancy column.
##'
##' @param supply The column name of data corresponding to the calculated total
##' supply.
##' @param utilization The column correspoding to calculated total utilization.
##' @param balanceElement The column name of data corresponding to the
##' balancing element.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A logical vector indicating which rows were updated with the
##' balance.
##' 
##' @export
##' 

calculateBalance = function(supply, utilization, balanceElement, data,
                            aupusParam){
    
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(161, 171, 181), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    balanceElement = aupusParam$keyNames$balanceElementName
    
    ## TODO (Michael): Need to check the replace column of this
    ##                 function.
    
    ## Calculate the temporary value for balance
    data[!get(itemTypeCol) %in% c(aupusGroups$excludedFromBalance,
                                  aupusGroups$specialBalanceSugar),
         BALANCE := get(supply) - get(utilization)]
    data[get(itemTypeCol) %in% aupusGroups$specialBalanceSugar,
         BALANCE := ifelse(!is.na(get(element161Num)) &
                               !is.na(get(element171Num)),
                           # If 161, 171 are both non-missing, use these values
                           computeRatio(get(element161Num),
                                        get(element171Num)) * 1000,
                           # If one of 161 or 171 are missing, assign 0
                           0)]

    ## Reverse the value if balance element is 71.
    data[get(balanceElement) %in% aupusGroups$specialBalanceNegate,
         BALANCE := -BALANCE]

    valName = paste0(aupusParam$keyNames$valuePrefix, aupusParam$keyNames$elementName,
                     "_")
    flagName = paste0(aupusParam$keyNames$flagPrefix, aupusParam$keyNames$elementName,
                     "_")
    for(balElement in data[, unique(get(balanceElement))]){
        if(is.na(balElement))
            next
        data[get(balanceElement) == balElement,
             `:=`(c(paste0(valName, balElement),
                    paste0(flagName, balElement),
                    element181Num, element181Symb, "replaced"),
                  fillBalance(calculatedBalance = "BALANCE",
                              balanceNum = paste0(valName, balElement),
                              balanceSymb = paste0(flagName, balElement),
                              element181Num = element181Num,
                              element181Symb = element181Symb,
                              data = .SD)),
             by = balanceElement]
    }
    replaceIndex1 = which(unlist(data[, replaced]))
    data[, replaced := NULL]
    replaceIndex1
}
