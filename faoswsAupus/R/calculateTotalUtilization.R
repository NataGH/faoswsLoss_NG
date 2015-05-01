##' Calculate Total Utilization
##' 
##' This function calculates total utilization for each row by summing up all
##' elements which correspond to utilization.  These are currently elements
##' 91 (Outflow), 96 (Standardized Outflow), 101 (Use for Animals), 111 (Use
##' for Same Product), 121 (Losses), 131 (Reemployment), 141 (Consumption),
##' 151 (Reemployment Other Sector), 161 (Final Existence), and 546 (Total
##' Demand).
##' 
##' Also, element 151 is not considered utilization for item types defined by
##' aupusGroups$reemploymentNotAsUtilization (currently only item type 53).
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return No value is returned, but a new column ("TOTAL_UTILIZATION") is
##' appended to the passed data.table.
##' 
##' @export
##' 

calculateTotalUtilization = function(data, aupusParam){
    
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(91, 95, 96, 101, 111, 121, 131, 141,
                                        151, 161, 546), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## We could sum the columns directly with +, but we wouldn't be able to
    ## exclude NA's.  Instead, we can use rowSums and pass the .SD function.
    ## In data.table, the .SD function returns the data.table corresponding
    ## to the by grouping, which in this case is each row individually.
    ## Moreover, we use .SDcols to restrict which columns are added.
    data[, TOTAL_UTILIZATION :=
             rowSums(.SD, na.rm = TRUE),
         .SDcols = c(element91Num, element95Num, element96Num,
                 element101Num, element111Num, element121Num,
                 element131Num, element141Num, element161Num,
                 element546Num)]

    ## For most item types, element 151 is also considered utilization
    invisible(data[!get(itemTypeCol) %in%
                       aupusGroups$reemploymentNotAsUtilization,
                   TOTAL_UTILIZATION := rowSums(.SD, na.rm = TRUE),
                   .SDcols = c("TOTAL_UTILIZATION", element151Num)])
}
