##' Calculate Elements 31, 41, and 51
##' 
##' This function computes elements 31, 41, and 51 according to the AUPUS
##' logic.  The rules are as follows:
##' 
##' If exactly one of the three elements is missing, then that element is
##' computed from the others using the relationship
##' 51 = 31 * 41 / divisionFactor (i.e. "balancing").
##' 
##' If none of the three elements are missing, then any computed or trended
##' values are recomputed.  This is done by first attempting to balance: if
##' only one value needs to be recomputed, it is done so by balancing.  If more
##' than one value needs to be recomputed, we first "trend" element 31 (see
##' ?trendOnce).  With element 31 recomputed, we now attempt to balance again.
##' If we still cannot balance (i.e. 41 and 51 are still missing) we trend
##' element 41.  Now, as element 51 is the only missing value, it is computed
##' via balancing.
##' 
##' Note that in one dataset, all of these different cases are possible (given
##' different configurations for different years/countries).  This function
##' takes that into account and performs the appropriate approach for each
##' case.
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' 
##' @return This function returns a list (of length two) of integer vectors
##' containing the row numbers of observations which were updated (first by
##' updating missing values, second by retrending).  However, it also
##' has a side effect: rows within the passed data.table ("data") have
##' element 31/41/51's value(s) and symbol(s) updated.
##'  
##' @export
##' 

calculateEle314151 = function(aupusParam, data){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Create variables for referencing column names
    defineElementVariables(elements = c(31, 41, 51), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    ## Assign division factor based on item type
    data[, divisionFactor := 10000] # Default value
    data[get(itemTypeCol) %in% aupusGroups$sugarDivisonFactor,
         divisionFactor := 1]
    data[get(itemTypeCol) %in% aupusGroups$escrDivisionFactor,
         divisionFactor := 1000]

    ## Calculate condition statistics
    data[, numberOfMissingElements := numberOfMissingElement(.SD),
          .SDcols = c(element31Num, element41Num, element51Num)]
    data[, numberOfTrendingElements := numberOfTrendingElement(.SD),
          .SDcols = c(element31Symb, element41Symb, element51Symb)]
    data[, case := ifelse(numberOfMissingElements == 1, "replaceMissing",
                       ifelse(numberOfMissingElements == 0 &
                              numberOfTrendingElements > 0, "replaceTrended",
                                                            "doNothing"))]
    data[, numberOfMissingElements := NULL]
    data[, numberOfTrendingElements := NULL]

    ## Step 1: Only one value is missing.  In this case, replace it by
    ## balancing.
    data[case == "replaceMissing",
         c(element31Num, element41Num, element51Num, element31Symb,
           element41Symb, element51Symb) :=
             balanceProductionElements(value31 = get(element31Num),
                                       value41 = get(element41Num),
                                       value51 = get(element51Num),
                                       symb31 = get(element31Symb), 
                                       symb41 = get(element41Symb),
                                       symb51 = get(element51Symb),
                                       divisionFactor = divisionFactor)]
    
    ## Step 2: No values are missing.  In this case, we recompute any trended
    ## values.  First, we balance if only one element needs to be "retrended".
    ## Then, we trend element 31 and attempt to balance.  If we still cannot
    ## balance, we trend element 41, and finally we balance.
    data[case == "replaceTrended" & replaceable(get(element31Symb)),
         c(element31Num) := NA]
    data[case == "replaceTrended" & replaceable(get(element31Symb)),
         c(element31Symb) := "M"]
    data[case == "replaceTrended" & replaceable(get(element41Symb)),
         c(element41Num) := NA]
    data[case == "replaceTrended" & replaceable(get(element41Symb)),
         c(element41Symb) := "M"]
    data[case == "replaceTrended" & replaceable(get(element51Symb)),
         c(element51Num) := NA]
    data[case == "replaceTrended" & replaceable(get(element51Symb)),
         c(element51Symb) := "M"]

    ## Step 2a: Balance if possible
    data[case == "replaceTrended",
         c(element31Num, element41Num, element51Num, element31Symb,
           element41Symb, element51Symb) :=
             balanceProductionElements(value31 = get(element31Num),
                                       value41 = get(element41Num),
                                       value51 = get(element51Num),
                                       symb31 = get(element31Symb), 
                                       symb41 = get(element41Symb),
                                       symb51 = get(element51Symb),
                                       divisionFactor = divisionFactor)]
    
    ## Step 2b: Recompute element 31, if necessary.  Note that we can easily
    ## check if 31 needs to be recomputed based on if it's NA.  This is because
    ## if it was originally NA (before calculateEle314151 was called) then case
    ## would be "replaceTrended".  However, case is "replaceTrended", and so
    ## element 31 must have been set to NA in the beginning of the Step 2.
    data[case == "replaceTrended" & is.na(get(element31Num)),
         c(element31Num, element31Symb) :=
            trendOnce(Num = get(element31Num), Symb = get(element31Symb))]

    ## Step 2c: Again attempt to rebalance
    data[case == "replaceTrended",
         c(element31Num, element41Num, element51Num, element31Symb,
           element41Symb, element51Symb) :=
             balanceProductionElements(value31 = get(element31Num),
                                       value41 = get(element41Num),
                                       value51 = get(element51Num),
                                       symb31 = get(element31Symb), 
                                       symb41 = get(element41Symb),
                                       symb51 = get(element51Symb),
                                       divisionFactor = divisionFactor)]
    
    ## Step 2d: Recompute element 41, if necessary.  The same logic as for step
    ## 2b applies: we just must check if element41Num is NA.
    data[case == "replaceTrended" & is.na(get(element41Num)),
         c(element41Num, element41Symb) :=
            trendOnce(Num = get(element41Num), Symb = get(element41Symb))]
    
    ## Step 2e: Everything should be imputed for last rebalancing
    data[case == "replaceTrended",
         c(element31Num, element41Num, element51Num, element31Symb,
           element41Symb, element51Symb) :=
             balanceProductionElements(value31 = get(element31Num),
                                       value41 = get(element41Num),
                                       value51 = get(element51Num),
                                       symb31 = get(element31Symb), 
                                       symb41 = get(element41Symb),
                                       symb51 = get(element51Symb),
                                       divisionFactor = divisionFactor)]
    
    ## Remove unneeded columns
    replaced = data[, case]
    data[, c("divisionFactor", "case") := NULL]
    return(list(which(replaced == "replaceMissing"),
                which(replaced == "replaceTrended")))
}