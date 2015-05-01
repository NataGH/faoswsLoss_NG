##' Calculate Element 541 (Final Demand)
##' 
##' Element 541 (Final Demand) is computed as a summation of elements 542, 543,
##' 544, and 545.  If some of those values are missing, they are ignored in
##' the calculation.  However, if all four of the values are missing, then the
##' resulting calculation returns a 0M for element 541.
##' 
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.
##' However, it also has a side effect: rows within the passed
##' data.table ("data") have element 541's value and symbol updated.
##' Additionally, columns will be added to the passed data.table if they don't
##' currently exist: Value_measuredElementFS_54X, where X = 2, 3, 4, 5.
##' 
##' @export
##' 

calculateEle541 = function(data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(541, 542, 543, 544, 545),
                           aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    ## If any of these elements were missing, then assign the column
    ## with NA
    finalDemandElements =
        c(element542Num, element543Num, element544Num, element545Num)
    missingElements = !finalDemandElements %in% colnames(data)
    if(any(missingElements))
        data[, `:=`(finalDemandElements[missingElements],
                    as.numeric(NA))]
    
    data[, newSummation := rowSums(.SD, na.rm = TRUE),
         .SDcols = c(element542Num, element543Num,
                     element544Num, element545Num)]
    data[, numberOfMissingElements :=
             numberOfMissingElement(get(element542Num), get(element543Num),
                                    get(element544Num), get(element545Num))]
    replaceIndex1 = data[, replaceable(get(element541Symb), newSummation)]
    data[replaceIndex1,
         `:=`(c(element541Num, element541Symb),
              appendSymbol(newSummation, "C"))]
    replaceIndex2 = data[, numberOfMissingElements == 4 &
                             replaceable(get(element541Symb), newSummation)]
    data[replaceIndex2,
         `:=`(c(element541Num, element541Symb), list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]
    data[, newSummation := NULL]
    
    which(replaceIndex1)
}
