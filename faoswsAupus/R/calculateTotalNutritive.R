##' Calculate Total Nutritive (Calories, Proteins, Fats)
##' 
##' Each commodity has a ratio defined, R_261/R_271/R_281, which gives the
##' conversion rate of the quantity of the commodity in question into
##' calories/proteins/fats.  Thus, element 261/271/281 is computed simply as
##' E_261 = E_141 * R_261 / 100
##' 
##' E_271 = E_141 * R_271 / 1000
##' 
##' E_281 = E_141 * R_281 / 1000
##' 
##' @param ratioNum The column name of data corresponding to the ratio for this
##' element.  Typically one of "Ratio_measuredElementFS_261" for calories,
##' "Ratio_measuredElementFS_271" for proteins, or
##' "Ratio_measuredElementFS_261" for fats.
##' @param elementNum The element number.  Currently, this should be one of 261
##' (calories), 271 (proteins) or 281 (fats).
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 261/271/281's value
##' and symbol updated.
##' 
##' @export
##' 

calculateTotalNutritive = function(ratioNum, elementNum, data, aupusParam){

    ## Data Quality Checks
    stopifnot(ratioNum %in% colnames(data))
    stopifnot(elementNum %in% c(261, 271, 281))
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(141), aupusParam = aupusParam)
    elementValue = paste0(aupusParam$keyNames$valuePrefix,
                           aupusParam$keyNames$elementName, "_", elementNum)
    elementSymb = paste0(aupusParam$keyNames$flagPrefix,
                           aupusParam$keyNames$elementName, "_", elementNum)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    divisionFactor = ifelse(elementNum == 261, 100,
                            ifelse(elementNum %in% c(271, 281), 1000, NA))

    replaceIndex1 = data[, replaceable(get(elementSymb), get(ratioNum) *
                                         get(element141Num) / divisionFactor)]
    data[replaceIndex1,
         `:=`(c(elementValue, elementSymb),
              appendSymbol(get(ratioNum) * get(element141Num) /
                               divisionFactor, "C"))]
    which(replaceIndex1)
}
