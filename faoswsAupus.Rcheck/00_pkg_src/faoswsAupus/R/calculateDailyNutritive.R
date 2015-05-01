##' Calculate Daily Nutritive (Calories, Proteins, and Fats)
##' 
##' This function converts total calories/proteins/fats to daily per person
##' calories/proteins/fats.  The calculation for each case is identical,
##' and thus the logic is implemented in this one function.  The calculation
##' is straightforward, although there is a multiplicative adjustment of 1000:
##' E_264 = E_261 * 1000 / (365 * Population)
##' E_274 = E_271 * 1000 / (365 * Population)
##' E_284 = E_281 * 1000 / (365 * Population)
##' (261/264 is calories/calories per day, 271/274 is for proteins, 281/284 is
##' for fats).
##' 
##' Population is extracted from the population commodity's element 21 (FBS
##' population).  If this element is missing, it uses the population
##' commodity's element 11 (initial existence).
##' 
##' Note: this function is a replacement for calculateEle264, calculateEle274,
##' and calculateEle284.
##' 
##' @param population11Num The column corresponds to element 11 of the
##' population.
##' @param population21Num The column corresponds to element 21 of the
##' population.
##' @param dailyElement The element number for the daily element.  Currently,
##' this should be one of 264, 274, or 284.
##' @param totalElement The element number for the total element.  Currently,
##' this should be one of 261, 271, or 281.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 264/274/284's value
##' and symbol updated.
##' 
##' @export
##' 

calculateDailyNutritive = function(population11Num, population21Num, dailyElement,
                           totalElement, data, aupusParam){

    ## Data Quality Checks
    stopifnot(population11Num %in% colnames(data))
    stopifnot(population21Num %in% colnames(data))
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(dailyElement %in% c(264, 274, 284))
    stopifnot(totalElement %in% c(261, 271, 281))
    if(dailyElement != totalElement + 3)
        warning("dailyElement != totalElement +3.  Are you sure you have the ",
                "correct element numbers?")
    
    ## Define column name variables
    dailyColValue = paste0(aupusParam$keyNames$valuePrefix,
                           aupusParam$keyNames$elementName, "_", dailyElement)
    dailyColSymb = paste0(aupusParam$keyNames$flagPrefix,
                           aupusParam$keyNames$elementName, "_", dailyElement)
    totalColValue = paste0(aupusParam$keyNames$valuePrefix,
                           aupusParam$keyNames$elementName, "_", totalElement)
    totalColSymb = paste0(aupusParam$keyNames$flagPrefix,
                           aupusParam$keyNames$elementName, "_", totalElement)
    itemTypeCol = aupusParam$keyNames$itemTypeName

    data[, validPopulation := get(population21Num)]
    data[is.na(validPopulation), validPopulation := get(population11Num)]
    replaceIndex1 = data[, replaceable(get(dailyColSymb))]
    data[replaceIndex1,
         `:=`(c(dailyColValue, dailyColSymb),
              appendSymbol(get(totalColValue) / 365 *
                               computeRatio(1000, validPopulation),
                           "C"))]
    data[, validPopulation := NULL]
    
    which(replaceIndex1)
}
