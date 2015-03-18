##' Get AUPUS Parameters
##' 
##' This function gets all the parameters in order to query the data, and
##' either assigns these parameters to the global environment of returns them
##' as a list.
##'
##' @param areaCode A character value giving the country code of the country of
##' interest.
##' @param assignGlobal logical, default to FALSE, in which case a list is
##' returned. If TRUE, then the result will be assigned globally.
##' @param yearsToUse If NULL, then all available years (as determined by
##' GetCodeList) will be used.  Otherwise, a vector of years should be provided
##' and these years alone will be included.
##' 
##' @return If assignGlobal is TRUE, nothing is returned but 5 objects are
##' written to the global environment: areaCode, itemCode, elementCode, year,
##' and keyNames.  The first four of these variables each define a dimension
##' for slicing the data, and the fifth element is a list of arguments to
##' processing functions.  If assignGlobal is false, these 5 objects are
##' returned in a named list.
##' 
##' @export
##' 

getAupusParameter = function(areaCode, assignGlobal = TRUE, yearsToUse = NULL){
    
    ## Data Quality Checks
    checkCode = GetCodeList(domain = "faostat_one",
                            dataset = "FS1_SUA",
                            dimension = "geographicAreaFS",
                            codes = areaCode)
    if(nrow(checkCode) < 1)
        stop("No valid codes found!")
    stopifnot(length(areaCode) == 1)
    stopifnot(is(assignGlobal, "logical"))
    if(!is.null(yearsToUse)){
        stopifnot(is(yearsToUse, "numeric"))
        stopifnot(min(yearsToUse) >= 1960)
        stopifnot(max(yearsToUse) <= 2020) # May eventually need updating
    }

    ## Get all item Code
    ## --------------------------------------------------------------
    itemCodeList =
        GetCodeList(domain = "faostat_one",
                    dataset = "FS1_SUA",
                    dimension = "measuredItemFS")
    allItemCodes = itemCodeList[type != 0, code]

    ## Get all year
    if(is.null(yearsToUse)){
        yearCodeList =
            GetCodeList(domain = "faostat_one",
                        dataset = "FS1_SUA",
                        dimension = "timePointYears")
        allYearCodes = yearCodeList[description != "wildcard", code]
    } else {
        allYearCodes = as.character(yearsToUse)
    }

    ## Get all aupus element
    ## --------------------------------------------------------------
    ## TODO (Michael): Need to add back element 541 and 546, when Nick
    ##                 import then into the data base.
    allElementCodes =
        as.character(c(11, 21, 31, 41, 51, 58, 61, 62, 63, 66, 71, 91,
                       92, 93, 95, 96, 101, 111, 121, 131, 141, 144,
                       151, 161, 171, 174, 181, 191, 261, 264, 271,
                       274, 281, 284))

    ## Set key names
    ## --------------------------------------------------------------
    keyNames =
        list(areaName = "geographicAreaFS",
             itemName = "measuredItemFS",
             itemParentName = "measuredItemParentFS",
             itemChildName = "measuredItemChildFS",
             itemTypeName = "measuredItemTypeFS",
             elementName = "measuredElementFS",
             extractionRateName = "Value_extractionRate",
             balanceElementName = "Value_balanceElement",
             inputName = "Value_input",
             shareName = "Value_share",
             yearName = "timePointYearsSP",
             valuePrefix = "Value_",
             flagPrefix = "flagFaostat_",
             ratioPrefix = "Ratio_")

        
    tmp = list(areaCode = areaCode,
        itemCode = allItemCodes, elementCode = allElementCodes,
        year = allYearCodes, keyNames = keyNames)
    if(assignGlobal){
        lapply(names(tmp), FUN = function(x)
            assign(x, tmp[[x]], envir = .GlobalEnv))
        invisible(tmp)
    } else {
        return(tmp)
    }
}
