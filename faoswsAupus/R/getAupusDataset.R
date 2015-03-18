##' Get AUPUS Data
##' 
##' This function obtains all required data for the AUPUS module by running
##' several individual functions, such as getRatioData, getShareData, etc.  The
##' datasets returned by these individual functions are then returned from this
##' function in a list (if assignGlobal = FALSE) or are assigned to the global
##' environment (if assignGlobal = TRUE).
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param assignGlobal Whether the data should be assigned globally (TRUE)
##' or returned as a list (FALSE). Defaults to FALSE.
##' 
##' @return If assignGlobal = FALSE, then a list is returned with names
##' "aupusData", "inputData", "ratioData", "shareData", "balanceElementData",
##' "itemInfoData", "populationData", and "extractionRateData".  Each of these
##' elements is a data.table containing information for the AUPUS processing.
##' If assignGlobal = TRUE, then these data.tables are assigned to the global
##' environment and no data is returned.
##' 
##' @export
##' 

getAupusDataset = function(aupusParam, assignGlobal = FALSE){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(assignGlobal, "logical"))
    
    ## Get aupus data
    ## ----------------------------------------------------------------
    aupusData = getAupusData(aupusParam = aupusParam, database = "new")
    if(nrow(aupusData) == 0){
        warning("No data available for this area code")
        return(NULL)
    }

    ## Get input from processing data
    ## ----------------------------------------------------------------
    inputData = getInputFromProcessData(aupusParam = aupusParam, database = "new")

    ## Get ratio data
    ## ----------------------------------------------------------------
    ratioData = 
        collapseSpecificData(
            aupusParam = aupusParam,
            listData = getRatioData(aupusParam = aupusParam, database = "new"))

    ## Get share data
    ## ----------------------------------------------------------------
    shareData =
        collapseSpecificData(
            aupusParam = aupusParam,
            listData = getShareData(aupusParam = aupusParam, database = "new"))
    ## Remove any cases where the share is 0, as these aren't really shares
    shareData = shareData[Value_share != 0, ]

    ## Get balancing item
    ##
    ## Note (Michael): This is just the flag of the 'ratio' table
    ## ----------------------------------------------------------------
    balanceElementData =
        collapseSpecificData(
            aupusParam = aupusParam,
            listData = getBalanceElementData(aupusParam = aupusParam, database = "new"))

    ## Get item information table
    ## ---------------------------------------------------------------
    itemInfoData = getItemInfoData()

    ## Get population data
    populationData = getPopulationData(aupusParam = aupusParam, database = "new")

    ## Get extraction rate
    extractionRateData = getExtractionRateData(aupusData = aupusData,
                                               aupusParam = aupusParam)

    ## Return the data
    dataList =
        list(aupusData = aupusData, inputData = inputData,
             ratioData = ratioData, shareData = shareData,
             balanceElementData = balanceElementData,
             itemInfoData = itemInfoData, populationData = populationData,
             extractionRateData = extractionRateData)
    
    ## Coerce column types
    warning("This step should no longer be necesssary upon resolution of ",
            "SWS-797")
    lapply(dataList, coerceColumnTypes, aupusParam = aupusParam)
    
    if(assignGlobal){
        lapply(names(dataList), FUN = function(x)
            assign(x, dataList[[x]], envir = .GlobalEnv))
        invisible(dataList)
    } else {
        return(dataList)
    }
}


