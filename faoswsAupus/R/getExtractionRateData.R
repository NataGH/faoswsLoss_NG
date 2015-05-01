##' Get Extraction Rate Data
##' 
##' This function extracts the extraction rate data from the aupus data by
##' grabbing the key columns of aupusData as well as the column corresponding
##' to extraction rate (as specified by element41Num).
##' 
##' Note: the old function queried the data.base for this data, but this new,
##' simpler function just pulls directly from a table that has already been
##' loaded into R.
##'
##' @param aupusData The data returned from the function getAupusData
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A data.table with the same key columns as aupusData and 
##' additional columns "Value_extraction" and "flagFaostat_extraction".
##' This are the columns of aupusData corresponding to element 41.
##' 
##' @export
##' 

getExtractionRateData = function(aupusData, aupusParam){
    
    ## Data Quality Checks
    stopifnot(is(aupusData, "data.table"))
    stopifnot(key(aupusData) ==
                  c("geographicAreaFS", "measuredItemFS", "timePointYearsSP"))
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(41, aupusParam)
    
    ## This is to get the extraction rate data for the edge
    extractionRateData =
        aupusData[!is.na(get(element41Num)),
                  c(key(aupusData), element41Num, element41Symb), with = FALSE]
    setkeyv(extractionRateData, cols = key(aupusData))
    setnames(x = extractionRateData,
             old = c("measuredItemFS", element41Num, element41Symb),
             new = c("measuredItemChildFS", "Value_extraction",
                     "flagFaostat_extraction"))
    extractionRateData
}
