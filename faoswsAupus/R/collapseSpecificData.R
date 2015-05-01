##' Collapse Specific Data
##' 
##' Certain datasets are extracted from the database in three chunks:
##' country/year specific data, country specific data, and generic data
##' (applying to all countries/years).  The most specific data available
##' should be used when available, otherwise a more general value should
##' be used.  This function collapses the three different datasets
##' (supplied in listData) into one final dataset.  Note that all three
##' datasets should have the same columns except for the key columns which
##' provide the level of detail (i.e. item, item/country, and
##' item/country/year).
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param listData A list of three data.tables, typically as obtained from the
##' functions getShare, getBalanceElementData, or getRatioData.  This
##' list is of length 3 with data.tables as elements (specific, yearWildCard,
##' and areaYearWildCard).  Each data.table contains information at a
##' different level of specificity.
##' @param verbose Whether the output should be printed.
##' 
##' @return Returns a single data.table derived by condensing the listData
##' list into one dataset.
##' 
##' @export
##' 

collapseSpecificData = function(aupusParam, listData, verbose = FALSE){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(verbose, "logical"))
    stopifnot(names(listData) == c("specific", "yearWildCard",
                                   "areaYearWildCard"))
    
    ## Make sure the column types are all correct
    listData = lapply(listData, coerceColumnTypes, aupusParam = aupusParam)
    
    allKeys = sapply(listData, key)
    ## Get the keys common to all data.tables
    baseKey = Reduce(intersect, allKeys)
    uniquePath = unique.data.frame(Reduce(rbind, lapply(listData, 
        FUN = function(x){
            x[, baseKey, with = FALSE]
        })))
    uniqueYear = as.numeric(aupusParam$year)
    uniqueArea = aupusParam$areaCode

    ## Create the final data.table by combining all possible values
    tmp = lapply(uniquePath, rep, times = length(uniqueYear))
    tmp[[aupusParam$keyNames$yearName]] = rep(uniqueYear, each = NROW(uniquePath))
    tmp[[aupusParam$keyNames$areaName]] = as.numeric(uniqueArea)
    finalBase = as.data.table(tmp)
    setkeyv(finalBase, key(listData$specific))
    dataColumns = setdiff(colnames(listData[["areaYearWildCard"]]), baseKey)
    finalBase[, c(dataColumns) := NA_real_]
    for(columnName in dataColumns){
        for(datasetName in c("specific", "yearWildCard", "areaYearWildCard")) {
            wildCardFill(originalData = finalBase,
                         wildCardData = listData[[datasetName]],
                         variable = columnName, verbose = verbose)
        }
    }
    finalBase[, `:=`(timePointYearsSP, as.numeric(timePointYearsSP))]
    
    ## Filter out rows that are all NA
    filter = finalBase[, dataColumns, with = FALSE]
    filter = apply(is.na(filter), 1, all)
    setkeyv(finalBase, key(listData$specific))
    finalBase[!filter, ]
}
