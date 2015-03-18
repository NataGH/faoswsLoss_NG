##' Collapse Share
##' 
##' Share data is extracted from the database in three chunks: country/year
##' specific data, country specific data, and generic data (applying to all
##' countries/years).  The most specific data available should be used when
##' available, otherwise a more general value should be used.  This function
##' collapses the three different datasets (supplied in shareData) into one
##' final dataset.
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param shareData The shares data obtained from the function getShare.  This
##' is a list of length 3 with data.tables as elements (specific, yearWildCard,
##' and areaYearWildCard).  Each data.table contains share information at a
##' different level of specificity.
##' @param shares The name of the column correspond to shares within each
##' element of shareData (note that it must be the same for each element).
##' @param verbose Whether the output should be printed.
##' 
##' @return Returns a single data.table derived by condensing the shareData
##' list into one dataset.
##' 
##' @export
##' 

collapseShare = function(aupusParam, shareData, shares, verbose = FALSE){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(verbose, "logical"))
    stopifnot(shares %in% colnames(shareData[[1]]))
    stopifnot(shares %in% colnames(shareData[[2]]))
    stopifnot(shares %in% colnames(shareData[[3]]))
    
    uniquePath = unique.data.frame(Reduce(rbind, lapply(shareData, 
        FUN = function(x){
            x[, c(aupusParam$keyNames$itemParentName, 
                  aupusParam$keyNames$itemChildName),
              with = FALSE]
        })))
    uniqueYear = as.numeric(aupusParam$year)
    uniqueArea = aupusParam$areaCode

    ## Create the final data.table by combining all possible values
    tmp = lapply(uniquePath, rep, times = length(uniqueYear))
    tmp[[aupusParam$keyNames$yearName]] = rep(uniqueYear, each = NROW(uniquePath))
    tmp[[aupusParam$keyNames$areaName]] = uniqueArea
    tmp[[shares]] = as.numeric(NA)
    finalBase = as.data.table(tmp)
    setkeyv(finalBase, key(shareData$specific))
    for(name in c("specific", "yearWildCard", "areaYearWildCard")) {
        wildCardFill(finalBase, shareData[[name]], shares, verbose)
    }
    setkeyv(finalBase, key(shareData$specific))
    finalBase[, `:=`(timePointYearsSP, as.numeric(timePointYearsSP))]
    finalBase[get(shares) != 0, ]
}
