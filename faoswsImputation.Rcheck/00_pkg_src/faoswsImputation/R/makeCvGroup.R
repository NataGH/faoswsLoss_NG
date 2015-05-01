##' Make Cross-Validation Groups
##' 
##' Creates a vector of cross-validation groups to be used for leave-one-out
##' cross-validation in later models.
##' 
##' @param data A data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A numeric vector taking values in 1:groupCount, or NA if the
##' corresponding observation is missing.  All groups are represented within
##' each individual timeseries (defined by byKey) when possible.
##' 
##' @export
##' 

makeCvGroup = function(data, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    cvGroup = rep(NA, nrow(data))
    impName = imputationParameters$imputationValueColumn
    cvGroup[!is.na(data[,get(impName)])] =
        data[!is.na(get(impName)),
             sampleEqually(n = .N, k = imputationParameters$groupCount),
             by = c(imputationParameters$byKey)]$V1
    return(cvGroup)
}