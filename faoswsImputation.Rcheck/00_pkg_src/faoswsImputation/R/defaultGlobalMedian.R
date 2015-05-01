##' Global Median Model for Imputation
##'
##' This function imputes missing values through a global median.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' Otherwise, the mean of all available observations is returned (computed
##' across the entire dataset).
##' 
##' @export
##' 

defaultGlobalMedian = function(data, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    modelFit = median(data[[imputationParameters$imputationValueColumn]],
                      na.rm = TRUE)
    modelFit = rep(modelFit, nrow(data))
    return(modelFit)
}
