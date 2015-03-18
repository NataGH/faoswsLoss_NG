##' Function to compute the error of different model fits
##'
##' @param data A data.table containing the data.
##' @param fit The fitted value from the model.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A vector of (the absolute value of the) errors corresponding to the
##' non-missing entries in data's "value" column.
##' 
##' @export
##' 

computeErrorRate = function(data, fit, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    ### Run the function:
    x = data[[imputationParameters$imputationValueColumn]]
    if(all(is.na(x - fit))){
        er = rep(NA, length.out = nrow(data))
    } else {
        er = (x - fit)
    }
    abs(er)
}
