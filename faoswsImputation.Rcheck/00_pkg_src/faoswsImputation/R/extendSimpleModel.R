##' Extend Simple Model
##'
##' This function takes any model which is designed to run on a simple time
##' series (i.e. at the commodity-country level) and applies it to each country
##' in turn, thus making it a commodity level model.
##'
##' @param data The data.table object containing the data.
##' @param model The model to be applied to each individual time series.  
##' Typically, this will be a function such as one from allDefaultModels().
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

extendSimpleModel = function(data, model, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    impName = imputationParameters$imputationValueColumn
    missingIndex = is.na(data[, get(impName)])
    modelFit = data[,
        # Apply the model if there is a missing value.  Else, return the data
        if(any(is.na(get(impName)))){
            # If all NA's are returned, sometimes type is logical.  Avoid this
            # by using the as.numeric below
            as.numeric(model(get(impName)))
        } else {
            get(impName)
        }, by = c(imputationParameters$byKey)]
    return(modelFit$V1)
}
