##' Function to compute the fits of all the component models
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return Returns a list of vectors of the same length as ensembleModels. The
##' ith element of the list represents the fit of the ith model to data.
##' 
##' @export
##' 

computeEnsembleFit = function(data, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    ### Fit Models
    fits = lapply(imputationParameters$ensembleModels,
        FUN = function(model){
            if(model@level == "commodity"){
                model@model(data = data,
                            imputationParameters = imputationParameters)
            } else if(model@level == "countryCommodity"){
                extendSimpleModel(data = data, model = model@model,
                                  imputationParameters = imputationParameters)
            }
        })
    return(fits)
}