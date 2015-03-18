##' Function to compute the fitted values from Leave One Out Cross Validation
##'
##' @param data A data.table containing the data.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A list of the same length as ensembleModels which contains the
##' fitted values corresponding to the loocv procedure.  The ith element of the
##' list corresponds to the fitting of the ith element of ensembleModels.
##' 
##' @export
##' 

computeLoocvFits = function(data, cvGroup, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    stopifnot(length(cvGroup) == nrow(data))
    
    ensemModels = imputationParameters$ensembleModels
    fits = lapply(1:length(ensemModels), FUN = function(i){
        model = ensemModels[[i]]
        fit = rep(NA, nrow(data))
        for(j in unique(cvGroup[!is.na(cvGroup)])){
            #Copy x and remove the ith observation to fit the model
            dataTemporary = copy(data)
            dataTemporary[cvGroup == j,
                          c(imputationParameters$imputationValueColumn) := NA]
            if(model@level == "commodity"){
                fitTemporary = model@model(data = dataTemporary,
                                           imputationParameters =
                                               imputationParameters)
            } else if(model@level == "countryCommodity"){
                fitTemporary = extendSimpleModel(data = dataTemporary,
                                                 model = model@model,
                                                 imputationParameters =
                                                     imputationParameters)
            }
            filter = !is.na(cvGroup) & cvGroup == j
            fit[filter] = fitTemporary[filter]
        }
        return(fit)
    })
    names(fits) = names(ensemModels)
    return(fits)
}
