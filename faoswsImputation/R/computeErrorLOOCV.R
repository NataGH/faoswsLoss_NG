##' LOOCV Error Rate
##' 
##' This function computes the error of different model fits via leave-one-out
##' cross-validation.  However, typically this function will be called via
##' computeErrorRate and not directly.
##'
##' @param data A data.table containing the data.
##' @param model The model fit to x, should be of class ensembleModel.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @export

computeErrorLOOCV = function(data, model, cvGroup, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    stopifnot(is(model, "ensembleModel"))
    stopifnot(is.numeric(cvGroup))
    stopifnot(length(cvGroup)==nrow(data))
    stopifnot(length(unique(cvGroup)) > 1)

    error = rep(0, nrow(data))
    for(i in 1:length(unique(cvGroup))){
        #Copy x and remove the ith observation to fit the model
        dataTemporary = copy(data)
        setnames(dataTemporary,
                 old = imputationParameters$imputationValueColumn,
                 new = "imputationValueColumn")
        dataTemporary[cvGroup == i, imputationValueColumn := NA]
        setnames(dataTemporary, old = "imputationValueColumn",
                 new = imputationParameters$imputationValueColumn)
        if(model@level == "commodity"){
            fitTemporary = model@model(data = dataTemporary)
        } else if(model@level == "countryCommodity"){
            fitTemporary = extendSimpleModel(data = dataTemporary,
                model = model@model,
                imputationParameters = imputationParameters)
        }
        filter = !is.na(cvGroup) & cvGroup == i
        error[filter] = (data[[imputationParameters$imputationValueColumn]]
                         - fitTemporary)[filter]
    }
    return(error)
}
