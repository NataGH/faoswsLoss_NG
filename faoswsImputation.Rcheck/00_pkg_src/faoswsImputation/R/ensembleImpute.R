##' Function to perform ensemble imputation
##'
##' This is an implementation of the ensemble imputation methodology
##' developed for the FAO production domain.
##'
##' @param data A data.table containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##'
##' @export
##' 

ensembleImpute = function(data, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    valueMissingIndex = is.na(
        data[[imputationParameters$imputationValueColumn]])
    flagMissingIndex = (data[[imputationParameters$imputationFlagColumn]] ==
                            imputationParameters$missingFlag)
    # Ensure missing values agree with missing flags
    if(!all(valueMissingIndex == flagMissingIndex)){
        cat("Values that are NA: ", sum(valueMissingIndex), "\n")
        cat("Flags with missingFlag value: ", sum(flagMissingIndex), "\n")
        stop("Different missing values from flags/values!  Maybe call remove0M?")
    }
    if(is.null(names(imputationParameters$ensembleModels)))
        names(imputationParameters$ensembleModels) = paste(
            "Model", 1:length(imputationParameters$ensembleModels), sep = "_")
    if(!any(is.na(data[[imputationParameters$imputationValueColumn]]))){
        warning("No missing values in data[[imputationValueColumn]].",
        "Returning data[[imputationValueColumn]]")
        return(data[[imputationParameters$imputationValueColumn]])
    }
    ## Order data by byKey and then by year
    setkeyv(x = data, cols = c(imputationParameters$byKey,
                               imputationParameters$yearValue))
    
    ensemble = data[[imputationParameters$imputationValueColumn]]
    missIndex = is.na(ensemble)
    cvGroup = makeCvGroup(data = data,
                          imputationParameters = imputationParameters)
    modelFits = computeEnsembleFit(data = data,
                                   imputationParameters = imputationParameters)
    modelWeights = computeEnsembleWeight(data = data,
        cvGroup = cvGroup, fits = modelFits,
        imputationParameters = imputationParameters)
    ## print(modelWeights)
    ensembleFit = computeEnsemble(modelFits, modelWeights)
    ensemble[missIndex] = ensembleFit[missIndex]
    if(imputationParameters$plotImputation){
        plotEnsemble(data = data, modelFits = modelFits,
                     modelWeights = modelWeights, ensemble = ensemble,
                     imputationParameters = imputationParameters)
#         plotEnsembleOld(data, modelFits, modelWeights, ensemble)
    }
    ensemble
}
