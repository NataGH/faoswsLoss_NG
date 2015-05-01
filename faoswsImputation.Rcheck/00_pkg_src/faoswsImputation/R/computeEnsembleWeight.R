##' Function to compute the weights of the ensemble models
##'
##' @param data A data.table containing the data.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param fits The fitted values from the models.
##' @param method Must be either "inverse" or "stacking".  If "inverse", the
##' final ensemble is a weighted average of all the individual models, where
##' the weight of each model is proportional to 1/error from that model.  If
##' "stacking", then the weight is assigned via a linear regression (where the
##' independent variable in the regression is the variable being imputed, and
##' each individual model is a dependent variable).  The linear regression is
##' restricted, however: no weights may be negative and the weights must sum to
##' one.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @export
##' 

computeEnsembleWeight = function(data, cvGroup, fits, method = "inverse",
                                 imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    if(!all(lapply(fits, length) == nrow(data)))
        stop("All elements of fits must have the same length as nrow(x)!")
    stopifnot(all(names(fits) == names(imputationParameters$ensembleModels)))
    if(is.null(names(fits)))
        names(fits) = paste("Model", 1:length(fits), sep="_")
    counts = data[,
                  sum(!is.na(get(imputationParameters$imputationValueColumn))),
                  by = c(imputationParameters$byKey)]
    if(min(counts[, V1]) == 0 & !imputationParameters$estimateNoData)
        stop("Some countries have no data.  Have you ran removeNoInfo?")
    
    ### If doing loocv, compute a new fits object
    if(imputationParameters$errorType == "loocv")
        fits = computeLoocvFits(data = data, cvGroup = cvGroup,
                                imputationParameters = imputationParameters)
    
    ### Compute the actual weights by passing to another function
    if(method == "inverse"){
        weights = getInverseWeights(data = data, fits = fits,
                    imputationParameters = imputationParameters)
    } else if(method == "stacking"){
        weights = getStackingWeights(data = data, fits = fits,
                    imputationParameters = imputationParameters)
    } else {
        stop("Provide method is not currently implemented!")
    }
    
    ## Use global cross-validation rate for slices with no data.  Some slices
    ## of the data may have no observations, and thus we can't compute error to
    ## weight the ensemble.  Instead, we can weight by the total average cross-
    ## validation error for these cases.  But, only do this if
    ## imputationParameters$estimateNoData is TRUE.
    if(imputationParameters$estimateNoData){
        newRows = addNoDataModels(data = data, weights = weights,
                                  imputationParameters = imputationParameters)
        weights = rbind(weights, newRows)
    }
    
    ### Apply adjustments to weights (NA->0, reduce below maximumWeights)
    weights[is.na(weight), weight := 0]
    maxWt = imputationParameters$maximumWeights
    # Assign weights exceeding the threshold a new value.  We want this new
    # value to be maximumWeights, but after reassigning this weight we have
    # to re-normalize the weights so they sum to 1.  The (1-weight)/(1-max)
    # factor ensures the final weight for this maximum case will be
    # maximumWeights.
    weights[weight > maxWt, weight := maxWt * (1 - weight) / (1 - maxWt)]
    # If one area only has one non-missing weight, set to 1:
    weights[, validModelCnt := sum(!is.na(averageError)),
             by = c(imputationParameters$byKey)]
    # If there's only one valid model, it should have all the weight.
    weights[validModelCnt == 1 & !is.na(weight), weight := 1]
    # If there's no valid models, all should have 0 weight.
    weights[validModelCnt == 0, weight := 0]
    weights[, validModelCnt := NULL]
    # Re-normalize the weights so they sum to 1:
    weights[, weight := weight / sum(weight),
             by = c(imputationParameters$byKey)]
    
    ### Convert weights to a matrix
    weights = getWeightMatrix(data = data, w = weights,
                              imputationParameters = imputationParameters)
    weights
}
