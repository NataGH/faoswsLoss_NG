##' Get Ensemble Weights via Inverse Errors
##' 
##' Weights used to construct the final ensemble from the individual models are
##' computed by comparing the errors.  For each model, the errors are computed 
##' (simply comparing observed values in data to fitted values in fits) and
##' then the inverse (i.e. 1/) these errors are used as the weights (after
##' rescaling to 1).  For example, if three models have errors of 1, 2, and 3
##' then the inverse errors are 1, 1/2, and 1/3.  We rescale these weights to
##' add to 1 and get 6/11, 3/11, and 2/11.
##' 
##' @param data The data object containing the observations to impute.
##' @param fits A list of the fitted values.  These may be estimated via leave
##' one out cross-validation or directly.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A data.table containing the weight for each model within each byKey
##' group, as well as a few other (currently unused) statistics.
##' 
##' @export
##' 

getInverseWeights = function(data, fits, imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    stopifnot(nrow(data) == sapply(fits, length))

    ### Compute errors from each model
    error = lapply(1:length(fits),
           FUN = function(i){
               out = computeErrorRate(data = data, fit = fits[[i]],
                                imputationParameters = imputationParameters)
               out = data.table(model = names(fits)[i],
                                data[, c(imputationParameters$byKey), with = FALSE],
                                missingValue = is.na(
                                    data[[imputationParameters$imputationValueColumn]]),
                                year = data[[imputationParameters$yearValue]],
                                error = out,
                                fit = fits[[i]])
           })
    error = do.call("rbind", error)
    # Sometimes, a model will fail in the leave-out-one cross-validation.  If
    # that happens, we'll get a missing value (NA) for that error.  To work
    # around this, assign that NA to the highest error for that observation.
    # In other words, assumme the model that failed did as poor as possible.
    error[, error := ifelse(is.na(error), max(error, na.rm = TRUE), error),
          by = c(imputationParameters$byKey, "year")]
    ## Errors should never be infinity, so this signals some sort of problem:
    if(error[!(missingValue), max(abs(error))] == Inf){
        badKeys = error[!(missingValue) & abs(error) == Inf,
                        imputationParameters$byKey, with = FALSE]
        badKeys = unique.data.frame(badKeys)
        warning("Model fitting failed for keys: ", paste(badKeys, collapse=" "),
            "\nSome countries may have only one observation, and thus no",
            "cross-validation models worked.  Have you tried a global model",
            "like defaultMixedModel()?  Or, including a simple model like",
            "defaultMean() (which should fit every dataset well) might work.",
            "Or, you could also try errorType = 'raw' (although loocv is", 
            "generally preferred).")
    }
    # If the fit failed, we can't use this model.  Assign error of Inf.  To
    # determine if a model failed, we can check if a fitted value is NULL (but
    # we must exclude values which are missing, as loocv won't estimate those
    # and they won't influence error calcs anyways).
    error[, modelFailed := any(is.na(fit) & !missingValue),
          by = c(imputationParameters$byKey, "model")]
    error[(modelFailed), error := NA]
    
    ### Create the weights data.table using the errors
    # Aggregate the errors using the provided error function, applying to each
    # byKey group and model individually.
    weights = error[!(missingValue), imputationParameters$errorFunction(error),
                    by = c(imputationParameters$byKey, "model")]
    setnames(weights, old = "V1", new = "averageError")
    if(imputationParameters$errorType == "raw"){
        # Perfect fits can give really small errors but actually be overfitting
        # the data.  To prevent that, set small errors to the mean error.  But,
        # if all errors are less than 1e-3, change weights to uniform (in this
        # case, averageErrorByKey will be NA).
        ## NOTE (Michael): Maybe change this to uniform weight
        weights[, averageErrorByKey :=
                    mean(averageError[averageError > 1e-3], na.rm = TRUE),
                by = c(imputationParameters$byKey)]
        weights[, averageError := ifelse(averageError < 1e-3 &
                !is.na(averageErrorByKey), averageErrorByKey,
            averageError), by = c(imputationParameters$byKey)]
        ## If we still haven't corrected averageError, then all weights for a
        ## particular key are < 1e-3.  In this case, assign errors of 1 to all.
        weights[, averageError := ifelse(averageError < 1e-3,
                                         1, averageError)]
    } else if(imputationParameters$errorType == "loocv"){
        # Really small errors will cause 1/error^2 to be Inf, and this
        # gives weights of all 0, NA, NaN.  Prevent that by limiting how
        # small the errors can be:
        weights[, averageErrorByKey :=
                    mean(averageError[averageError > 1e-16], na.rm = TRUE),
                by = c(imputationParameters$byKey)]
        weights[, averageError := ifelse(averageError < 1e-16 &
                !is.na(averageErrorByKey), averageErrorByKey,
            averageError), by = c(imputationParameters$byKey)]
        ## If we still haven't corrected averageError, then all weights for a
        ## particular key are < 1e-16.  In this case, assign the same errors to
        ## all.
        weights[, averageError := ifelse(averageError < 1e-16,
                                         1e-16, averageError)]
    }
    weights[, weight := (1/averageError^2) /
                sum(1/averageError^2, na.rm = TRUE),
            by = c(imputationParameters$byKey)]
    return(weights)
}