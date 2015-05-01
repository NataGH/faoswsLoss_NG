##' Get Ensemble Weights via Stacking
##' 
##' Weights used to construct the final ensemble from the individual models are
##' computed via stacking.
##' 
##' Traditional stacking proceeds as follows: For each individual byKey
##' (usually country) a set of weights is chosen for the ensemble.  The weights
##' are constrained to be positive and to sum to 1, and the final ensemble is
##' constructed by sum(w_i*model_i).  The weights should be chosen in a way
##' such that better models are given more importance.  Thus, the following
##' criteria is minimized:
##' 
##' sum(errorFunction(|y - w_i*model_i|))
##' 
##' i.e. the errorFunction applied to the difference between the observed
##' values and the ensemble estimate.  The errorFunction is typically just x^2,
##' but could be a more complex function.
##' 
##' However, this is roughly equivalent to regression, with a constraint added.
##' In some cases, however, our datasets will be so sparse that we won't be
##' able to perform this optimization (only four observations and 7 valid
##' models, for example, will not have a unique solution).  Thus, we instead
##' use a LASSO regression for computing the stacking weights.
##' 
##' Note: if errorType is not "loocv", then stacking will be problematic: much
##' higher weights will be given to flexible models (such as loess or splines)
##' without valid reason.
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

getStackingWeights = function(data, fits, imputationParameters){
    
    stop("Stacking weights have not yet been implemented!")
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    stopifnot(nrow(data) == sapply(fits, length))

    stackingData = data.table(
        y = data[[imputationParameters$imputationValueColumn]],
        byKey = data[[imputationParameters$byKey]],
        do.call("cbind", fits))
    
    ### If a model is NA when y is not NA for some particular byKey value, then
    ### don't use that model for that byKey at all
    for(column in colnames(stackingData)){
        ## Don't do anything with the y or byKey columns, only the model ones
        if(column %in% c("y", "byKey"))
            next
        removeColumn = stackingData[!is.na(y), any(is.na(get(column))), by = byKey]
        removeColumn = removeColumn[(V1), byKey]
        stackingData[byKey %in% removeColumn, column := NA, with = FALSE]
    }
    
    ### Define model function to extract weights
    independentVariableCols = colnames(stackingData)[3:ncol(stackingData)]
    lassoModel = function(d){
        x = d[, independentVariableCols, with = FALSE]
        y = d[, y]
        filterObservations = is.na(y)
        ### Coerce to form required by glmnet
        y = y[!filterObservations]
        x = as.matrix(x[!filterObservations, ])
        filterColumns = !apply(x, 2, function(y) any(is.na(y)))
        x = x[, filterColumns]
        fit = cv.glmnet(x = x, y = y, intercept = FALSE, upper.limits = 1,
                        lower.limits = 0)
        bestModel = fit$lambda == fit$lambda.1se
        beta = rep(0, length(independentVariableCols))
        names(beta) = independentVariableCols
        beta[filterColumns] = fit$glmnet.fit$beta[, bestModel]
        return(data.frame(t(beta)))
    }
    
    stackingData[, lassoModel(.SD), by = byKey]
    return(weights)
}