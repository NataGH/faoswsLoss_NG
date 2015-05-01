##' Add Ensemble Weights for Sets of the Data without Valid Observations
##' 
##' In some cases, countries (or whatever the byKey is) will not have any data
##' available.  In these cases, we can impute values for these observations via
##' an ensemble, but we'll need a method for weighting the models in the
##' ensemble.
##' 
##' This function computes the weights for these models by looking at the
##' cross-validation error of the global models (or "commodity" level) and uses
##' those errors to determine the final weights.
##' 
##' @param data The data.table object containing the data which will be imputed.
##' @param weights A data.table containing the currently estimated ensemble
##' weights and errors.  These are used to compute the global errors and add new
##' weights for models with missing data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A data.table of the same format as weights.  This function is
##' really just a helper function for getEnsembleWeight, and so the output of
##' the function is specifically designed to be rbinded on to the weights
##' object in that function.
##' 

addNoDataModels = function(data, weights, imputationParameters){
    
    ## Grab the global models
    modelLevel = sapply(imputationParameters$ensembleModels,
                        function(x) x@level)
    globalModels = names(imputationParameters$ensembleModels)[
        modelLevel == "commodity"]
    
    ## Determine which byKey groups have no data.  It's a little complicated
    ## because byKey could be a vector, so we have to do a merge.
    byKey = imputationParameters$byKey
    weights[, match := TRUE]
    noDataLocations = merge.data.frame(
        unique(data[, c(byKey), with = FALSE]),
        unique(weights[, c(byKey, "match"), with = FALSE]),
        all = TRUE, by = byKey)
    noDataLocations = noDataLocations[is.na(noDataLocations$match), byKey,
                                      drop = FALSE]
    weights[, match := NULL]
    
    if(length(globalModels) == 0 & length(noDataLocations) > 0)
        stop("You must use at least one global model to impute on slices ",
             "of the data without any observations!  Add a global model ",
             "or set imputationParameters$estimateNoData = FALSE.")
    
    ## Add global models if we have locations needing them
    if(nrow(noDataLocations) > 0){
        newRows = weights[, list(averageError = mean(averageError,
                                                     na.rm = TRUE)),
                                by = model]
        ## Set non-global model error to Inf so they're never used here
        newRows = newRows[!model %in% globalModels, averageError := Inf]
        newRows = merge(noDataLocations, newRows)
        newRows$averageErrorByKey = NA
        newRows = as.data.table(newRows)
        newRows[, weight := 1/averageError / sum(1/averageError),
                 by = c(byKey)]
    } else {
        newRows = NULL
    }
    return(newRows)
}