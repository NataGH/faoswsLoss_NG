##' Get Weight Matrix
##' 
##' Initially, a weight is computed for each model and byKey.  However, some
##' models are not valid for some observations (as certain models are limited
##' in how far they can extrapolate outside the range of the data).  Thus, the
##' final weight for each ensemble model at each observation will depend on
##' that models performance for that byKey group as well as if that model is
##' valid at that point.
##' 
##' This function creates a weight matrix to use in constructing the final
##' ensemble.  If F is a nxk matrix (n = number of observations, k = number of
##' models) containing the fitted models, then this function constructs W,
##' another nxk matrix of weights.  The final ensemble estimate for observation
##' i can be computed by sum(F[i,]*W[i,]).
##'
##' @param data The data.table containing the data.
##' @param w The weights data.table, typically as produced in
##' computeEnsembleWeight.  There should be three columns: byKey, model, and
##' weight.  Weight gives the model weight for model within the byKey group,
##' and exactly one row should exist for each byKey/model pair.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A matrix of weights that can be multiplied by the fitted models to
##' give the imputed values.  Rows corresponding to non-missing values in data
##' have values of NA.
##' 
##' @export
##' 

getWeightMatrix = function(data, w, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    stopifnot(is(w, "data.table"))
    # w should have one row for each model at each byKey level
    uniqueKeyCnt = nrow(unique.data.frame(
        data[, imputationParameters$byKey, with = FALSE]))
    uniqueModelCnt = length(imputationParameters$ensembleModels)
    uniqueYearCnt = length(unique(data[[imputationParameters$yearValue]]))
    stopifnot(nrow(w) == uniqueKeyCnt * uniqueModelCnt)
    # There should only be one observation per byKey/year pair.
    if(nrow(data) > uniqueYearCnt * uniqueKeyCnt)
        stop("data has ",nrow(data), " rows but only ",
             uniqueKeyCnt * uniqueYearCnt, " unique levels!  This suggests ",
             "that byKey does not truly ",
             "partition the dataset.  You may need a different byKey, or you ",
             "may need to loop over each commodity, for example.")
    
    ## Run the function:
    impValue = imputationParameters$imputationValueColumn
    year = imputationParameters$yearValue
    data[, extrapolationRange := 
             getObservedExtrapolationRange(get(impValue)),
          by = c(imputationParameters$byKey)]
    weightMatrix = merge(data[, c(imputationParameters$byKey, impValue, year,
                                  "extrapolationRange"), with = FALSE],
                          w, by = imputationParameters$byKey, all = TRUE,
                         allow.cartesian = TRUE)
    # Set data back to it's original state
    data[, extrapolationRange := NULL]
    # Set weights to 0 that are outside of extrapolationRange
    range = sapply(imputationParameters$ensembleModels, function(model){
        model@extrapolationRange
    })
    weightMatrix[, allowedRange := range[model]]
    weightMatrix[allowedRange < extrapolationRange, weight := 0]
    # Renormalize weights so all columns add to 1
    weightMatrix[, weight := weight / sum(weight),
                 by = c(imputationParameters$byKey, year)]
    castFormula = paste( paste(c(imputationParameters$byKey, year),
                               collapse = " + "),
                         "~ model")
    weightMatrix = dcast.data.table(weightMatrix, castFormula, 
                                     value.var = "weight")
    weightMatrix[, c(imputationParameters$byKey, year) := NULL]
    weightMatrix[!is.na(data[, get(impValue)])] =
        NA
    return(weightMatrix)
}
