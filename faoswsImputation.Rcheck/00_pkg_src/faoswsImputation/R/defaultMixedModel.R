##' Mixed Model for Imputation
##'
##' This function imputes missing values through linear mixed model.
##'
##' @param data The data.table object containing the data.
##' @param maxdf The maximum degree of freedom for the spline.
##' @param weights The weights for the observation
##' @param modelFormula Formula specifying how the dependent variable (value)
##' depends on the other columns of data.  Should be a valid mixed model
##' formula, as it will be passed to lmer (R's mixed model function).
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

defaultMixedModel = function(data, maxdf = 5, weights = NULL, modelFormula,
                             imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    uniqueByKey = data[!is.na(get(imputationParameters$imputationValueColumn)),
                       1, by = c(imputationParameters$byKey)]
    if(nrow(uniqueByKey) <= 1) # Mixed model invalid if only one level:
        return(rep(NA_real_, nrow(data)))

    ## Make the key into one column by pasting all key columns together with
    ## a very unique symbol (:-: shouldn't show up in other keys on accident).
    byKey = apply(data[, imputationParameters$byKey, with = FALSE],
                  1, function(x) paste(x, collapse = " :-: "))
    data[, byKey := byKey]
    if(missing(modelFormula)){
        modelFormula =
            as.formula(paste0(imputationParameters$imputationValueColumn,
                              " ~ -1 + (1 + ", imputationParameters$yearValue,
                              "|byKey)"))
        ## print(modelFormula)
        model = try(
            lme4::lmer(formula = modelFormula, data = data,
                       ## weights = data[, productionValue],
                       weights = weights,
                       REML = FALSE)
            )
        
        predictError = function(x, y, newdata){
            yhat = predict(x, newdata = newdata, allow.new.levels = TRUE)
            amse = sum((yhat - y)^2, na.rm = TRUE)/
                length(na.omit(y))
            amse
        }

        benchmarkError = lme4::bootMer(model,
            FUN = function(x){
                predictError(x = x,
                    y = data[[imputationParameters$imputationValueColumn]],
                    newdata = data)
            }, nsim = 100)
        
        if(!inherits(model, "try-error") & maxdf > 1){
            for(i in 2:maxdf){
                ## cat("proposing df:", i, "\n")
                newModelFormula = as.formula(paste0(
                    imputationParameters$imputationValueColumn,
                    "~ -1 + (1 + bs(", imputationParameters$yearValue,
                    ", df = ", i, ", degree = 1)|byKey)"))
                ## print(newModelFormula)
                newModel = try(
                    lme4::lmer(formula = newModelFormula,
                               data = data,
                               ## weights = data[, productionValue],
                               weights = weights,
                               REML = FALSE)
                    )
                if(!inherits(newModel, "try-error")){

                    newModelError = lme4::bootMer(newModel,
                        FUN = function(x){
                            predictError(x = x,
                                y = data[[imputationParameters$imputationValueColumn]],
                                newdata = data)
                        }, nsim = 100)
                    ## cat("old:", mean(benchmarkError$t), "\n")
                    ## cat("new:", mean(newModelError$t), "\n")
                    if(mean(benchmarkError$t) > mean(newModelError$t)){
                        modelFormula = newModelFormula
                        model = newModel
                        benchmarkError = newModelError
                    } else {
                        cat("Model with", i - 1,
                            "degree of freedom is selected\n")
                        break
                    }                   
                    
                    ## m = bootMer(model, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## nm = bootMer(newModel, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## if(quantile(-2 * m$t + 2 * nm$t, prob = 0.05) < 0){
                    ##     break
                    ## } else {
                    ##     yieldFormula = newYieldFormula
                    ##     model = newModel
                    ## }
                }
            }
        }
    } else {
        model = try(
            lme4::lmer(formula = modelFormula, data = data,
                       ## weights = data[, productionValue],
                       weights = weights,
                       REML = FALSE)
            )
    }
                
    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        modelFit = predict(model, newdata = data, allow.new.levels = TRUE)
    }
    
    if("byKey" %in% colnames(data))
        data[, byKey := NULL]

    return(modelFit)
}
