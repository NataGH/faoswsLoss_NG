##' Optimized Mixed Model for Imputation
##'
##' This function imputes missing values with a linear mixed model.
##' 
##' The default functionality of this model is to fit a linear mixed model to
##' the data.  time and intercept are assumed to be fixed effects, and the
##' random effects are specified by the byKey parameter of
##' imputationParameters.  So, for example, byKey may reference the variable
##' containing country data.  In that case, a model is fit which assumes a
##' linear relationship between production (or whatever dependent variable the
##' user has specified) and time.  However, the intercept and slope of this
##' fit varies from country to country, and so country is considered a random
##' effect.
##' 
##' Moreover, the model fit is not a simple linear regression, but rather a
##' spline regression (using the bs function from the \pkg{splines} package).
##' The fit of this model will therefore depend on the number of degrees of
##' freedom of this spline model (and, if the degrees of freedom is 1, then the
##' simple linear regression model is used).
##'
##' @param data The data.table object containing the data.
##' @param maxdf The maximum degrees of freedom for the spline.
##' @param weights The weights for the observations.
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

defaultMixedModelOptimized = function(data, maxdf = 5, weights = NULL, modelFormula,
                             imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    uniqueByKey = data[!is.na(get(imputationParameters$imputationValueColumn)),
                       1, by = c(imputationParameters$byKey)]
    if(nrow(uniqueByKey) <= 1) # Mixed model invalid if only one level:
        return(rep(NA_real_, nrow(data)))
    if("byKey" %in% colnames(data))
        stop("defaultMixedModel assumes 'byKey' is not a column name of data")

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
        model = try(
            lme4::lmer(formula = modelFormula, data = data,
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
                newModelFormula = as.formula(paste0(
                    imputationParameters$imputationValueColumn,
                    "~ -1 + (1 + bs(", imputationParameters$yearValue,
                    ", df = ", i, ", degree = 1)|byKey)"))
                newModel = try(
                    lme4::lmer(formula = newModelFormula,
                               data = data,
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
                    if(mean(benchmarkError$t) > mean(newModelError$t)){
                        modelFormula = newModelFormula
                        model = newModel
                        benchmarkError = newModelError
                    } else {
                        cat("Model with", i - 1,
                            "degree of freedom is selected\n")
                        break
                    }                   
                }
            }
        }
    } else {
        model = try(
            lme4::lmer(formula = modelFormula, data = data,
                       weights = weights,
                       REML = FALSE)
            )
    }
                
    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        modelFit = predict(model, newdata = data, allow.new.levels = TRUE)
    } else {
        modelFit = rep(NA_real_, nrow(data))
    }
    
    if("byKey" %in% colnames(data))
        data[, byKey := NULL]

    return(modelFit)
}
