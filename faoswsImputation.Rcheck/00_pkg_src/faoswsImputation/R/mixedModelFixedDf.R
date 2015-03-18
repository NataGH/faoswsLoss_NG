##' Mixed Model for Imputation with Fixed DF
##'
##' This function imputes missing values through a linear mixed model.  It
##' differs from defaultMixedModel in that it only fits a model with one value
##' for the degree of freedom (defaultMixedModel currently fits multiple models
##' and uses bootstrapping to pick the optimal one).  However, if models are 
##' eventually combined into an ensemble using leave-one-out cross-validation,
##' such bootstrapping shouldn't be necessary (but it may be reasonable to
##' include several mixed models with varying degrees of freedom).
##' 
##' Note: this function will return the same result as defaultMixedModel if a
##' modelFormula is specified (to both functions).
##'
##' @param data The data.table object containing the data.
##' @param df The degrees of freedom for the spline.
##' @param weights The weights for the observation, if NULL each observation
##' has the same weight.
##' @param modelFormula Formula specifying how the dependent variable (value)
##' depends on the other columns of data.  Should be a valid mixed model
##' formula, as it will be passed to lmer (R's mixed model function).  If
##' missing, a spline on the year will be used.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

mixedModelFixedDf = function(data, df = 1, weights = NULL, modelFormula,
                             imputationParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    uniqueByKey = data[!is.na(get(imputationParameters$imputationValueColumn)),
                       1, by = c(imputationParameters$byKey)]
    if(nrow(uniqueByKey) <= 1) # Mixed model invalid if only one level:
        return(rep(NA_real_, nrow(data)))
    stopifnot(df >= 1)
    
    if(missing(modelFormula)){
        ## Define the formula (no splines if df == 1)
        if(df > 1)
            modelFormula = as.formula(paste0(
                imputationParameters$imputationValueColumn,
                "~ -1 + (1 + bs(", imputationParameters$yearValue,
                ", df = ", df, ", degree = 1)|", imputationParameters$byKey,
                ")"))
        else
            modelFormula = as.formula(paste0(
                imputationParameters$imputationValueColumn, " ~ -1 + (1 + ",
                imputationParameters$yearValue, "|",
                imputationParameters$byKey, ")"))
        
        model = try(
            lmer(formula = modelFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
    } else { #If modelFormula was specified, then use that formula
        model = try(
            lmer(formula = modelFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
    }
    
    ## Predict with resulting model, or return NA's if it failed
    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        modelFit = predict(model, newdata = data, allow.new.levels = TRUE)
    } else {
        modelFit = rep(NA_real_, nrow(data))
    }

    return(modelFit)
}
