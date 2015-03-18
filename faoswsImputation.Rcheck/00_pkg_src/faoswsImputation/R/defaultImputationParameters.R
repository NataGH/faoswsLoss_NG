##' Default Imputation Parameters
##' 
##' This function can be used to generate the input parameters for the
##' ensemble imputation code.  This is a good way to get a list of the required
##' parameters and then modify parameters to match your particular
##' configuration.
##' 
##' @param variable Should be one of "production", "yield", or "seed".  These
##' are currently the three variables for which this imputation package has
##' been used.  It's straightforward to implement another type: simply add a
##' new if statement in the code below to assign the correct column types.
##' However, you will also need to update ensureImputationInputs to accept this
##' new variable.
##' 
##' @return Returns a list of the default parameters used in the ensemble
##' imputation algorithms.
##' 
##' @details Below is a description of the parameters:
##' \itemize{
##'   \item yearValue: The column name for the year variable in data.
##'   \item byKey: The column name for the variable representing the splitting
##'   group.  Usually, this is the country variable.
##'   \item ensembleModels: A list of objects, all of type ensembleModel, which
##'   will be applied to the data.
##'   \item restrictWeights: Should the maximum weight of one model in the
##'   ensemble be restricted?
##'   \item maximumWeights: If restrictWeights == TRUE, then this value
##'   (between 0.5 and 1) gives the largest value of a weight for a particular
##'   ensemble.
##'   \item plotImputation: Should the results of the imputation be plotted?
##'   \item errorType: Should "raw" errors be used or "loocv" (leave-one-out
##'   cross-validation)?  In general, "loocv" should be preferred, but "raw" is
##'   faster.
##'   \item errorFunction: A custom error function may be specified.  The
##'   default is mean-squared error.  This should be a function of a single
##'   vector numeric argument, and the return value should be a numeric vector
##'   of length 1.
##'   \item groupCount: How many cross-validation groups should be used for the
##'   ensemble models?
##'   \item missingFlag: How are missing values specified in the database?
##'   Usually, this is "M".
##'   \item imputationFlag: What observation flag should be assigned to imputed
##'   values?
##'   \item newMethodFlag: What method flag should be assigned to imputed
##'   values?
##'   \item flagTable: A table of the observation flags and their corresponding
##'   weights.
##'   \item variable: The name of the variable being imputed, either "seed",
##'   "yield", or "production".
##'   \item imputationValueColumn: The column name of the value to be imputed. 
##'   \item imputationFlagColumn: The column name of the observation flag for
##'   the imputed variable.
##'   \item imputationMethodColumn: The column name of the method flag for the
##'   imputed variable.
##' }
##' 
##' @export
##' 

defaultImputationParameters = function(variable){
    out = list(
         yearValue = "timePointYears",
         byKey = "geographicAreaM49",
         ensembleModels = allDefaultModels(),
         restrictWeights = TRUE,
         maximumWeights = 0.7,
         plotImputation = TRUE,
         errorType = "loocv",
         errorFunction = function(x) mean(x^2),
         groupCount = 10,
         missingFlag = "M",
         imputationFlag = "I",
         newMethodFlag = "e",
         flagTable = faoswsFlagTable)
    out$flagTable$flagObservationStatus =
        as.character(out$flagTable$flagObservationStatus)
    out$variable = variable
    if(variable == "production"){
        out$imputationValueColumn = "Value_measuredElement_5510"
        out$imputationFlagColumn = "flagObservationStatus_measuredElement_5510"
        out$imputationMethodColumn = "flagMethod_measuredElement_5510"
    } else if(variable == "yield"){
        out$imputationValueColumn = "Value_measuredElement_5416"
        out$imputationFlagColumn = "flagObservationStatus_measuredElement_5416"
        out$imputationMethodColumn = "flagMethod_measuredElement_5416"
    } else if(variable == "seed"){
        #Columns below are defined within faoswsSeed::imputeAreaSown()
        out$imputationValueColumn = "Value_areaSownRatio"
        out$imputationFlagColumn = "flagObservationStatus_areaSownRatio"
        out$imputationMethodColumn = "flagMethod_areaSownRatio"
    } else {
        stop("This variable has not yet been implemented!")
    }
    return(out)
}