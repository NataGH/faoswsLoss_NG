##' Function to impute production or yield
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @export
##' 

imputeVariable = function(data, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)

    ## Define which columns should store the imputation and flags, and create
    ## those columns if they don't currently exist.
    if(imputationParameters$newImputationColumn == ""){
        newValueColumn = imputationParameters$imputationValueColumn
        newObsFlagColumn = imputationParameters$imputationFlagColumn
        newMethodFlagColumn = imputationParameters$imputationMethodColumn
    } else {
        newValueColumn = paste0("Value_",
                                imputationParameters$newImputationColumn)
        newObsFlagColumn = paste0("flagObservationStatus_",
                                imputationParameters$newImputationColumn)
        newMethodFlagColumn = paste0("flagMethod_",
                                imputationParameters$newImputationColumn)
    }
    
    missingIndex = is.na(
        data[, get(imputationParameters$imputationValueColumn)])
    data[, c(newValueColumn) := 
             ensembleImpute(data = data,
                            imputationParameters = imputationParameters)]
    imputedIndex = missingIndex & !is.na(data[[newValueColumn]])
    invisible(data[imputedIndex,
                   c(newObsFlagColumn, newMethodFlagColumn) :=
                       list(imputationParameters$imputationFlag,
                            imputationParameters$newMethodFlag)])
}
