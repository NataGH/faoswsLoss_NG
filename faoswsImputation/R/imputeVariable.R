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

    missingIndex = is.na(
        data[, get(imputationParameters$imputationValueColumn)])
    data[, c(imputationParameters$imputationValueColumn) := 
             ensembleImpute(data = data,
                            imputationParameters = imputationParameters)]
    imputedIndex = missingIndex &
        !is.na(data[[imputationParameters$imputationValueColumn]])
    invisible(data[imputedIndex,
                   c(imputationParameters$imputationFlagColumn,
                     imputationParameters$imputationMethodColumn) :=
                       list(imputationParameters$imputationFlag,
                            imputationParameters$newMethodFlag)])
}
