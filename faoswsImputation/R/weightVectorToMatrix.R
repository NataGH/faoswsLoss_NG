##' Convert the weights vector to a matrix
##' 
##' The default weights vector assumes that all models are valid for all
##' imputation points.  However, some imputations may be extrapolations that
##' are well outside the range of the data, and in this case not all models
##' will be valid.  Thus, this function takes the weights vector and adjusts it
##' for such observations.
##' 
##' Note that the extrapolation range was not a parameter in previous versions
##' of the code.  Thus, if that element is not present, the model assumes the
##' range is infinite.  This ensures backwards-compatibility.
##'
##' @param x A numeric vector to be imputed.
##' @param w The vector of weights, computed based on the errors of the models.
##' @param modelExtrapolationRange A numeric vector specifying the valid range
##' of extrapolation for each model.  This vector must be the same length as
##' w, as it's i-th element gives the extrapolation range for the
##' i-th weight.  Note: care should be taken to ensure the weight vector's i-th
##' element and the modelExtrapolationRange vector's i-th element both
##' correspond to the same model in the ensemble.
##' 
##' @return A matrix of weights of dimension Txm, where T=number of time steps
##' and m=number of models.  The (i,j) element of this matrix, then, is the
##' weight that the j-th model should receive at the i-th time step.
##' 
##' @export

weightVectorToMatrix = function(x, w, modelExtrapolationRange){

    ### Data Quality Checks
    stopifnot(length(w) == length(modelExtrapolationRange))
    
    ### Run the function:
    observedExtrapolationRange = getObservedExtrapolationRange(x)
    validRangeMatrix = sapply(modelExtrapolationRange,
        function(x) observedExtrapolationRange <= x)
    weightMatrix = matrix(w, nrow = length(x), ncol = length(w), byrow = TRUE)
    weightMatrix = weightMatrix * validRangeMatrix
    rowTotals = apply(weightMatrix, 1, sum)
    weightMatrix = weightMatrix * 1/rowTotals
    weightMatrix
}