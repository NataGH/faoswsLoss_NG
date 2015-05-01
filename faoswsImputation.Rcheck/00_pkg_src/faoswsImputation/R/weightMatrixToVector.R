##' Convert the weights matrix to a vector
##' 
##' Initially, a weight is computed for each model.  Then, the weight matrix is
##' created, and this contains those weights if they are valid at the current
##' location (i.e. if that particular model is allowed to extrapolate to the
##' current point).  It may be useful to convert the weight matrix back to a
##' vector (in particular, for plotting the weights of each base learner).
##'
##' @param weightMatrix The weights matrix, as returned by
##' computeEnsembleWeights.
##' 
##' @return A vector of weights that is the original weight vector prior to
##' being extended to a matrix.
##' 
##' @details This function works by finding which row of the weights matrix has
##' the fewest number of 0's.  This row will correspond to an interpolation or
##' non-missing row, and thus all models will be valid at this point.  Since
##' all models are valid here, the weights at this point will correspond to the
##' original weight vector, and so we just return that row.
##' 
##' @export

weightMatrixToVector = function(weightMatrix){
    zerosInRow = apply(weightMatrix, 1, function(x) sum(x == 0))
    smallestZerosRow = which.min(zerosInRow)
    weightMatrix[smallestZerosRow,]
}