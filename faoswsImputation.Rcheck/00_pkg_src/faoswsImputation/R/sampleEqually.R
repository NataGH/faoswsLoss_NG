##' Helper function for makeCvGroup
##' 
##' This function is designed to ensure that creation of cross-validation
##' groups are well-balanced.  Suppose there are k groups and n observations.
##' If n is a multiple of k, each group is represented exactly n/k times.
##' Otherwise, each group is represented either floor(n/k) or floor(n/k)+1
##' times.
##' 
##' @param n The total number of values to be sampled.
##' @param k The values to be sampled from (1:k).
##' 
##' @return A vector of length n that contains values in 1:k.
##' 
##' @export
##' 

sampleEqually = function(n, k){

    ### Data Quality Checks
    stopifnot(n > 0)
    stopifnot(k > 0)
    # n and k should be integers:
    stopifnot(floor(n) == n)
    stopifnot(floor(k) == k)

    values = rep(1:k, times = floor(n / k))
    values = c(values, sample(k, size = n %% k))
    sample(values, size = length(values))
}