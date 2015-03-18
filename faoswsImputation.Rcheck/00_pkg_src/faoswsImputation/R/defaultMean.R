##' The default mean model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMean = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    meanFit = rep(mean(x, na.rm = !all(is.na(x))), yearCount)
    meanFit
}
