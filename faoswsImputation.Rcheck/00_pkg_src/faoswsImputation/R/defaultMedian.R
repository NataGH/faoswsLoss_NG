##' The default median model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMedian = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    medianFit = rep(median(x, na.rm = !all(is.na(x))), yearCount)
    medianFit
}
