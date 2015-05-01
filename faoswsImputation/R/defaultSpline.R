##' The default spline model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export
##' 

defaultSpline = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    yearCount = length(x)
    time = 1:length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    splineFit = spline(time, x, n = yearCount * 5 - 4, xout = time,
        method = "natural")$y
    splineFit[splineFit < 0] = 0
    splineFit
}
