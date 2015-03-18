##' The default exponential model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultExp = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    time = 1:length(x)
    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    expFit = exp(predict(lm(formula = log(x + 1) ~ time),
        newdata = data.frame(time = time))) - 1
    expFit
}
