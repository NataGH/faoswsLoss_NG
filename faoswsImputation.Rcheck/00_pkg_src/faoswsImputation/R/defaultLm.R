##' The default linear regression model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultLm = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    time = 1:length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, length(x))))
    lmFit = predict(lm(formula = x ~ time),
        newdata = data.frame(time = time))
    lmFit[lmFit < 0] = 0
    lmFit    
}
