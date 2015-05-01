##' The default LOESS model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultLoess = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    time = 1:length(x)
    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    n.obs = length(na.omit(x))
    if(n.obs < 5)
        return(as.numeric(rep(NA_real_, length.out = yearCount)))
    span = 4 / sum(!is.na(x))
    loessFit = try(predict(loess(formula = x ~ time,
        control = loess.control(surface = "direct"),
        span = span, degree = 1),
        newdata = data.frame(time)), silent = TRUE)
    if(!inherits(loessFit, "try-error") & n.obs >= 5){
        loessFit[loessFit < 0] = 0
    } else {
        loessFit = as.numeric(rep(NA_real_, yearCount))
    }
    loessFit
}