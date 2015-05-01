##' The default MARS model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMars = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    time = 1:length(x)
    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    obs.x = na.omit(x)
    if(any(is.na(x)))
        obs.time = time[-attr(obs.x, "na.action")]
    else
        obs.time = time
    marsFit = try(predict(earth::earth(obs.x ~ obs.time),
        newdata = data.frame(obs.time = time)), silent = TRUE)
    if(!inherits(marsFit, "try-error")){
        marsFit = c(marsFit)
        marsFit[marsFit < 0] = 0
    } else {
        marsFit = as.numeric(rep(NA_real_, yearCount))
    }
    marsFit
}
