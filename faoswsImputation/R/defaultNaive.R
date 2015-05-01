##' The default naive model for the ensemble model.
##'
##' The naive model is simply linear interpolation with last
##' observation carried forward and backward.
##'
##' @param x A numeric vector to be imputed.
##' @export
##' 

defaultNaive = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)

    require(zoo)
    nobserved = length(na.omit(x))
    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA_real_, yearCount)))
    type = ifelse(nobserved == 0, "none",
        ifelse(nobserved == 1, "repeat", "naive"))
    switch(type,
           none = {
               tmp = as.numeric(rep(NA_real_, yearCount))
           },
           `repeat` = {
               tmp = rep(na.omit(x), yearCount)
           },
           naive = {
               tmp = na.locf(na.locf(na.approx(x, na.rm = FALSE),
                   na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)
           }
           )
    as.numeric(tmp)
}
