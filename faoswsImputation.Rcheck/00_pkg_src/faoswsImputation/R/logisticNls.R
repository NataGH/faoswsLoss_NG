##' Helper function for defaultLogistic
##' 
##' This function fits a logistic model to the data via non-linear least
##' squares (?nls).  The function fit is:
##' x = B / (1 + exp(-C*(time-D)))
##' where time is the independent variable (1 to length(x)) and x is the
##' dependent variable.
##'
##' @param x The dependent variable.
##' 
##' @return A numeric vector of the same length as x but with the model
##' estimates at each point in time.  Note: if the logistic model has it's
##' midpoint outside the range of the data, this function will return a vector
##' of NA's (as the original defaultLogistic function had this behavior to
##' prevent poor fitting).
##' 
##' @family logistic functions
##' 
##' @export

logisticNls = function(x){
    time = 1:length(x)
    model = nls(formula = x ~ B / (1 + exp(-C*(time-D))),
        start = list(B = max(x, na.rm = T), C = 1, D = mean(time)))
    if(coef(model)[1] < 0)
        stop("B must be non-negative, bad model found.")
    logisticFit = predict(model, newdata = data.frame(time = time))
    return(logisticFit)
}