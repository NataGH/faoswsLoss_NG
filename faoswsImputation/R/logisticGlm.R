##' Helper function for defaultLogistic
##' 
##' This function fits a logistic model to the data via logistic regression
##' (see ?glm and the binomial family).  To ensure all x values are between
##' 0 and 1, the values are all divided by the maximum x value.  Thus, this
##' model assumes the asymptote is the largest observed x value.  Time is the
##' independent variable and is the vector 1:length(x).
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

logisticGlm = function(x){
    time = 1:length(x)
    xmax = max(x, na.rm = TRUE)
    #If all values are 0 or NA's, then return no prediction
    if(is.na(xmax) | xmax <= 0)
        return(as.numeric(rep(NA, length(x))))
    x.scaled = x/xmax    
    model = glm(formula = x.scaled ~ time, family="binomial")
    midpoint = - coef(model)[1]/coef(model)[2]
    if(length(na.omit(x[time < midpoint])) < 1 |
       length(na.omit(x[time > midpoint])) < 1)
        return(as.numeric(rep(NA, length(x))))
    logisticFit =
        predict(model, 
                newdata = data.frame(time = time),
                type = "response") * xmax
    return(logisticFit)
}