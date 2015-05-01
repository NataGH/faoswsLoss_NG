##' Default range for ensemble models
##' 
##' This function returns the default extrapolation range values for each model
##' in the input list of models.
##'
##' @param ensembleModel A list of ensemble models to return ranges for.
##' 
##' @return A numeric vector of the default ranges for each of the ensemble
##' models.
##' 
##' @export

getDefaultRange = function(ensembleModel){

    ### Data Quality Checks
    if(is.null(names(ensembleModel)))
        stop("ensembleModel must be a named list!")
    stopifnot(is(ensembleModel, "list"))
    stopifnot(all(sapply(ensembleModel, is) == "ensembleModel"))
        
    range = lapply(names(ensembleModel), switch,
        defaultMean = Inf,
        defaultLm = Inf,
        defaultExp = 0,
        defaultLogistic = Inf,
        defaultLoess = 1,
        defaultSpline = 1,
        defaultArima = Inf,
        defaultMars = Inf,
        defaultNaive = 1,
        defaultMixedModel = Inf
    )
    if(do.call("any", lapply(range, is.null)))
        stop("A model in ensembleModel has no default range in getDefaultRange()")
    return(unlist(range))
}