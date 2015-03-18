##' Returns all default models
##'
##' This is a convenience function that returns a list of all the default
##' models.
##'
##' @return A list of all the default model functions.
##' 
##' @export
##' 

allDefaultModels = function(){
    return(list(defaultMean = ensembleModel(model = defaultMean,
                                            extrapolationRange = Inf,
                                            level = "countryCommodity"),
                defaultLm = ensembleModel(model = defaultLm,
                                          extrapolationRange = Inf,
                                          level = "countryCommodity"),
                defaultExp = ensembleModel(model = defaultExp,
                                           extrapolationRange = 2,
                                           level = "countryCommodity"),
                defaultLogistic  = ensembleModel(model = defaultLogistic,
                                                 extrapolationRange = Inf,
                                                 level = "countryCommodity"),
                defaultLoess  = ensembleModel(model = defaultLoess,
                                              extrapolationRange = 1,
                                              level = "countryCommodity"),
                defaultSpline  = ensembleModel(model = defaultSpline,
                                               extrapolationRange = 1,
                                               level = "countryCommodity"),
                defaultArima  = ensembleModel(model = defaultArima,
                                              extrapolationRange = Inf,
                                              level = "countryCommodity"),
                defaultMars  = ensembleModel(model = defaultMars,
                                             extrapolationRange = Inf,
                                             level = "countryCommodity"),
                defaultNaive  = ensembleModel(model = defaultNaive,
                                              extrapolationRange = 0,
                                              level = "countryCommodity"),
                defaultMixedModel = ensembleModel(model = defaultMixedModel,
                                                  extrapolationRange = Inf,
                                                  level = "commodity")))
}
