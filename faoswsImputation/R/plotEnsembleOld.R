##' Plot Ensemble and Model Fits (Old)
##' 
##' This function plots each of the individual models in the ensemble as well
##' as the initial data and the final ensemble.  This function is not meant to
##' be called directly by the user but instead is a helper function called by
##' ensembleImpute (if the argument plotImputation = TRUE).  This function
##' differs from plotEnsemble in that it uses the original plotting code (base
##' graphics instead of ggplot2).
##' 
##' @param data The data.table containing the data being imputed.
##' @param modelFits A list of length equal to the number of models.  Each
##' element of the list should be a numeric vector of length nrow(data). This
##' object is usually created by computeEnsembleFit.
##' @param modelWeights A matrix of dimension nrow(data) x length(modelFits).
##' Each element corresponds to the weight for model/column j and for 
##' observation/row i.  Note: modelFits and modelWeights need not have the same
##' ordering, but they should have the same names as this is how they are
##' matched.
##' @param ensemble A numeric vector containing either the original data (if it
##' was not missing) or the imputed value (if the original data was missing).
##' @param value The column name of data containing the variable being imputed.
##' @param byKey The column name of data containing the grouping variable.
##' @param yearValue The column name of data containing the time variable.
##' 
##' @return No value is returned, but a plot is generated.
##' 

plotEnsembleOld = function(data, modelFits, modelWeights, ensemble){
    modelNames = names(modelFits)
    nModels = length(modelNames)
    # Use par(mfrow=...) to plot all ensemble imputations
    plotCount = data[, anyNA(get(imputationValueColumn)), by = byKey]
    plotCount = sum(plotCount$V1)
    if(plotCount == 0){
        warning("No values imputed so no plots generated!")
    } else {
        if(plotCount >= 25)
            mar = c(3, 2, 2, 1)
        else
            mar = c(5, 4, 4, 2) # the default
        par(mfrow = n2mfrow(plotCount), mar = mar)
        for(aCode in unique(data[[byKey]])){
            filter = data[[byKey]] == aCode
            # Don't plot this value of byKey if no imputation was done
            if(!anyNA(data[[imputationValueColumn]][filter]))
                next
            plotMax = sapply(modelFits, function(x){
                max(x[filter])
            })
            plot(ensemble[filter],
                 ylim = c(0, 1.1 * max(plotMax, na.rm = TRUE)),
                 type = "n", xlab = "", ylab = "",
                 main = data[get(byKey) == aCode, get(byKey)][1])
            colPal = brewer.pal(nModels, "Paired")
            for(i in 1:nModels){
                lines(modelFits[[modelNames[i]]][filter],
                      col = colPal[i])
            }
            yearCount = sum(filter)
            lines(1:yearCount, ensemble[filter],
                  col = "steelblue", lwd = 3)
            points(ensemble[filter], pch = 19)
            points((1:yearCount)[missIndex[filter]],
                   ensemble[filter][missIndex[filter]],
                   col = "steelblue", cex = 1, pch = 19)
            modelWeightsToPlot =
                weightMatrixToVector(modelWeights[filter,])
            # Reorder so sequence matches modelNames:
            modelWeightsToPlot =
                as.data.frame(modelWeightsToPlot)[modelNames]
            legend("topleft",
                   legend = c(paste0(modelNames, "(",
                       round(modelWeightsToPlot * 100, 2),
                       "%)"), "Ensemble"),
                   col = c(colPal, "steelblue"),
                   lwd = c(rep(1, nModels), 3),
                   bty = "n")
        }
    }
}