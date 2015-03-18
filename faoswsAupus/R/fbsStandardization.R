##' FBS Standardization
##' 
##' This function takes the graph and performs the standardization.
##'
##' @param graph The standardization graph from the function
##' constructStandardizationGraph.
##' @param standardizeElement A character vector of the column names of the
##' node object that should be processed.  This should correspond to the
##' elements of the FBS that need to be standardized.
##' @param plot Whether the network should be plotted as it is processed.  
##' A plot will be generated for each year/processing level, with prompts
##' between, so only use this option if running this function in interactive
##' mode.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A data.table object containing the final standardization values
##' for each year.
##' 
##' @export
##' 

fbsStandardization = function(graph, standardizeElement, plot, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)

    standardized =
        lapply(graph, FUN = function(x){
            standardization(graph = x,
                            standardizeElement = standardizeElement,
                            plot = plot,
                            aupusParam = aupusParam)
        })
    
    ## Combine results together and add a column for year
    standardizationFinal =
        Reduce(rbind,
               lapply(names(standardized),
                      FUN = function(x){
                          standardized[[x]][, `:=`(c(aupusParam$keyNames$yearName),
                                                   as.numeric(x))]
                      })
               )
    standardizationFinal
}
