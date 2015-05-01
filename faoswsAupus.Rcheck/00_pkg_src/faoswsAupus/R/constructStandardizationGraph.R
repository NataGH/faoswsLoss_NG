##' Construct Standardization Graph
##' 
##' This function constructs the graph/network required for standardization by
##' generating one graph object for each year.  This graph has the supplied
##' edges and nodes as the corresponding edges and nodes on the graph.
##'
##' @param aupusNetwork A list of 2 data.tables named "nodes" and "edges".
##' These are usually produced by suaToNetworkRepresentation and contain
##' information about the data at the nodes and along the edges of the network.
##' @param standardizeElement The column names of nodes that should be
##' included in the returned graphs.  These are the elements to be
##' standardized.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A list of the same length as the number of unique years in the
##' aupus network.
##' 
##' @export
##' 

constructStandardizationGraph = function(aupusNetwork,
    standardizeElement, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(aupusNetwork, "list"))
    stopifnot(names(aupusNetwork) == c("nodes", "edges"))
    stopifnot(sapply(aupusNetwork, is.data.table))
    
    ## Set parent and child names based on aupusParam input:
    from = aupusParam$keyNames$itemChildName
    to = aupusParam$keyNames$itemParentName
    nodeCopy = copy(aupusNetwork$nodes)
    edgeCopy = copy(aupusNetwork$edges)

    ## Reorder edges so that the first two columns are parent/child.  This is
    ## necessary for later when we use graph.data.frame.
    attributeColumns = colnames(edgeCopy)[!colnames(edgeCopy) %in% c(from, to)]
    setcolorder(edgeCopy, neworder = c(from, to, attributeColumns))
    setnames(edgeCopy, to, aupusParam$keyNames$itemName)

    ## Create the final object as a list of igraph objects, one for each year.
    yearName = aupusParam$keyNames$yearName
    uniqueYears = unique(nodeCopy[, unique(get(yearName))],
        edgeCopy[, unique(get(yearName))])
    graph.lst = lapply(uniqueYears, function(year){
        graph.data.frame(
            d = edgeCopy[get(yearName) == year, ],
            vertices = nodeCopy[get(yearName) == year,
                    ## Don't pull all the data, only the needed FBS elements:
                    c(aupusParam$keyNames$itemName, standardizeElement),
                    with = FALSE])
    })
    names(graph.lst) = uniqueYears
    graph.lst
}
