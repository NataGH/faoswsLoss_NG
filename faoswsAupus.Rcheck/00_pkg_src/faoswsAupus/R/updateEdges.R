##' Update Edges
##' 
##' Function to update the extraction rates and input from processing after
##' each iteration of Aupus.  This function takes the quantity of a commodity
##' that was allocated to Reemployment Same Sector in one step and passes that
##' value on to the edges (to be passed to the children).  Additionally, the
##' extraction rates computed by the parents are passed on to the children.
##' 
##' @param nodes The nodes data returned by the function buildNodes
##' @param edges The edge data returned by the function buildEdges
##' @param aupusParam A list of running parameters to be used in pulling the
##' data. Typically, this is generated from getAupusParameter (see that
##' function for a description of the required elements).
##' 
##' @return No data is returned, but the edges object is updated with the
##' information from nodes.
##' 
##' @export
##' 

updateEdges = function(nodes, edges, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(nodes, "data.table"))
    stopifnot(is(edges, "data.table"))
    
    ## Update input
    ##
    ## Element 131 is for parent, while input value is child
    defineElementVariables(elements = c(41, 131), aupusParam = aupusParam)
    newInputs = na.omit(nodes[, c(key(nodes), element131Num), with = FALSE])
    setnames(newInputs,
             old = c(aupusParam$keyNames$itemName, element131Num),
             new = c(aupusParam$keyNames$itemParentName, "inputFromParent"))
    newInputKeys = key(nodes)
    ## Update keys to reflect that the itemName corresponds to the parent
    newInputKeys[newInputKeys == aupusParam$keyNames$itemName] =
        aupusParam$keyNames$itemParentName
    setkeyv(newInputs, newInputKeys)

    ## Update extraction rates
    ##
    ## Extraction rate is for children
    newExtraction = na.omit(nodes[, c(key(nodes), element41Num), with = FALSE])
    setnames(newExtraction,
             old = c(aupusParam$keyNames$itemName, element41Num),
             new = c(aupusParam$keyNames$itemParentName, "newExtractionRates"))
    newExtractionKeys = key(nodes)
    newExtractionKeys[newExtractionKeys == aupusParam$keyNames$itemName] =
        aupusParam$keyNames$itemParentName
    setkeyv(newExtraction, newExtractionKeys)

    okey = key(edges)
    setkeyv(edges, key(newInputs))
    ## Use allow.cartesian = TRUE in case some rows of newInputs don't match.
    edges[newInputs, Value_input := inputFromParent * Value_share / 100,
          allow.cartesian = TRUE]
    setkeyv(edges, key(newExtraction))
    edges[newExtraction, Value_extraction := newExtractionRates,
          allow.cartesian = TRUE]
    setkeyv(edges, okey)
}
