##' Update Input from Processing
##' 
##' The edge dataset contains information about the quantities of a particular
##' item flowing from a particular parent to a particular child.  This function
##' aggregates the edge dataset, grouping by child, and replaces element 31
##' (Actual Producing Factor) of the child with the inputs from all the
##' parents.
##' 
##' @param nodes The nodes data returned by the function buildNodes.
##' @param edges The edge data returned by the function buildEdges.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return No values are returned, but instead the passed nodes dataset is
##' updated.
##' 
##' @export
##' 

updateInputFromProcessing = function(nodes, edges, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Bring in variables for element 31
    defineElementVariables(elements = 31, aupusParam = aupusParam)
    
    aggregateKey = key(edges)
    aggregateKey = aggregateKey[aggregateKey != aupusParam$keyNames$itemParentName]
    aggregatedInput = edges[, list(Aggregated_input = sum(Value_input)),
        by = aggregateKey]
    setnames(aggregatedInput,
             old = aupusParam$keyNames$itemChildName,
             new = aupusParam$keyNames$itemName)
    newInputKeys = aggregateKey
    newInputKeys[newInputKeys == aupusParam$keyNames$itemChildName] =
        aupusParam$keyNames$itemName

    okey = key(nodes)
    setkeyv(nodes, newInputKeys)
    nodes[aggregatedInput, c(element31Num) := Aggregated_input]
    setkeyv(nodes, okey)
}
