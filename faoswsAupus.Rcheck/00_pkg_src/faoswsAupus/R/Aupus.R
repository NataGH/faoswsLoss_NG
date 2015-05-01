##' AUPUS
##' 
##' This function performs the whole aupus procedure.
##'
##' @param aupusNetwork A list of two data.tables.  The objects should be the 
##' node and edge data.tables.  Typically, this argument will be the output
##' from the function suaToNetworkRepresentation.
##' @param aupusParam A list of running parameters to be used in pulling the
##' data. Typically, this is generated from getAupusParameter (see that
##' function for a description of the required elements).
##' 
##' @return A list with two components: nodes and edges.  These objects are the
##' same as the input nodes and edges but after being processed by the AUPUS
##' algorithm.
##' 
##' @export
##' 

Aupus = function(aupusNetwork, aupusParam){
    
    ## Data Quality Checks
    stopifnot(is(aupusNetwork, "list"))
    stopifnot(names(aupusNetwork) == c("nodes", "edges"))
    nodes = aupusNetwork$nodes
    edges = aupusNetwork$edges
    stopifnot(is(nodes, "data.table"))
    stopifnot(is(edges, "data.table"))
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Set parent and child names based on aupusParam input:
    from = aupusParam$keyNames$itemParentName
    to = aupusParam$keyNames$itemChildName
    
    processingLevelData =
        edges[, findProcessingLevel(.SD, from = from, to = to,
                                    aupusParam = aupusParam),
                   by = c(aupusParam$keyNames$areaName,
                          aupusParam$keyNames$yearName)]
    setkeyv(processingLevelData, key(nodes))

    ## Add processing level to the nodes data.table
    nodes[processingLevelData, processingLevel := i.processingLevel]
    nodes[is.na(processingLevel), processingLevel := as.numeric(0)]

    for(currentLevel in unique(nodes$processingLevel)){
        
        ## Step (0): Grab the subset of nodes at this processing level
        nodesCurrentLevel = nodes[processingLevel == currentLevel, ]
        nodesNextLevel = nodes[processingLevel == currentLevel + 1, ]
        
        ## Step (1): Run the aupus module at the primary level on the nodes
        calculateAupusElements(aupusFinalData = nodesCurrentLevel,
                               itemTypeCol = aupusParam$keyNames$itemTypeName,
                               balanceElementNum =
                                   aupusParam$keyNames$balanceElementName,
                               aupusParam = aupusParam)

        ## Step (2): Update the edges (extraction rate and input from processing)
        updateEdges(nodes = nodesCurrentLevel, 
                    edges = edges, aupusParam = aupusParam)

        ## Step (3): Propagate input from processing to the node
        updateInputFromProcessing(nodes = nodesNextLevel,
                                  edges = edges,
                                  aupusParam = aupusParam)
        
        ## Step (4): Write the processed data.table back to nodes.  Note: edges
        ## does not need to be written back as the entire edges data.table is
        ## passed (by reference) and thus changes are made in place.
        
        ## First, add NA columns to nodes if necessary
        missingNames = colnames(nodesCurrentLevel)
        missingNames = missingNames[!missingNames %in% colnames(nodes)]
        if(length(missingNames) > 0){
            missingType = nodesCurrentLevel[, sapply(.SD, class),
                                            .SDcols = missingNames]
            ## Ensure you're adding the right type: numeric, character, or logical
            for(name in missingNames)
                nodes[, c(name) := switch(missingType[[name]],
                                          numeric = NA_real_,
                                          character = NA_character_,
                                          NA)]
        }
        
        ## Add processed data back into the nodes object
        okey = key(nodes)
        nodes[processingLevel == currentLevel, colnames(nodes) :=
                  nodesCurrentLevel]
        setkeyv(x = nodes, cols = okey)
        nodes[processingLevel == currentLevel + 1, colnames(nodes) :=
                  nodesNextLevel]
        setkeyv(x = nodes, cols = okey)
    }

    nodes[, processingLevel := NULL]
    list(nodes = nodes, edges = edges)
}
