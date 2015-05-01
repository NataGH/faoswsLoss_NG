##' SUA to Network Representation
##' 
##' SUA data (Supply Utilization Accounts) contains information on specific
##' uses of commodities within the food balance sheet.  Often, SUA is explained
##' as being analogous to all the financial transactions of a company and the
##' Food Balance Sheets are thought of as the financial summary statement.
##' 
##' This function converts sua data to a network specification
##' which is more natural for processing.
##'
##' @param dataList A list of AUPUS datasets to be analyzed, typically as
##' produced by the function getAupusDataset.  For more details on this
##' argument, please see ?getAupusDataset.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A list of two elements: nodes and edges.  Both of these objects are
##' data.tables, and they share the same keys with one exception: edges has a
##' parent and child item key where nodes only has an item key.  All the input,
##' share and extraction data is stored in edges and the AUPUS, ratio, balance,
##' item, and population data is stored in the nodes.
##' 
##' @export
##' 

suaToNetworkRepresentation = function(dataList, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(dataList, "list"))
    stopifnot(names(dataList) == c("aupusData", "inputData", "ratioData",
                                   "shareData", "balanceElementData",
                                   "itemInfoData", "populationData",
                                   "extractionRateData"))

    edges = buildEdges(dataList)
    nodes = buildNodes(dataList)

    ## CHECK (Michael): Add in nodes that are contained in the edges
    ##                  list but not found in the node frame. Check
    ##                  why they are missing.

    uniqueEdgeNodes =
        unique(unlist(edges[, c(aupusParam$keyNames$itemParentName,
                                aupusParam$keyNames$itemChildName), with = FALSE]))

    missingNodes = uniqueEdgeNodes[!uniqueEdgeNodes %in%
        nodes[[aupusParam$keyNames$itemName]]]
    if(length(missingNodes) > 0)
        warning("The following nodes have no edges:\n",
                paste(missingNodes, collapse = "\n"))

    ## Determine which keys will be missing because of missingNodes
    missingKeyTable =
        data.table(expand.grid(unlist(unique(nodes[, c(aupusParam$keyNames$areaName),
                                                   with = FALSE])),
                               unlist(unique(nodes[, c(aupusParam$keyNames$yearName),
                                                   with = FALSE])),
                               missingNodes, stringsAsFactors = FALSE))
    with(aupusParam$keyNames,
         setnames(missingKeyTable, c(areaName, yearName, itemName)))
    nodes = rbind(nodes, missingKeyTable, fill = TRUE)
    setkeyv(nodes, key(dataList$aupusData))

    list(nodes = nodes, edges = edges)
}
