##' Find Connected Graph
##' 
##' This function takes a graph/network and then find the subset of
##' the network which are connected to the specified nodes/commodities.
##'
##' @param graph The graph object created by the function
##' constrctGraph.
##' @param commodity The commodities
##' 
##' @return This function is not currently used.
##' 
##' @export
##' 

findConnectedGraph = function(graph, commodity){
    dist = shortest.paths(graph = graph, v = V(graph)[commodity],
        mode = "in")
    if(any(!commodity %in% V(graph)$name))
       stop("Commodity not found in graph")
    connectedNodes =
        names(which(colSums(dist != Inf) != 0))
    print(connectedNodes)
    if(length(connectedNodes) > 1){
        connectedGraph =
            induced.subgraph(graph, vids = V(graph)[connectedNodes])
    } else {
        connectedGraph =
            graph.data.frame(d = data.frame(from = "000",
                                 to = commodity))
        connectedGraph = connectedGraph - vertices("000")
    }
    connectedGraph
}
