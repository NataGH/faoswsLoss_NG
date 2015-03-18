##' Standardize Node
##' 
##' Standardization follows the below process:
##' \itemize{
##'     \item Compute a matrix ("reverseMatrix" in the code) describing how
##'     much of a particular quantity is passed from a child to it's parent.
##'     The (i,j)-th entry of this matrix specifies a conversion factor for
##'     transferring element j quantities into element i quantities.
##'     \item Compute the matrix of required elements (column) for each
##'     commodity (row), and call this "valueMatrix".
##'     \item Compute the new value for each commodity by pre-multiplying
##'     valueMatrix by reverseMatrix and therefore aggregating children
##'     commodities into their parents.
##'     \item Lastly, save this new data back to the passed graph object, and
##'     return.
##' }
##'
##' @param graph The graph object created by the function
##' constructGraph.
##' @param workingNode The nodes to be standardized
##' @param standardizeElement The attribute of the nodes to be
##' standardized.
##' 
##' @return A graph object that has been updated according to the description
##' above.
##' 
##' @export
##' 

standardizeNode = function (graph, workingNode, standardizeElement){

    ## Get the edges and the construct the reverse matrix
    outEdges = E(graph)[from(V(graph)[workingNode])]
    ## shareMatrix/rateMatrix contains the values on the edges from parent to
    ## child, but in a matrix instead of a graph.  They are also
    ## subsetted based on the current working nodes to save memory.
    ## Divide shareMatrix by 100, as it's expressed as a percent
    shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_share") / 100
    ## Divide rateMatrix by 10000, as it's expressed as a percent and scaled
    ## within the database.
    rateMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_extraction") / 10000
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    ## reverseMatrix allows us to transfer quantities from a child to it's
    ## parent.  However, the parent values should all remain fixed, so place
    ## a value of 1 in the (i,i) element of reverseMatrix whenever i is a 
    ## parent node.
    parentNode = rownames(reverseMatrix)[!rownames(reverseMatrix) %in%
                                             workingNode]
    diag(reverseMatrix)[parentNode] = 1
    ## NA values or infinities indicate no transfer, so set to 0.
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    
    ## Extract the values which are to be standardized from the vertices
    ## of the graph.
    valueMatrix = matrix(unlist(lapply(
            X = standardizeElement,
            FUN = function(x){
                get.vertex.attribute(graph = graph, name = x,
                                     index = V(graph)[colnames(shareMatrix)])
            })),
        ncol = length(standardizeElement))

    ## Standardize children up to parents
    standardizedMatrix = reverseMatrix %*% ifelse(is.na(valueMatrix), 0, valueMatrix)

    ###########################################################################
    ## Josh: Calculations above seem off.  If we standardize back a processed
    ## value but don't remove inputFromProcessing counts, it seems that we'll
    ## be double-counting.  So, we may need to remove that here (but only for
    ## element 51).
    ## 
    ## if(TRUE){
    ##     inputFromProcessing = data.table(
    ##         parents = get.edgelist(graph)[, 2],
    ##         inputs = get.edge.attribute(graph, "Value_input")
    ##     )
    ##     inputFromProcessing = inputFromProcessing[, list(input = sum(inputs)),
    ##                                                by = parents]
    ##     standardizedMatrix = data.frame(standardizedMatrix,
    ##                         parents = rownames(standardizedMatrix))
    ##     standardizedMatrix = merge.data.frame(standardizedMatrix,
    ##                                           inputFromProcessing, all.x = TRUE)
    ##     standardizedMatrix$X1 = standardizedMatrix$X1 - standardizedMatrix$input
    ##     standardizedMatrix = as.matrix(standardizedMatrix[, paste0("X", 1:8)])
    ##     standardizedMatrix[is.na(standardizedMatrix)] = 0
    ##     colnames(standardizedMatrix) = NULL
    ##     rownames(standardizedMatrix) = rownames(reverseMatrix)
    ## }
        
    ## Save the target value back to the graph, and the intermediate
    ## to the intermediate value matrix
    for(i in 1:length(standardizeElement)){
        graph = set.vertex.attribute(graph = graph,
                 name = standardizeElement[i],
                 index = V(graph)[rownames(standardizedMatrix)],
                 value = standardizedMatrix[, i])
    }

###############################################################################
##### NOTE (Josh): The intermediateValueMatrix computed here was previously
#     giving values because the above for loop wasn't working (values were
#     not being assigned and the graph would be processed but not updated.
#     Once the graph is updated, all the child commodities have an
#     "intermediate value" of 0, and so I have no idea what this matrix
#     (below) would tell us.
###############################################################################
#     intermediateValuesMatrix =
#         matrix(unlist(lapply(X = standardizeElement,
#                              FUN = function(x){
#                                  get.vertex.attribute(graph = graph, name = x,
#                                                       index = V(graph)[workingNode])
#                              })),
#                ncol = length(standardizeElement))
#     rownames(intermediateValuesMatrix) = workingNode

    ## Delete the standardized workingNodes
    graph = graph - vertices(workingNode)

    ## Return the objects
#     list(standardizedGraph = graph, intermediateValues = intermediateValuesMatrix)
     graph
}
