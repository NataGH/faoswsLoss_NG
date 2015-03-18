##' Build Edges
##' 
##' Function to merge shares and extraction rate and build the
##' relation graph.
##'
##' @param dataList A list of AUPUS datasets to be analyzed, typically as
##' produced by the function getAupusDataset.  For more details on this
##' argument, please see ?getAupusDataset.
##' 
##' @return A data.table object with the same keys as dataList$inputData
##' (typically geographicAreaFS, measuredItemParentFS, measuredItmChildFS, and
##' timePointYearsSP).  Additional columns are Value_share (shares as specified
##' in dataList$shareData), Value_extraction (extraction rates as specified in
##' dataList$extractionRateData), and Value_input, flagFaostat_input (as
##' specified in dataList$inputData).
##' 
##' @export
##' 


## NOTE (Michael): The extraction rate should only apply to process
##                 commodity.

buildEdges = function(dataList){
    
    ## TODO (Michael): Need to check this, if there are no extraction
    ##                 rates then probably the defaults are filled
    ##                 in. Also need to check how the input from
    ##                 processing data base is built.
    edgeData =
        Reduce(function(x, y){
            merge(x, y, all = FALSE, by = intersect(colnames(x), colnames(y)))
          }, x = dataList[c("shareData", "extractionRateData", "inputData")])
    setkeyv(edgeData, key(dataList$shareData))
    edgeData
}
