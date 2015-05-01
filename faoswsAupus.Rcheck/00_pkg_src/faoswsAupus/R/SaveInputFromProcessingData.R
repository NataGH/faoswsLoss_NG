##' Save Inputs from Processing Data
##' 
##' @param inputData A data.table containing the input data read from the
##' database, see ?getInputFromProcessData.  This is provided solely for
##' extraction of the appropriate column names, it isn't used in any other
##' way.
##' @param edges The edge data.table, after having been processed by the
##' AUPUS proceedure.  This is the updated data.set to write back to the
##' SWS.
##' 
##' @return No data is returned, but the edges data.set is written back to
##' the SWS.
##' 
##' @export
##' 

SaveInputFromProcessingData = function(inputData, edges){
    updatedInput = edges[, colnames(inputData), with = FALSE]
    updatedInput[, timePointYearsSP := as.character(timePointYearsSP)]
    setnames(updatedInput, old = c("Value_input", "flagFaostat_input"),
             new = c("Value", "flagFaostat"))
    SaveData(domain = "faostat_one", dataset = "input_from_proc_fs",
             data = updatedInput, normalized = TRUE)
}
