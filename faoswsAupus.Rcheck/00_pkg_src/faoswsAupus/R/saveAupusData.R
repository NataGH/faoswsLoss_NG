##' Save AUPUS Data
##' 
##' This function is designed as a wrapper around SaveData, and is specifically
##' used to save data produced from the AUPUS analysis back to the SWS.
##' 
##' @param aupusData A data.table containing the AUPUS data read from the
##' database, see ?getAupusDataset.  This is provided solely for extraction
##' of the appropriate column names, it isn't used in any other way.
##' @param nodes The data after having been processed by the AUPUS
##' proceedure.  This is the data written back to the SWS.
##' 
##' @return No data is returned, but the nodes data.set is written back to
##' the SWS.
##' 
##' @export 
##' 

## Functions to save the data back
SaveAupusData = function(aupusData, nodes){
    updatedAupus = nodes[, colnames(aupusData), with = FALSE]
    updatedAupus[, timePointYearsSP := as.character(timePointYearsSP)]
    setnames(updatedAupus, "timePointYearsSP", "timePointYears")
    SaveData(domain = "faostat_one", dataset = "FS1_SUA",
             data = updatedAupus, normalized = FALSE)
}
