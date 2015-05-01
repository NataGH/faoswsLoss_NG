##' Save Population Data
##' 
##' @param populationData The population data to write back to the SWS.
##' 
##' @return No data is returned, but the population data.set is written back
##' to the SWS.
##' 
##' @export
##' 

SavePopulationData = function(populationData){
    updatedPopulation = copy(populationData)
    updatedPopulation[, measuredItemFS := "1"]
    updatedPopulation[, timePointYearsSP := as.character(timePointYearsSP)]
    setnames(updatedPopulation, "timePointYearsSP", "timePointYears")
    ## Set the names back to 'measuredElementFS' instead of 'population'
    setnames(updatedPopulation, old = colnames(updatedPopulation),
             new = gsub("population", "measuredElementFS",
                 colnames(updatedPopulation)))
    setcolorder(updatedPopulation, c("geographicAreaFS", "measuredItemFS",
                                     "timePointYears",
                                     "Value_measuredElementFS_11",
                                     "flagFaostat_measuredElementFS_11",
                                     "Value_measuredElementFS_21",
                                     "flagFaostat_measuredElementFS_21"))
    SaveData(domain = "faostat_one", dataset = "FS1_SUA",
             data = updatedPopulation, normalized = FALSE)
}