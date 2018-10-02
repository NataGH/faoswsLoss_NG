##' Get Fish Production Data
##' 
##' Function to obtain production data at primary level
#' @import  faosws
#' @export getProductionData


getFishProductionData = function(areaVar,itemVar,yearVar,elementVar, selectedYear){

keyCapture = DatasetKey(domain = "Fisheries", dataset = "capture", 
                        dimensions = list(
                                geographicAreaM49_fi = Dimension(
                                  name = "geographicAreaM49_fi", 
                                  keys =  GetCodeList("Fisheries", "capture","geographicAreaM49_fi" )[,code]),
                                fisheriesAsfis = Dimension(
                                  name = "fisheriesAsfis", 
                                  keys = GetCodeList("Fisheries", "capture","fisheriesAsfis" )[,code]),
                                fisheriesCatchArea = Dimension(
                                  name = "fisheriesCatchArea", 
                                  keys = GetCodeList("Fisheries", "capture","fisheriesCatchArea" )[,code]),
                                measuredElement = Dimension(
                                  name = "measuredElement", keys = c("FI_001")),
                                timePointYears = Dimension(
                                  name = "timePointYears",
                                  keys = c(as.character(min(as.numeric(selectedYear)):max(as.numeric(selectedYear))))) 
                                )
)

captureProduction=GetData(keyCapture) 

keyAquaculture = DatasetKey(domain = "Fisheries", dataset = "aqua", 
                            dimensions = list(
                              geographicAreaM49_fi = Dimension(
                                name = "geographicAreaM49_fi", 
                                keys =  GetCodeList("Fisheries", "aqua","geographicAreaM49_fi" )[,code]),
                              fisheriesAsfis = Dimension(
                                name = "fisheriesAsfis", 
                                keys = GetCodeList("Fisheries", "aqua","fisheriesAsfis" )[,code]),
                              fisheriesProductionSource = Dimension(
                                name = "fisheriesProductionSource", 
                                keys = GetCodeList("Fisheries", "aqua","fisheriesProductionSource" )[,code]),
                              fisheriesCatchArea = Dimension(
                                name = "fisheriesCatchArea", 
                                keys = GetCodeList("Fisheries", "aqua","fisheriesCatchArea" )[,code]),
                              measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
                              timePointYears = Dimension(
                                name = "timePointYears", 
                                keys = c(as.character(min(as.numeric(selectedYear)):max(as.numeric(selectedYear)))))
                              )
)

aquaculturaProduction=GetData(keyAquaculture)


}