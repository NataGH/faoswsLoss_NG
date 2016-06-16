#' Save Imputed Loss
#' 
#' @export

saveImputedLoss = function(data){
  saveSelection =
    subset(data,
           select = c("geographicAreaM49", "measuredItemCPC",
                      "timePointYears", "Value_measuredElement_5016", # 5120
                      "flagObservationStatus_measuredElement_5016"))  # 5120
  setnames(saveSelection,
           old = grep("measuredElement", colnames(saveSelection), value = TRUE),
           new = gsub("measuredElement", "measuredElementSuaFbs",
                      grep("measuredElement", colnames(saveSelection), value = TRUE)))
  setnames(saveSelection,
           new = "measuredItemSuaFbs",
           old = "measuredItemCPC")
  
  saveSelection[, `:=` (measuredItemSuaFbs = as.character(measuredItemSuaFbs),
                        geographicAreaM49 = as.character(geographicAreaM49))]
  
  SaveData(domain = "lossWaste",
           dataset = "loss",
           data = saveSelection,
           normalized = FALSE)
}
