#' Save Imputed Loss
#' 
#' 

saveImputedLoss = function(data){
  saveSelection =
    subset(data,
           select = c("geographicAreaM49", "measuredItemCPC",
                      "timePointYears", "Value_measuredElement_5120",
                      "flagObservationStatus_measuredElement_5120",
                      "flagMethod_measuredElement_5120"))
  setnames(saveSelection,
           old = grep("measuredElement", colnames(saveSelection), value = TRUE),
           new = gsub("measuredElement", "measuredElementSuaFbs",
                      grep("measuredElement", colnames(saveSelection), value = TRUE)))
  setnames(saveSelection,
           new = "measuredItemSuaFbs",
           old = "measuredItemCPC")
  
  saveSelection[, measuredItemSuaFbs := as.character(measuredItemSuaFbs)]
  
  SaveData(domain = "lossWaste",
           dataset = "loss",
           data = saveSelection,
           normalized = FALSE)
}