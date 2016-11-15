#' Save Imputed Loss
#' 
#' @export

saveImputedLoss = function(data){
  saveSelection =
    subset(data,
           select = c("geographicAreaM49", "measuredItemCPC",
                      "timePointYears", "Value_measuredElement_5016", # 5120
                      "flagObservationStatus_measuredElement_5016"
                     ,
                      "flagMethod_measuredElement_5016" # 5120
                      )) %>%
    .[flagObservationStatus_measuredElement_5016 != ""]

  ## setnames(saveSelection,
  ##          old = grep("measuredElement", colnames(saveSelection), value = TRUE),
  ##          new = gsub("measuredElement", "measuredElementSuaFbs",
  ##                     grep("measuredElement", colnames(saveSelection), value = TRUE)))
  ## setnames(saveSelection,
  ##          new = "measuredItemSuaFbs",
  ##          old = "measuredItemCPC")
  
  ## saveSelection[, `:=` (measuredItemSuaFbs = as.character(measuredItemSuaFbs),
  ##                       geographicAreaM49 = as.character(geographicAreaM49))]

  saveSelection[, `:=` (measuredItemCPC = as.character(measuredItemCPC),
                        geographicAreaM49 = as.character(geographicAreaM49))]

  saveSelection[, `:=` (flagObservationStatus_measuredElement_5016 = "I",
                        flagMethod_measuredElement_5016 = "e")]


  ## 1) filter "" (blank) flagObservationStatus_measuredElement_5016 (official)
  ## 2) convert remaining to "I" flagObservationStatus_measuredElement_5016
  ## 3) remaining method flags become "e"

  ## saveSelection[, `:=` (flagMethod_measuredElement_5016 = "e")]
  ## u: missing
  ## GetData
  ## only assign observation flag
  ## overwrite "missing"
  ## revise processing documentation
  ## remove set that is having missing values, replace with imputed
  ## use flag combination
  ## I,e or I,m

  ## SaveData(domain = "lossWaste",
  ##          dataset = "loss",
  ##          data = saveSelection,
  ##          normalized = FALSE)

  saveSelectionNorm <- 
  faosws:::normalizeData(saveSelection, keys =   c("geographicAreaM49", "measuredItemCPC", "timePointYears"), denormalizedKey = "measuredElement")
  
 
  SaveData(domain = "agriculture",
           dataset = "aproduction",
           data = saveSelectionNorm,
           normalized = TRUE)
}
