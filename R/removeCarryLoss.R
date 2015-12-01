#' Remove Carry Loss
#'
#'

removeCarryLoss = function(data, lossVar){
  data[, variance := var(.SD[[lossVar]], na.rm = TRUE),
       by = c("geographicAreaM49", "measuredItemCPC")]
  data[, duplicateValue := duplicated(.SD[[lossVar]]),
       by = c("geographicAreaM49", "measuredItemCPC")]
  data = data[!(variance == 0 & duplicateValue), ]
  data[, `:=`(c("variance", "duplicateValue"), NULL)]
  data         
}
