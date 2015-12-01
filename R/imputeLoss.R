#' Impute Loss Data
#' 
#' 

imputeLoss = function(data, lossVar, lossObservationFlagVar, lossMethodFlagVar,
                      lossModel){
  imputedData = copy(data)
  imputedData[, predicted := exp(predict(lossModel, newdata = imputedData,
                                         allow.new.levels = TRUE))]
  imputedData[(is.na(imputedData[[lossVar]]) |
                 imputedData[[lossObservationFlagVar]] %in% c("E", "I", "T")) &
                !is.na(predicted),
              `:=`(c(lossVar, lossObservationFlagVar, lossMethodFlagVar
              ),
              list(predicted, "I", "e"))]
  imputedData[, predicted := NULL]
  imputedData
}
