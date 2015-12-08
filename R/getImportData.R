##' Get Import Data
##' 
##' Function to obtain import data at primary level
##'  
##' @export


getImportData = function(){
  
  measuredItemFS = ReadDatatable(table = "loss_food_group") %>%
    select(measured_item_fs,food_general_group,measured_item_cpc) %>%
    filter(food_general_group == "primary")
  
  
  importQuery = data.table(sws_query(area = 1:299, 
                                     item = as.character(measuredItemFS$measured_item_fs), 
                                     ele = 61, 
                                     year = 1961:2015, 
                                     value.names = F
  ))
  
  
  
  setnames(importQuery, 
           old = c(names(importQuery)),
           new = c("geographicAreaFS","measuredItemFCL","measuredElement","timePointYears",
                   "Value_measuredElement_5600","flagObservationStatus_measuredElement_5600")
  )
  
  importQuery = 
    importQuery%>%
    select(geographicAreaFS,measuredItemFCL,timePointYears,
           Value_measuredElement_5600,flagObservationStatus_measuredElement_5600)
  
  
  ## Convert measuredItemFCL to measuredItemCPC
  #   importQuery[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
  
  ## Convert geographicAreaFS to geographicAreaM49
  #   importQuery[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
  
  
  ## Adding headings to FCL codes
  importQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  
  ## Convert time to numeric
  importQuery[, timePointYears := as.numeric(timePointYears)]
  
  importQuery
  
}
