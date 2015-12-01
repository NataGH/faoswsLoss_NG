#' Get Official Food Loss Data
#'
#' Function to obtain official food loss data at primary level 
#'

getOfficialLossData = function(){
  
  source("sws_query.r")
  
  measuredItemFS = GetTableData(schemaName = "ess", tableName = "loss_food_group") %>%
    select(measured_item_fs,food_general_group,measured_item_cpc) %>%
    filter(food_general_group == "primary")
  
  
  lossQuery = data.table(sws_query(area = 1:299, 
                                   item = as.character(measuredItemFS$measured_item_fs), 
                                   ele = 121, 
                                   year = 1961:2015, 
                                   value.names = F,
                                   class.path="ojdbc14.jar"
  ))
  
  
  setnames(lossQuery, 
           old = c(names(lossQuery)),
           new = c("geographicAreaFS","measuredItemFCL","measuredElement","timePointYears",
                   "Value_measuredElement_5120","flagObservationStatus_measuredElement_5120")
  )
  
  lossQuery = 
    lossQuery%>%
    select(geographicAreaFS,measuredItemFCL,timePointYears,
           Value_measuredElement_5120,flagObservationStatus_measuredElement_5120)
  
  
  ## Convert measuredItemFCL to measuredItemCPC
  #   lossQuery[, measuredItemCPC := faoswsUtil::cpc2fcl(as.character(measuredItemFCL))]
  
  ## Convert geographicAreaFS to geographicAreaM49
  #   lossQuery[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
  
  
  ## Convert time to numeric
  lossQuery[, timePointYears := as.numeric(timePointYears)]
  
  ## Taking only official data
  lossQuery = lossQuery[flagObservationStatus_measuredElement_5120 == " ", ]
  
  ## Adding headings to FCL codes
  lossQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  
  
}
