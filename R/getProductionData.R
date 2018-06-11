##' Get Production Data
##' 
##' Function to obtain production data at primary level
#' @import  faosws
#' @export getProductionData


getProductionData = function(areaVar,itemVar,yearVar,elementVar){
  productionKey <- faoswsUtil::getCompleteImputationKey(table = "production")
  
  milk=ReadDatatable("animal_milk_correspondence")
  milk=milk[,milk_item_cpc]
  
  productionKey@dimensions$measuredItemCPC@keys=c(productionKey@dimensions$measuredItemCPC@keys,milk )
  
  
  productionKey@dimensions$measuredElement@keys <- "5510"
  
  ## create keys for data retrieval
  # productionKey = DatasetKey(
  #   domain = "agriculture",
  #   dataset = "aproduction",
  #   dimensions = list(
  #     Dimension(name = "geographicAreaM49",
  #               keys = GetCodeList(domain = "agriculture",
  #                                  dataset = "aproduction",
  #                                  dimension = "geographicAreaM49")[type == "country", code]),
  #     Dimension(name = "measuredElement", keys = c("5510")), 
  #     Dimension(name = "timePointYears", keys = as.character(1990:2016)),
  #     Dimension(name = "measuredItemCPC",
  #               keys = GetCodeList(domain = "agriculture",
  #                                  dataset = "aproduction",
  #                                  dimension = "measuredItemCPC")[, code]))
  # )
  # 

  
  ## Pivot to vectorize yield computation
  productionPivot = c(
    Pivoting(code = areaVar, ascending = TRUE),
    Pivoting(code = itemVar, ascending = TRUE),
    Pivoting(code = yearVar, ascending = FALSE),
    Pivoting(code = elementVar, ascending = TRUE)
  )
  
  ## Query the data
  productionQuery = GetData(
    key = productionKey,
    flags = TRUE,
    normalized = FALSE,
    pivoting = productionPivot
  )
  
  
  ## Convert time to numeric
  productionQuery[, timePointYears := as.numeric(timePointYears)]
  
  productionQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
  
  
  
  productionQuery
  
  
  #   productionQuery%>%
  #     distinct(measuredItemCPC)
}
