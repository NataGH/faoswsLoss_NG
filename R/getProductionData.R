##' Get Production Data
##' 
##' Function to obtain production data at primary level
##' 
##' @export


getProductionData = function(){
  
  allCountries =
    GetCodeList(domain = "agriculture",
                dataset = "aproduction",
                dimension = "geographicAreaM49")[type == "country", code]
  
  
  productionKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = areaVar,
                keys = allCountries),
      Dimension(name = elementVar,
                keys = "5510"),
      Dimension(name = itemVar,
                keys = as.character(requiredItems$measuredItemCPC)),
      Dimension(name = yearVar,
                keys = selectedYear)
    )
  )
  
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
  
  
  ## Convert geographicAreaM49 to geographicAreaFS
  productionQuery[, geographicAreaFS := as.numeric(faoswsUtil::m492fs(as.character(geographicAreaM49)))]
  
    ## Convert measuredItemCPC to measuredItemFCL
  productionQuery[, measuredItemFCL := faoswsUtil::cpc2fcl(as.character(measuredItemCPC),returnFirst = TRUE)]
  
  
  ## Convert time to numeric
  productionQuery[, timePointYears := as.numeric(timePointYears)]
  
  productionQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
  
  
  
  productionQuery
  
  
  #   productionQuery%>%
  #     distinct(measuredItemCPC)
}
