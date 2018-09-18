##' Get Production Data
##' 
##' Function to obtain production data at primary level
#' @import  faosws
#' @export getImportData


getImportData = function(areaVar,itemVar,yearVar, selectedYear){

  allReportersDim_tot <-
    GetCodeList("trade", "total_trade_cpc_m49", areaVar)[type == "country", code] %>%
    Dimension(name =areaVar, keys = .)
  
  allElementsDim_tot <-
    c("5608", "5609", "5610") %>%
    Dimension(name = "measuredElementTrade", keys = .)
  
  allItemsDim_tot <-
    GetCodeList("trade", "total_trade_cpc_m49", itemVar)[,code] %>%
    Dimension(name = itemVar, keys = .)
  
  allYearsDim <- Dimension(name = yearVar, keys = as.character(min(as.numeric(selectedYear)):max(as.numeric(selectedYear))))
  
  totaltradekey <-
    DatasetKey(
      domain = "trade",
      dataset = "total_trade_cpc_m49",
      dimensions =
        list(
          allReportersDim_tot,
          allElementsDim_tot,
          allItemsDim_tot,
          allYearsDim
        )
    )
  
  existing_data <- GetData(key = totaltradekey) 
  existing_data
}
