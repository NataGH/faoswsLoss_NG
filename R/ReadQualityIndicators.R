#' Read Quality Indicators from dataset
#' 
#' These functions allow reading and writing of quality indicators, Quality indicators are supplied as lists.
#' 
#' @param domain
#' @param dataset
#' 
#' @export ReadQualityIndicators

ReadQualityIndicators <- function(dataset){
  
  url <- paste0(swsContext.baseRestUrl, "/api/metadata/instance/dataset/", dataset, "/latest?_xid=", swsContext.token)
  quality_indicators <- GetRestCall(url)
  quality_indicators[["content"]] <- as.list(quality_indicators[["content"]])
  
  quality_indicators
  
}
