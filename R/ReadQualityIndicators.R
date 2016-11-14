#' Read Quality Indicators from dataset
#' 
#' These functions allow reading and writing of quality indicators, Quality indicators are supplied as lists.
#' 
#' @param dataset character. Name of an SWS dataset
#' 
#' @export ReadQualityIndicators

ReadQualityIndicators <- function(dataset){
  
  stopifnot(is.character(dataset), length(dataset) == 1L)
  
  url <- paste0(swsContext.baseRestUrl, "/api/metadata/instance/dataset/", dataset, "/latest?_xid=", swsContext.token)
  quality_indicators <- GetRestCall(url)
  quality_indicators[["content"]] <- as.list(quality_indicators[["content"]])
  
  # Find objects that should be tables
  quality_indicators <- FormatIndicatorList(quality_indicators)

  quality_indicators
  
}

FormatIndicatorList <- function(indicatorList){
  
  new_indicatorList <- indicatorList
  
  table_candidate_names <- names(Filter(function(x){all(all(vapply(x, is.atomic, logical(1))) & 
                                                          length(x) > 1 & 
                                                          all(diff(lengths(x)) == 0L))}, new_indicatorList[["content"]]))
  
  if(length(table_candidate_names) > 0){
    for(i in table_candidate_names){
      new_indicatorList[["content"]][[i]] <- as.data.table(new_indicatorList[["content"]][[i]])
    }
  }
  
  new_indicatorList
  
}
