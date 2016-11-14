#' Write Quality Indicators to dataset
#' 
#' @param indicators list. List of indicators to modify or add.
#' @param dataset character. Name of dataset to which to write Quality
#'   indicators
#'   
#' @details The dataset may be supplied as a nested list. The list should
#'   contain Quality Indicators that you wish to add or modify. Additions and
#'   modifications occur at the top level of the list.
#'   
#' @export WriteQualityIndicators

WriteQualityIndicators <- function(indicators, dataset){
  
  prev <- ReadQualityIndicators(dataset)
  
  # If there is no existing metadata, create it
  if(length(prev) == 1L && prev == ""){
    
    json <- list(
      model = "sws-indicators",
      version = "0.1",
      target = list(
        id = dataset,
        type = "dataset"
      ),
      content = indicators
    )
    
    newurl <- paste0(swsContext.baseRestUrl, "/api/metadata/instance/all?_xid=", swsContext.token)
    response <- PostRestCall(newurl, json)
    
  } else {
    
    # Find elements to replace and add
    replacement_names <- intersect(names(indicators), names(prev[["content"]]))
    new_names <- setdiff(names(indicators), replacement_names)
    
    # Remove indicators present in both and replace them with new indicators
    new_indicators <- prev[["content"]]
    
    if(length(replacement_names) > 0){
    new_indicators[replacement_names] <- NULL
    }
    
    new_indicators <- c(new_indicators, indicators[c(new_names, replacement_names)])
    
    json <- c(list(
      id = prev[["id"]],
      model = "sws-indicators",
      version = "0.1",
      target = list(
        id = dataset,
        type = "dataset"
      ),
      content = new_indicators
    ),
    prev[c("createdOn", "lastModifiedOn", "createdBy", "lastModifiedBy")]
    )
    
    # Send newly compiled indicators to replace the old ones
    newurl <- paste0(swsContext.baseRestUrl, "/api/metadata/instance/all/", prev[["id"]], "?_xid=", swsContext.token)
    response <- PutRestCall(newurl, json)
  }
  
  # Return modified indicators as returned by the server
  response <- FormatIndicatorList(response)
  as.list(response[["content"]])
  
}
