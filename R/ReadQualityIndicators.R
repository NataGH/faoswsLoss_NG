#' Read Quality Indicators from dataset
#' 
#' These functions allow reading and writing of quality indicators, Quality
#' indicators are supplied as lists.
#' 
#' @return A list with the following structure
#'   \itemize{
#'     \item \code{id} Unique id of this version of indicators
#'     \item \code{version} Version of the schema to which indicators adhere
#'     \item \code{model} Name of the schema, in this case always 'sws-indicators'
#'     \item \code{target} The metadata target. For our purposes, the dataset name
#'     \item \code{content} A nested list containing the quality indicators
#'     \item \code{createdOn} Unix timestamp of point where indicators were first 
#'       created
#'     \item \code{lastModifiedOn} Unix timestamp of point when indicators were 
#'       last modified
#'     \item \code{createdBy} User who first created indicators
#'     \item \code{lastModifiedBy} User who last modified indicators
#'   }
#' 
#' @param dataset character. Name of an SWS dataset
#' 
#' @examples \dontrun{ReadQualityIndicators("aproduction")}
#'   
#' @export ReadQualityIndicators

ReadQualityIndicators <- function(dataset){
  
  stopifnot(is.character(dataset), length(dataset) == 1L)
  
  url <- paste0(swsContext.baseRestUrl, "/api/metadata/instance/dataset/", dataset, "/sws-indicators/latest?_xid=", swsContext.token)
  quality_indicators <- GetRestCall(url, nullValue = NA)
  quality_indicators[["content"]] <- as.list(quality_indicators[["content"]])
  
  # Find objects that should be tables
  quality_indicators[["content"]] <- FormatIndicatorList(quality_indicators[["content"]])
  
  quality_indicators
  
}

FormatIndicatorList <- function(indicatorList){
  
  new_indicatorList <- indicatorList
  
  if(is.atomic(new_indicatorList)) return(new_indicatorList)
  
  if(any(vapply(new_indicatorList, Negate(is.atomic), logical(1)))) {
    return(lapply(new_indicatorList, FormatIndicatorList))
  }
  
  can_data.table <- function(x){all(all(vapply(x, is.atomic, logical(1))) &
                                      length(x) > 1 &
                                      all(diff(lengths(x)) == 0L))}
  
  #table_candidate_names <- names(Filter(can_data.table, new_indicatorList[["content"]]))
  if(can_data.table(new_indicatorList)){
    new_indicatorList <- as.data.table(new_indicatorList, stringsAsFactors = FALSE)
  }
  
  new_indicatorList
  
}

lengths <- function(x, use.names = TRUE){
  if(getRversion() >= "3.2.0"){
    return(base::lengths(x, use.names))
  } else {
    vapply(x, length, 1L, USE.NAMES = use.names)
  }
}
