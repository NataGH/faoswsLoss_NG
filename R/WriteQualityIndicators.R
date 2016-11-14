#' Write Quality Indicators to dataset
#' 
#' @param indicators list. List of indicators to modify or add.
#' @param dataset character. Name of dataset to which to write Quality 
#'   indicators
#'   
#' @details The dataset may be supplied as a nested list like the \code{content}
#'   section received from \code\link{ReadQualityIndicators}. The list should 
#'   contain Quality Indicators that you wish to add or modify. Additions and 
#'   modifications occur at the top level of the list.
#'   
#'   In order to delete a value, simply have a top-level list element named as 
#'   NULL.
#'   
#' @seealso \link{ReadQualityIndicators}
#' @export WriteQualityIndicators

WriteQualityIndicators <- function(indicators, dataset){

  stopifnot(is.character(dataset), length(dataset) == 1L, is.list(indicators))

  if(length(names(indicators)) != length(indicators)){
    stop("All elements of indicators must be named")
  }

  prev <- ReadQualityIndicators(dataset)

  # If there is no existing metadata, create it
  if(length(prev) == 1L && prev == ""){

    # Remove any NULL values
    indicators <- indicators[!vapply(indicators, is.null, logical(1))]

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
    response <- PostRestCall(newurl, json, nullValue = NA)

  } else {

    # Find elements to replace, add and remove
    ## Find names to be deleted then remove them from indicators to be written
    null_names <- names(indicators[vapply(indicators, is.null, logical(1))])
    indicators[null_names] <- NULL
    ## Find names to be replaced and brand new ones to be added
    replacement_names <- intersect(names(indicators), names(prev[["content"]]))
    new_names <- setdiff(names(indicators), replacement_names)

    # Remove indicators present in both and replace them with new indicators
    new_indicators <- prev[["content"]]

    if(length(replacement_names) > 0){
    new_indicators[replacement_names] <- NULL
    }

    if(length(null_names) > 0){
      new_indicators[null_names] <- NULL
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
    response <- PutRestCall(newurl, json, nullValue = NA)
  }

  # Return modified indicators as returned by the server
  response[["content"]] <- FormatIndicatorList(response[["content"]])
  as.list(response[["content"]])

}
