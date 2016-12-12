##' Get History
##' 
##' @param key An object of class DatasetKey.  Often, this will be one of the
##' list elements of swsContext.datasets (if running in a debug/local session,
##' create this object with GetTestEnvironment).
##' @param pivoting A vector, each of whose elements must be an object of type
##' Pivoting.  If omitted, no pivoting is performed on the dataset.  Using this
##' argument can allow for convenient reshaping of the data prior to pulling it
##' into R.
##' 
##' @return A data table of observation objects containing the values and flags
##' through history with the associated history metadata.
##' 
##' @export GetHistory

GetHistory <- function(key, pivoting) {
  
  # Validate passed arguments.
  #
  GetHistory.validate(key, pivoting)
  
  # Prepare JSON for REST call.
  #
  json <- GetHistory.buildJSON(key, pivoting)
  
  # Perform REST call.
  #
  url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.executionId) 
  data <- PostRestCall(url, json)
  
  # Create result data table.
  #
  query <- GetHistory.processNormalizedResult(data)
  
  # normalizes result transforming columns from list of NULLs to vector of NAs
  as.data.table(
    lapply(query,
           FUN = function(x){
             if(is.list(x))
               x = NullToNa(x)
             x
           }
    )
  )
  
}


GetHistory.validate <- function(key, pivoting) {
  
  # Validate passed key.
  #
  if(missing(key)) {
    stop("The key argument is mandatory.")
  }
  if(class(key) != "DatasetKey") {
    stop("The passed key argument is not an instance of the DatasetKey class.")
  }
  if(!validObject(key)) {
    stop("The passed key argument is not valid.")
  }
  
  # Validate pivoting, if present.
  #
  if(!missing(pivoting)) {
    if(is.list(pivoting)) {
      for(p in pivoting) {
        if(class(p) != "Pivoting") {
          stop("At least one of the objects in the list passed for the pivoting argument is not an instance of the Pivoting class.")
        }
        if(!validObject(p)) {
          stop("At least one of the objects in the list passed for the pivoting argument is not valid.")
        }
      }
    } else {
      if(class(pivoting) != "Pivoting") {
        stop("The pivoting parameter passed is not an instance of the Pivoting class.")
      }
      if(!validObject(pivoting)) {
        stop("The pivoting parameter passed is not valid.")
      }
    }
  }
}


GetHistory.buildJSON <- function(key, pivoting) {
  
  # Build JSON for REST call.
  #
  json <- list(
    token = swsContext.token,
    domain = key@domain,
    dataSet = key@dataset)
  
  # Set up dimensions and selected keys.
  #
  json[["dimension2codes"]] <- list()
  for(d in key@dimensions) {
    json[["dimension2codes"]][[d@name]] <- I(d@keys)
  }
  
  # Add pivoting parameters, if requested.
  #
  if(!missing(pivoting) && !is.na(pivoting) && length(pivoting) > 0) {
    json[["pivotingDimensions"]] <- pivoting
  }
  
  # Add parameter controlling the inclusion of flags (always true in
  # GetHistory call).
  #
  json[["includeFlags"]] <- TRUE
  
  # Add parameter controlling the inclusion of metadata (always true
  # in GetHistory call).
  #
  json[["includeMetadata"]] <- TRUE
  
  # Request historical data.
  #
  json[["includeHistory"]] <- TRUE
  
  # Add parameter to force normalized data generation.
  #
  json[["denormalized"]] <- FALSE
  
  json
}

GetHistory.processNormalizedResult <- function(data) {
  
  keyNames <- vapply(data$keyDefinitions, `[`, character(1), 1)
  flagNames <- vapply(data$flagDefinitions, `[`, character(1), 1)
  
  cols <- c(keyNames, "Value", "Version", "StartDate", "EndDate", flagNames, "Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value")
  offset <- length(keyNames) + 4 + length(flagNames)
  
  result = lapply(data$data, function(listElement) {
    # Horrible hack to counter the fact that the list may not be the right length 
    # and to pad it with NULLs correctly
    if(length(listElement) < offset){
      listPosition <- which(vapply(listElement, is.list, logical(1)))
      
      if(length(listPosition) != 1){
        stop("The API has changed unexpectedly. Please contact the engineering
	      team to resolve the issue")
      }
      # Insert NULLs into middle rather than the end
      listElement <- c(listElement[1:(listPosition - 1 )], lapply(vector((offset + 1) - (listPosition - 1) , mode = "list"), as.null), listElement[listPosition])
    }
    currList <- listElement[1:offset]
    currMeta <- list()
    if (length(listElement) >= (offset + 1) && length(listElement[[offset + 1]]) > 0) {
      currMeta <- listElement[[offset+1]]
    }
    currList[[offset + 1]] <- currMeta
    currList <- list(currList)
    totalList <- NULL
    if (length(listElement) >= (offset + 2) && length(listElement[[offset+2]]) > 0) {
      histList = lapply(listElement[[offset + 2]], function(listElement) {
        out <- append(currList[[1]][1:length(keyNames)], listElement)
        return(out)
      })
      totalList <- append(currList, histList)
    } else {
      totalList <- currList
    }
    out = lapply(totalList, function(listElement) {
      out = NULL
      elem <- listElement[1:offset]
      lapply(1:offset, function(i) {
        if (is.null(elem[[i]])) {
          elem[[i]] <<- NA
        } 
      })
      elem <- list(elem)
      # Check if there is metadata
      if (length(listElement) >= (offset + 1) && length(listElement[[offset + 1]]) > 0) {
        ## alternatively: 
        ## if (length(listElement) > offset && length(listElement[[offset + 1]]) > 0) {
        
        meta1 = lapply(listElement[[offset + 1]], function(listElement) {
          meta2 = lapply(listElement[[4]], function(listElement) {
            out = data.frame(list(0, listElement[[1]], listElement[[3]]), stringsAsFactors = FALSE)
            colnames(out) = c(cols[(offset + 3):length(cols)])
            return(out)
          })
          lapply(1:length(meta2), function(i) {
            meta2[[i]]$Metadata_Group <<- i
          })
          out = data.frame(list(listElement[[1]], listElement[[3]]), stringsAsFactors = FALSE)
          out = merge(out, do.call(rbind, meta2))
          colnames(out) = c(cols[(offset + 1):length(cols)])
          return(out)
        })
        out = data.frame(elem, stringsAsFactors = FALSE)
        out = merge(out, do.call(rbind, meta1))
      } else {
        lapply(1:5, function(i) {
          elem[[1]][[offset + i]] <<- NA
        })
        out = data.frame(elem, stringsAsFactors = FALSE)
      }
      colnames(out) = cols
      return(out)
    })
    out = rbindlist(out)
    colnames(out) = cols
    return(out)
  })
  result = rbindlist(result)
  # If empty, don't break when trying to set col order
  if(nrow(result) == 0){
    result <- as.data.table(setNames(replicate(length(cols),logical()), cols))
  }
  setcolorder(result, c(keyNames, "Version", "StartDate", "EndDate", "Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value", "Value", flagNames))
  
  return(result[])
}
