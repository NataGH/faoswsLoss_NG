##' Get Data
##' 
##' This function provides an interface between an R session and the database. 
##' Note that swsContext files must exist in your session, so you should run 
##' GetTestEnvironment before calling this function.
##' 
##' NOTE: This function will either pull directly from the database or from the
##' local session you set up.  This is important, as analysts may have changed
##' values within a local session and may then execute GetData.  To avoid
##' overwriting the values they updated, you should make sure to include the
##' session ID in the key argument.  This is fairly easy to do: when
##' constructing the DatasetKey, simply pass sessionId =
##' slot(swsContext.datasets[[1]], "sessionId") as an argument.
##' 
##' If the denormalized value is not set then the data is normalized; if set the
##' data is denormalized along the axis specified.
##' 
##' If the pivoting vector is present, the dimensions are extracted in the 
##' specified order and applying the requested sort direction. This affects both
##' normalized and denormalized extractions. For normalized extractions only the
##' order of the dimensions is influenced, while for denormalized extraction the
##' effect is more evident since the last dimension specified in the pivoting
##' vector gets its values developed along the column of the generated
##' data.table result object.
##' 
##' @param key An object of class DatasetKey.  Often, this will be one of the 
##'   list elements of swsContext.datasets (if running in a debug/local session,
##'   create this object with GetTestEnvironment).
##' @param flags Logical, indicating if flags should be returned (TRUE) 
##'   otherwise not returned (FALSE).
##' @param normalized Logical, if true then data are returned in normalized 
##'   format, otherwise the format is denormalized.
##' @param pivoting A vector, each of whose elements must be an object of type 
##'   Pivoting.  If omitted, no pivoting is performed on the dataset.  Using
##'   this argument can allow for convenient reshaping of the data prior to
##'   pulling it into R.  Note: if this argument is included, then all of the
##'   dimensions in key must be included in this vector.  See ?Pivoting for a
##'   description on creating this argument and for some examples on how to use
##'   it.
##'   
##' @return A data table containing the data matching the key (may be empty).
##'   
##' @examples
##' \dontrun{
##' # swsContext files are necessary for GetData to run (token may need to be updated)
##' GetTestEnvironment(
##'    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##'    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##' )
##' 
##' # Use GetCodeList to find all countries and commodities
##' areaCodes = GetCodeList("agriculture", "aproduction", "geographicAreaM49")
##' itemCodes = GetCodeList("agriculture", "aproduction", "measuredItemCPC")
##' 
##' # Pull data for one country and all commodities
##' dim1 = Dimension(name = "geographicAreaM49", keys = "12")
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = itemCodes[, code])
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "aproduction",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetData(key)
##' 
##' # Pull data for all countries and one commodity
##' dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[, code])
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "aproduction",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetData(key)
##' }
##' 
##' @export GetData

GetData <- function(key, flags = TRUE, normalized = TRUE, pivoting) {
  
  # Validate passed arguments.
  #
  GetData.validate(key, flags, normalized, metadata = FALSE, pivoting)
  
  # Prepare JSON for REST call.
  #
  json <- GetData.buildJSON(key, flags, normalized = TRUE, metadata = FALSE, pivoting)
  
  # Perform REST call.
  #
  url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.executionId) 
  data <- PostRestCall(url, json)

  # Create result data table.
  #
  query <- GetData.NEW_processNormalizedResult(data, flags)
  if(!missing(pivoting)){
    colOrder <- sapply(pivoting, function(x) x@code)
    newKeyDefinitions <- as.list(colOrder)
    data$keyDefinitions <- sapply(newKeyDefinitions, function(x){
        position <- sapply(data$keyDefinitions, function(y) y[["code"]] == x)
        newKeyDefinitions <- data$keyDefinitions[position]
    })
    colOrder <- c(colOrder, colnames(query)[!colnames(query) %in% colOrder])
    setcolorder(query, colOrder)
  }
  if(!normalized){
    query <- denormalizeResult(data, query, key)
  }

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



## ---------------------------------------------------------

GetData.NEW_processNormalizedResult <- function(data, flags) {
  keyNames <- sapply(data$keyDefinitions, function(x) x[1])
  if(flags){
    flagNames <- sapply(data$flagDefinitions, function(x) x[1])
    if(length(flagNames) == 0){
      flags <- FALSE
      flagNames <- c()
      warning("flags set to TRUE but no flags are available in this ",
              "dataset.  Setting flags to FALSE and proceeding.")
    }
  } else {
    flagNames <- c()
  }
  columns <- c(keyNames, "Value", flagNames)
  ## Drop any additional data (i.e. metadata)
  data$data <- lapply(data$data, function(x) x[1:length(columns)])
  out <- data.table(do.call("rbind", data$data))
  ## Columns are of type list, convert to vector
  ## lapply(out, class)
  out = as.data.table(lapply(out, unlist))
  if(nrow(out) > 0){
    ## lapply(out, class)
    setnames(out, columns)
    setcolorder(out, c(keyNames, "Value", flagNames))
  } else {
    out = data.table(matrix(nrow = 0, ncol = length(keyNames) +
                                length(flagNames) + 1))
    setnames(out, c(keyNames, "Value", flagNames))
    # By default R makes empty cols logical. This replaces makes all key columns
    # characters
    for (col in keyNames) {
      set(out, j = col, value = as.character(out[[col]]))
    }
  }
  out[, Value := as.numeric(Value)]
  out
}

denormalizeResult <- function(data, query, key){
  # If flags is FALSE, data$flagDefinitions will be NULL and so flagNames will
  # be an empty list
  noData = nrow(query) == 0 # Need this logical for later flow structure.
  flagNames <- sapply(data$flagDefinitions, function(x) x[1])
  keyNames <- sapply(data$keyDefinitions, function(x) x[1])
  normalizedKeys <- keyNames[-length(keyNames)]
  denormalizedKey <- keyNames[length(keyNames)]
  denormalizedPosition <- lapply(key@dimensions, function(x) x@name) ==
      denormalizedKey
  denormalizedValues <- sort(key@dimensions[denormalizedPosition][[1]]@keys)
  
  # If one of the denormalized values never has any data, it won't show up in
  # the data with missing values.  We want a column for each value, though,
  # whether it has data or not.  So, add them in:
  missingKeys <- denormalizedValues[!denormalizedValues %in%
                                        query[[denormalizedKey]]]
  toMerge = data.table(missingKeys)
  setnames(toMerge, denormalizedKey)
  query = rbindlist(list(query, toMerge), fill = TRUE)
  
  castFormula <- paste0(paste(normalizedKeys, collapse = "+"),
                        "~", keyNames[length(keyNames)])
  ## Specify which variables we wish to have in the denormalization.  This
  ## should include all the flags as well as the Value column.
  value.vars <- c("Value", flagNames)
  ## For each element of value.vars, denormalize the data across the last key
  denormalizedTables <- lapply(value.vars, function(valName){
    valueTable <- dcast.data.table(query, formula = castFormula,
                                   value.var = valName)
    keyColNames <- colnames(valueTable)[!colnames(valueTable) %in% keyNames]
    setnames(valueTable, keyColNames,
             paste0(valName, "_", keyNames[length(keyNames)],
                    "_", keyColNames))
    valueTable
  })
  # Merge all the tables together
  mergeFunc <- function(x, y){
    #Unname is a hack fix to circumvent a bug in data.table 1.9.6 - https://github.com/Rdatatable/data.table/issues/1352 
      merge(x, y, by = unname(normalizedKeys))
  }
  out <- Reduce(mergeFunc, denormalizedTables)

  # Order the output so that we have normalized keys followed by columns for
  # each denormalized key.  Keep the values and flags together for the same
  # denormalized value, if applicable.
  setcolorder(out, c(normalizedKeys,
                     paste0(c("Value", flagNames), "_",
                            denormalizedKey, "_",
                            # repeat the denormalized values for the Value and
                            # each flag column:
                            rep(denormalizedValues, each = length(flagNames) + 1))))

  # Note: If the normalized data.frame has no rows, then the denormalized should
  # also have 0 rows.  However, the above code will create one row of NAs.  To
  # fix that one special case, just delete that row.
  if(noData){
    out = out[-1, ]
  }
  
  return(out)
}

GetData.NEW_processNormalizedResultMetadata <- function(data) {
  keyNames <- sapply(data$keyDefinitions, function(x) x[1])
  cols <- c(keyNames, "Metadata_Language", "Metadata", "Metadata_Group", "Metadata_Value")
  result = lapply(data$data, function(listElement) {
    if (length(listElement[[length(keyNames)+2]]) > 0) {
      meta1 = lapply(listElement[[length(keyNames)+2]], function(listElement) {
        meta2 = lapply(listElement[[4]], function(listElement) {
          out = data.frame(list(listElement[[1]], 0, listElement[[3]]))
          colnames(out) = c(cols[(length(keyNames)+2):length(cols)])
          return(out)
        })
        lapply(1:length(meta2), function(i) {
          meta2[[i]]$Metadata_Group <<- i
        })
        out = data.frame(list(listElement[[3]]))
        out = merge(out, do.call(rbind, meta2))
        colnames(out) = c(cols[(length(keyNames)+1):length(cols)])
        return(out)
      })
      out = data.frame(listElement[1:length(keyNames)])
      out = merge(out, do.call(rbind, meta1))
      colnames(out) = cols
      return(out)
    } else {
      return(NULL)
    }
  })
  result = do.call("rbind", result)
  result = data.table(result)
  setcolorder(result, c(keyNames, "Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Value"))
}

## ---------------------------------------------------------





##' Get Metadata
##' 
##' This function provides an interface between an R session and the database.
##' Note that swsContext files must exist in your session, so you should run
##' GetTestEnvironment before calling this function.
##' 
##' If the pivoting vector is present, the dimensions are extracted in the
##' specified order and applying the requested sort direction.
##' 
##' @param key An object of class DatasetKey.  Often, this will be one of the
##' list elements of swsContext.datasets (if running in a debug/local session,
##' create this object with GetTestEnvironment).
##' @param pivoting A vector, each of whose elements must be an object of type
##' Pivoting.  If omitted, no pivoting is performed on the dataset.  Using this
##' argument can allow for convenient reshaping of the data prior to pulling it
##' into R.  Note: if this argument is included, then all of the dimensions in
##' key must be included in this vector.  See ?Pivoting for a description on
##' creating this argument and for some examples on how to use it.
##' 
##' @return A data table containing the metadata matching the key (may be empty).
##' 
##' @examples
##' \dontrun{
##' # swsContext files are necessary for GetData to run (token may need to be updated)
##' GetTestEnvironment(
##'    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
##'    token = "7823c00b-b82e-47bc-8708-1be103ac91e4"
##' )
##' 
##' # Use GetCodeList to find all countries and commodities
##' areaCodes = GetCodeList("agriculture", "aproduction", "geographicAreaM49")
##' itemCodes = GetCodeList("agriculture", "aproduction", "measuredItemCPC")
##' 
##' # Pull data for one country and all commodities
##' dim1 = Dimension(name = "geographicAreaM49", keys = "12")
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = itemCodes[, code])
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "aproduction",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetMetadata(key)
##' 
##' # Pull data for all countries and one commodity
##' dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[, code])
##' dim2 = Dimension(name = "measuredElement", keys = "5510")
##' dim3 = Dimension(name = "measuredItemCPC", keys = "0111")
##' dim4 = Dimension(name = "timePointYears", keys = as.character(2000:2013))
##' key = DatasetKey(domain = "agriculture", dataset = "aproduction",
##'                  dimensions = list(dim1, dim2, dim3, dim4))
##' GetMetadata(key)
##' }
##' 
##' @export GetMetadata

GetMetadata <- function(key, pivoting) {
  
  # Validate passed arguments.
  #
  GetData.validate(key, flags = FALSE, normalized = TRUE,
                   metadata = TRUE, pivoting)
  
  # Prepare JSON for REST call.
  #
  json <- GetData.buildJSON(key, flags = FALSE, normalized = TRUE,
                            metadata = TRUE, pivoting)
  
  # Perform REST call.
  #
  url <- paste0(swsContext.baseRestUrl, "/r/data/", swsContext.executionId)
  data <- PostRestCall(url, json)
  
  # Create result data table.
  #
  query <- GetData.NEW_processNormalizedResultMetadata(data = data)
  
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

GetData.validate <- function(key, flags, normalized, metadata, pivoting) {
  
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
  
  # Validate that at least one key per dimension has been specified.
  #
  for(d in key@dimensions) {
    if(is.null(d@keys)) {
      stop(paste("The passed dimension", d@name, "has a null key array. It is necessary to specify at least one key for every dimension of the target dataset."))
    }
    if(length(d@keys) == 0) {
      stop(paste("The passed dimension", d@name, "has an empty key array. It is necessary to specify at least one key for every dimension of the target dataset."))
    }
  }
  
  # Validate pivoting, if present.
  #
  if(!missing(pivoting)) {
    if(!is.list(pivoting))
      stop("The pivoting argument must be a list of Pivoting objects.")
    for(p in pivoting) {
      if(class(p) != "Pivoting") {
        stop("At least one of the objects in the list passed for the pivoting argument is not an instance of the Pivoting class.")
      }
      if(!validObject(p)) {
        stop("At least one of the objects in the list passed for the pivoting argument is not valid.")
      }
    }
    dimensionNames = sapply(key@dimensions, slot, "name")
    pivotNames = sapply(pivoting, slot, "code")
    if(!setequal(pivotNames, dimensionNames))
      stop("pivoting must contain all the same elements as dimensions (specified in key), and no more.")
  }
  
  # Denormalized format with metadata is not supported.
  #
  if(!normalized && metadata) {
    stop("Denormalized data format with metadata is not supported.")
  }
}


GetData.buildJSON <- function(key, flags, normalized, metadata, pivoting) {
  
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
  
  # Add parameter controlling the inclusion of flags.
  #
  json[["includeFlags"]] <- flags
  
  # Add parameter controlling the inclusion of metadata.
  #
  json[["includeMetadata"]] <- metadata
  
  # Add parameter used to request normalized or denormalized data.
  #
  json[["denormalized"]] <- !normalized
  
  json
}


GetData.processNormalizedResult <- function(data, flags) {
  keyNames <- sapply(data$keyDefinitions, function(x) x[1])
  if(flags){
    flagNames <- sapply(data$flagDefinitions, function(x) x[1])
    if(length(flagNames) == 0){
      flags = FALSE
      warning("flags set to TRUE but no flags are available in this ",
              "dataset.  Setting flags to FALSE and proceeding.")
    }
  }
  rows <- lapply(data$data, function(listElement){
    out <- data.table(Value = listElement$value)
    out[, c(keyNames) := as.list(listElement$keys)]
    if(flags){
      out[, c(flagNames) := as.list(listElement$flags)]
      ## Reorder columns
      setcolorder(out, c(keyNames, "Value", flagNames))
    } else {
      ## Reorder columns
      setcolorder(out, c(keyNames, "Value"))
    }
  })
  do.call("rbind", rows)
}

##' Clean metadata
##' 
##' This function takes a metadata object as created by a call to PostRestCall
##' and restructures it into a data.table object.
##' 
##' @param metadata The list created by the PostRestCall.
##' 
##' @return A data.table object containing the metadata values extracted
##' from the list.
##' 

cleanMetadata <- function(metadata){
  result <- lapply(metadata, function(x){
    ## Multiple elements may exist for each metadata record.  These can
    ## just be combined using an "rbind".
    out <- data.table(do.call("rbind", x$elements))
    out[, language := x$language]
  })
  lapply(1:length(result), function(i){
    result[[i]][, Metadata_Group := i]
  })
  do.call("rbind", result)
}

GetData.processNormalizedResultMetadata <- function(data){
  keyNames <- sapply(data$keyDefinitions, function(x) x[1])
  rows <- lapply(data$data, function(listElement){
    out <- cleanMetadata(listElement$metadata)
    out[, c(keyNames) := as.list(listElement$keys)]
    out[, typeDescription := NULL] # not needed
    ## Reassign column name for consistency
    setnames(out, c("typeCode", "value", "language"),
             c("Metadata", "Metadata_Value", "Metadata_Language"))
    ## Reorder columns
    setcolorder(out, c(keyNames, "Metadata", "Metadata_Language",
                       "Metadata_Group", "Metadata_Value"))
  })
  do.call("rbind", rows)
}

GetData.processDenormalizedResult <- function(data) {
  
  
  columns <- list()
  # Extract grouping key columns.
  #
  i <- 0
  for(col in data$groupingKeyDefinitions) {
    i <- i + 1
    columns[[col["code"]]] <- sapply(data$data, function(x) { x[["groupingKeys"]][i] })
  }
  
  # Extract denormalized column keys.
  #
  denormalizedDimension <- data$columnKey$definition[["code"]]
  i <- 0
  for(col in data$columnKey$codes) {
    i <- i + 1
    columns[[paste0("Value_", denormalizedDimension, "_", col)]] <- sapply(data$data, function(x) { 
      y <- x[["content"]][[i]][["value"]] 
      ifelse(is.null(y), NA, y)
    })
    
    # Extract flag columns.
    #
    j <- 0
    for(flag in data$flagDefinitions) {
      j <- j + 1
      columns[[paste0(flag["code"], "_", denormalizedDimension, "_", col)]] <- sapply(data$data, function(x) { x[["content"]][[i]][["flags"]][j] })
    }
  }
  
  # Bind columns into a data table object.
  #
  do.call("data.table", columns)
}
