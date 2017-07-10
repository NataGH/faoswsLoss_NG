##' Save Data
##' 
##' This is the main function used to write data back to a user's session.  The 
##' data must be in a particular format, and thus it's recommended to first read
##' data via GetData, modify the data.table, and then write it back to the 
##' database with this function.
##' 
##' Also, this function will throw an error if you attempt to write to invalid 
##' data.  For example, some countries have restrictions on which time period 
##' they have data (as they recently became a country, or are no longer a 
##' country).  See ?GetCodeList.
##' 
##' @note Saving a missing value removes the value of the destination record. If
##'   you do not wish to remove values, remove NAs from your output. If you have
##'   denormalized data, it is not possible to remove values. If you wish to do
##'   so, you need to normalize first.
##'   
##' @param domain A character value specifying the domain for which the code 
##'   list is required.
##' @param dataset A character value specifying the dataset for which the code 
##'   list is required.
##' @param data A data.table object containing keys, data and flags.
##' @param metadata A data.table object containing the metadata.  The first 
##'   columns of this object should contain the keys, and immediately following 
##'   these key columns should be a column named "Metadata" which contains the 
##'   metadata to write.
##' @param normalized Logical, indicates whether data is in a normalized or 
##'   denormalized format.list
##' @param waitMode A character string indicating how to behave with respect to 
##'   the backend.  Should be either "wait", "forget", or "synch".  "wait" 
##'   allows R to routinely check for completion of the save, up to pullTimeout 
##'   seconds. "forget" returns control to R immediately without verifying that 
##'   the object has been saved (this can be dangerous if the object is accessed
##'   later). "synch" gives the writing process a short time period, and returns
##'   an error if the write fails.
##' @param waitTimeout An integer giving the number of seconds to allow for the 
##'   write process to the database before returning an error.
##' @param chunkSize The largest number of rows to write at one time.  This 
##'   value helps prevent server timeouts, and shouldn't usually need to be 
##'   adjusted from the default value of 50,000.
##'   
##' @return A list is returned, which contains the following values (note that 
##'   "version" below refers to the version of the observation in the database):
##'   \itemize{ 
##'   \item inserted =  the number of rows inserted 'ex novo' (with 
##'   version=1: there wasn't any already existing observation for same 
##'   coordinates) 
##'   \item appended =  the number of rows inserted in append mode 
##'   (with version=max+1: there was one or more observation/s: version number 
##'   incremented) 
##'   \item ignored = the number of rows not inserted because an 
##'   already existing observation was in place with identical attributes 
##'   \item discarded = the number of new observations not created because of non 
##'   blocking errors (i.e. 'warnings' which caused the row to be discarded 
##'   without blocking other rows insertion) 
##'   \item warnings =  a data.table  which can be NULL or contain 1 to 10 rows.if value of "discarded" is > 0, 
##'   the data.table contains an excerpt of rows discarded (max 10 rows), any of
##'   which with following info: 
##'   \itemize{ 
##'   \item row = the positional row 
##'   number in the data as sent 
##'   \item reason = a description of the discarding
##'   error
##'   } 
##'   }
##'   If waitMode is 'forget', NULL is returned always
##' 
##' @section Metadata: Metadata can be optionally specified. If it is, the
##'   following fields are mandatory:
##'   \itemize{ 
##'   \item All the keys required to identify a value 
##'   \item Metadata - Metadata category 
##'   \item Metadata_Element - Metadata subcategory 
##'   \item Metadata_Language - Two letter lowercase code for language in which the
##'   metadata is in
##'   \item Metadata_Value - The actual value of the metadata
##'   }
##' 
##' @section Attempting to write invalid data: Currently, if no flags are 
##'   provided, all rows with no flags will be marked as appended and rejected. 
##'   Up to 10 warnings will be provided.
##'   
##' @details
##' 
##' In order to remove a value or flag, simply write NA in its place. Be careful
##' not to accidentally include NAs in your data if you do not wish those values
##' to be cleared.
##' 
##' @examples 
##' \dontrun{
##' d <- GetData(swsContext.datasets[[1]])
##' config <- GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
##' metad <- d[, mget(config$dimensions)]
##' 
##' metad[, `:=`(Metadata = "GENERAL",
##'              Metadata_Element = "COMMENT",
##'              Metadata_Language = "en",
##'              Metadata_Group = "1",
##'              Metadata_Value = "Test")]
##' 
##' SaveData(config$domain, config$dataset, d, metadata = metad)
##' }
##' 
##' @export SaveData

SaveData <- function(domain, dataset, data, metadata, normalized = TRUE, waitMode = "wait", waitTimeout = 600, chunkSize = 50000) {
  
  if (!(waitMode %in% c("wait", "forget", "synch"))) {
    stop(paste("invalid waitMode: ", waitMode, ", expected one among 'wait', 'forget', 'synch'"))
  }
  
  # Validate passed arguments.
  #
  SaveData.validate(domain, dataset, data, metadata, normalized)
  
  datasetConfig <- GetDatasetConfig(domain, dataset)
  
  uuid <- NULL
  baseUrl <- paste0(swsContext.baseRestUrl, "/r/data/")
  nRowData <- 0
  if (!missing(data)) {
    nRowData <- nrow(data)
  }
  nRowMeta <- 0
  if (!missing(metadata)) {
    nRowMeta <- nrow(metadata)
  }
  dataChunksCnt <- 0
  metaChunksCnt <- 0
  
  allKeys <- datasetConfig[["dimensions"]]
  if(!normalized){
    
    dKey <- SaveData.getDenormalizedKey(allKeys, data)
    data <- normalizeData(data, setdiff(allKeys, dKey), dKey, keepNA = FALSE)
    
  }
  
  #Reorganise columns to keys, value, flags and other
  flagCols <- datasetConfig[["flags"]]
  
  # If columns are missing, reject the data
  if(!all(c(allKeys, "Value", flagCols) %in% colnames(data))){
    stop("The supplied data is missing the following keys: ", 
         paste0(setdiff(c(allKeys, "Value", flagCols), colnames(data)), collapse = ", "))
  }
  
  requiredCols <- c(allKeys, "Value", flagCols)
  otherCols <- setdiff(colnames(data), requiredCols)
  
  if(length(otherCols)){
    message("The following columns were discarded: ", paste0(otherCols, collapse = ", "))
  }
  
  
  
  if(!identical(names(data), requiredCols)) {
    #If the order isn't the same, copy the data and reorganise them
    data <- copy(data)[, mget(requiredCols)]
  }
  
  SaveData.validateFlagValues(data, flagCols)
  
  if (waitMode != "synch" & (nRowData > chunkSize | nRowMeta > chunkSize)) {
    uuid <- SaveData.generateUuid()
    if (!missing(data)) {
      json <- SaveData.buildNormalizedDataDefJSON(data, datasetConfig)
      PutRestCall(paste0(baseUrl, "stream/", swsContext.executionId, "/",
                         uuid, "/data/def?token=", swsContext.token), json)
      dataChunks <- SaveData.splitIntoChunkTables(data, chunkSize)
      dataChunksCnt <- length(dataChunks)
      for (i in 1 : dataChunksCnt) {
        json <- SaveData.buildNormalizedDataContentJSON(dataChunks[[i]], datasetConfig)
        PutRestCall(paste0(baseUrl, "stream/", swsContext.executionId, "/",
                           uuid, "/data/chunk/", (i - 1), "?token=", swsContext.token),
                    json)
      }
    }
    if (!missing(metadata)) {
      PutRestCall(
        paste0(baseUrl, "stream/", swsContext.executionId, "/",
               uuid, "/meta/def?token=", swsContext.token),
        SaveData.buildMetadataDefJSON(metadata, datasetConfig[["dimensions"]]))
      metaChunks = SaveData.splitIntoChunkTables(metadata, chunkSize)
      metaChunksCnt <- length(metaChunks)
      for (i in 1 : metaChunksCnt) {
        PutRestCall(paste0(baseUrl, "stream/", swsContext.executionId, "/",
                           uuid, "/meta/chunk/", (i - 1), "?token=", swsContext.token),
                    SaveData.buildMetadataContentJSON(metaChunks[[i]]), datasetConfig[["dimensions"]])
      }
    }
    out <- PutRestCall(
      paste0(baseUrl, "exec/deferred/", swsContext.executionId, "/", domain, "/", dataset,
             "?token=", swsContext.token, "&normalized=true&waitMode=", waitMode, "&uuid=", uuid,
             "&dataChunks=", dataChunksCnt, "&metaChunks=", metaChunksCnt),
      list()
    )
  } else {
    out <- PutRestCall(
      paste0(baseUrl, "exec/inline/", swsContext.executionId, "/", domain, "/", dataset,
             "?token=", swsContext.token, "&normalized=true&waitMode=", waitMode),
      SaveData.buildUniqueJSON(data, metadata, datasetConfig)
    )
    uuid <- out[['message']]
  }
  
  ## STATUS:NOT-FOUND, STATUS:STARTED, STATUS:COMPLETED
  ## STATUS:VALID-ERRS
  ##   	VALID-ERR-1, VALID-ERR-2, ..., VALID-ERR-N
  ## 	STATUS:FAILED
  ## 		STACKTRACE
  ## 	ALL STATUSES (EXCEPT NOT-FOUND):
  ## 		STARTED-AT, EXEC-ID, USER-ID
  
  # wait mode -> has to check for execution result any 3 seconds
  if (waitMode == "wait") {
    # counts time elapsed from invocation , to stop when the timeout is exceeded
    elapsed <- 0
    # subtracted in above-mentioned check
    p1 <- proc.time()[['elapsed']]
    # it is used as condition for the subsequent loop
    statusOut <- NULL
    
    # checks until one of the two is given: 1. the timeout is not exceeded; 2. a final result is obtained
    while ((elapsed < waitTimeout) & (is.null(statusOut))) {
      
      # out[['message']] contains the UUID for the exec. status check, it invokes the get by concatenating it to the
      # previously used PUT url
      statusOut = GetRestCall(paste0(baseUrl, "status/", uuid))
      
      if (statusOut[['details']][['STATUS']] == "NOT-FOUND") {
        # this shouldn't happen: the status key is not found on the back end cache
        stop(paste("Error while executing CHECK STATUS on the SWS Server: Execution key not found in the cache"))
        
      } else if (statusOut[['details']][['STATUS']] == 'STARTED') {
        # it has to sleep and check again, for this the loop condition is set
        statusOut <- NULL
        elapsed <- (proc.time()[['elapsed']] - p1)
        Sys.sleep(3)
      }
      # for other cases (COMPLETED, FAILED, VALID-ERRS) statusOut is not null, exits the loop
    }
    
    # if it exits the loop with null statusOut , it means the timeout is elapsed without a result
    if (is.null(statusOut)) {
      stop("Timeout reached while waiting for execution to be completed")
    } else {
      # for all other cases, out is overwritten with result from the check status, evaluated below
      out <- statusOut
    }
  }
  
  statistics <- NULL
  
  # if not success can be FAILED or VALID-ERRS, provides details
  if (!out[["success"]]) {
    
    # if FAILED the stacktrace is printed
    if (out[["details"]][["STATUS"]] == "FAILED") {
      stop(paste("Error while executing SAVE DATA on the SWS Server:", out[['details']][['STACKTRACE']]))
      
      # if VALID-ERRS the list of validation details is printed
    } else if (out[["details"]][["STATUS"]] == "VALID-ERRS") {
      msg <- "Validation error/s occurred:"
      for (name in names(out[["details"]])) {
        if (grepl("VALID-ERR-", name)) {
          msg <- paste(msg, out[["details"]][[name]], sep = "\n")
        }
      }
      stop(msg)
    }
  } else if (waitMode != "forget") {
    outStats = NULL
    if (waitMode == "synch") {
      outStats = out[["details"]]
    } else { #wait
      outStats = out[["details"]][["STATISTICS"]]
    }
    ## The unlists below are due to an idiosyncracy in RJSONIO's fromJSON where 
    ## it may sometimes return a named vector instead of a list. `[[` is not the
    ## logical option in this case.
    if (!is.null(outStats[["warnings"]])) {
      statistics <- list(
        inserted  = unlist(outStats[["statistics"]]["inserted"]),
        appended  = unlist(outStats[["statistics"]]["updated"]),
        ignored   = unlist(outStats[["statistics"]]["ignored"]),
        discarded = unlist(outStats[["statistics"]]["discarded"]),
        warnings  = data.table(
          row = lapply(outStats[["warnings"]], FUN = function(x) { x[["row"]] }),
          message = lapply(outStats[["warnings"]], FUN = function(x) { x[["message"]] })))
    } else {
      statistics <- list(
        inserted  = unlist(outStats[["statistics"]]["inserted"]),
        appended  = unlist(outStats[["statistics"]]["updated"]),
        ignored   = unlist(outStats[["statistics"]]["ignored"]),
        discarded = unlist(outStats[["statistics"]]["discarded"]),
        warnings = NULL)
    }
  }
  
  statistics
  
}

SaveData.generateUuid <- function() {
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
  paste0(
    substr(baseuuid,1,8),
    "-",
    substr(baseuuid,9,12),
    "-",
    "4",
    substr(baseuuid,13,15),
    "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18),
    "-",
    substr(baseuuid,19,30),
    collapse=""
  )
}

SaveData.validate <- function(domain, dataset, data, metadata, normalized) {
  
  # Validate passed domain.
  #
  if(missing(domain)) {
    stop("The domain argument is mandatory.")
  }
  if(class(domain) != "character") {
    stop("The passed domain argument is not a character value.")
  }
  
  # Validate passed dataset
  #
  if(missing(dataset)) {
    stop("The dataset argument is mandatory.")
  }
  if(class(dataset) != "character") {
    stop("The passed dataset argument is not a character value.")
  }
  
  # At least one argument amoung data and metadata need to be
  # specified.
  #
  if(missing(data) & missing(metadata)) {
    stop("Neither data nor metadata arguments have been specified.")
  }
  
  # Validate data argument, if passed.
  #
  if(!missing(data)) {
    if(!is.data.table(data)) {
      stop("The passed data argument is not a data table.")
    }
    # verifies that no factors are present as columns type in the data as passed to the function
    # if so, it stops by reporting all of the wrong factor columns
    badColumns = sapply(data, class) == "factor"
    if(any(badColumns))
      stop("Unexpected factor columns in 'data' input parameter. ",
           "Bad columns:\n", paste(colnames(data)[badColumns], collapse = "\n"))
  }
  
  # Validate metadata argument, if passed.
  #
  if(!missing(metadata)) {
    if(!is.data.table(metadata)) {
      stop("The passed metadata argument is not a data table.")
    }
  }
  
  if(normalized){
    if(!"Value" %in% names(data)){
      stop("Normalized data must have a 'Value' column!")
    }
  } 
  
  if(!normalized){
    if(any("Value" %in% names(data))){
      stop("Denormalized data shouldn't have a 'Value' column!")
    }
  }
}

SaveData.splitIntoChunkTables <- function(tbl, chunkSize) {
  totChunks = ceiling(nrow(tbl) / chunkSize)
  chunks <- list()
  
  for (i in 1 : totChunks) {
    firstRow <- (i * chunkSize) - chunkSize + 1
    lastRow <- (i * chunkSize)
    if (i == totChunks) {
      lastRow <- nrow(tbl)
    }
    chunks[[i]] <- tbl[firstRow:lastRow,]
  }
  chunks
}

SaveData.buildUniqueJSON <- function(data, metadata, config) {
  
  json <- list()
  
  if(!missing(data)) {
      json[["data"]] <- SaveData.buildNormalizedDataDefJSON(data, config)
      json[["data"]][["data"]] <- SaveData.buildNormalizedDataContentJSON(data, config)
  }
  
  if(!missing(metadata) && nrow(metadata) > 0L) {
    json[["metadata"]] <- list()
    json[["metadata"]][[1]] <- SaveData.buildMetadataDefJSON(metadata, config[["dimensions"]])
    jsonMetacont <- SaveData.buildMetadataContentJSON(metadata, config[["dimensions"]])
    for (i in 1:length(jsonMetacont)) {
      json[["metadata"]][[i+1]] <- jsonMetacont[[i]]
    }
  }
  
  json
}

SaveData.buildNormalizedDataDefJSON <- function(data, config) {
  
  # Save the original key of the passed data table.
  #
  #origKey <- key(data)
  
  # Do not consider metadata column, if they have been passed.
  #
  
  # metadataColumnsFilter <- !colnames(data) %in% c("Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value")
  # metadataColumnsFilter <- colnames(data) != "Metadata"
  # metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Language"
  # metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Group"
  # metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Element"
  # metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Value"
  # 
  # Prepare list to hold JSON data.
  #
  json <- list()
  
  # Extract key column names.
  #
  filteredColumnNames <- setdiff(colnames(data), c("Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value"))
  if(length(which(filteredColumnNames == "Value")) <= 0) {
    stop("Unexpected data table structure detected: could not locate Value column.")
  }

  keys <- config[["dimensions"]]
  stopifnot(all(keys %in% filteredColumnNames))
  
  # Set up section declaring data key definition.
  #
  json[["keyDefinitions"]] <- list()
  for(i in seq_along(keys)) {
    json[["keyDefinitions"]][[i]] <- list()
    json[["keyDefinitions"]][[i]][["code"]] <- keys[i]
  }
  
  # Check if flag columns are present. They are all those immediately following
  # the Value column.
  # as.character converts NULL to empty char
  flags <- as.character(config[["flags"]])

  # Set up section declaring flags definition.
  #
  json[["flagDefinitions"]] <- list()
  for(i in seq_along(flags)) {
    json[["flagDefinitions"]][[i]] <- list()
    json[["flagDefinitions"]][[i]][["code"]] <- flags[i]
  }
  
  
  # Set data table key.
  #
  #setkeyv(data, keys, verbose = FALSE)
  
  
  # Restore original key.
  #
  #setkeyv(data, origKey, verbose = FALSE)
  
  return(json)
}

SaveData.buildNormalizedDataContentJSON <- function(data, config) {
  
  # Save the original key of the passed data table.
  #
  origKey <- key(data)
  
  # Restore original key.
  #
  on.exit(setkeyv(data, origKey, verbose = FALSE), add = TRUE)
  
  
  # Do not consider metadata column, if they have been passed.
  #
  filteredColumnNames <- setdiff(colnames(data), 
                                 c("Metadata", "Metadata_Language", "Metadata_Group", "Metadata_Element", "Metadata_Value"))
  if(!("Value" %in% filteredColumnNames)) {
    stop("Unexpected data table structure detected: could not locate Value column.")
  }
  
  keys <- config[["dimensions"]]
  
  # Check if flag columns are present. They are all those immediately following
  # the Value column.
  # as.character converts NULL to empty char
  flags <- as.character(config[["flags"]])
  
  # Set data table key.
  #
  setkeyv(data, keys, verbose = FALSE)
  
  # Extract the set of unique keys for external loop.
  #
  uniqueKeys <- unique(copy(data))
  uniqueKeys[, Value := as.character(Value)]
  json <- split(as.matrix(uniqueKeys), seq_len(nrow(uniqueKeys)))
  json <- json[order(as.numeric(names(json)))]
  names(json) <- NULL
  
  return(json)
}

##' Normalize Data
##' 
##' This function takes denormalized data and casts it as normalized data.
##' 
##' @param data The denormalized dataset, provided as a data.table
##' @param keys A character vector of column names of data which correspond to 
##'   all the dimensions of the data except for the denormalized dimension.
##' @param denormalizedKey A character value containing the name of the 
##'   denormalized key.  This string is then used to determine the columns of 
##'   data which correspond to values and flags and which correspond to 
##'   dimensions
##' @param keepNA logical. If TRUE, missing values are preserved in the 
##'   normalised format. If FALSE, all missing values are assumed to be only 
##'   implicit missing and excluded
##' @param returnKeyed logical. If TRUE, the resulting data.table will be
##'   indexed by the keys provided
##'   
##' @return A data.table object in normalized format.
##'   

normalizeData <- function(data, keys, denormalizedKey, keepNA = TRUE, returnKeyed = FALSE){
  
  nd <- copy(data)
  blankit <- FALSE
  
  if(nrow(data) == 0){
    # If the data is empty, then there's no reshape needed
    nd <- nd[1,]
    blankit <- TRUE
  }
  
  # Code to fix a bug in data.table 1.9.6 - the bug has been fixed from 1.10.4 at the latest
  # Columns containing only NA would lead to malformed factors
  if(!keepNA){
    emptycols <- Filter(function(x) all(is.na(nd[,get(x)])), names(nd))
    if(length(emptycols)){
      nd[, (emptycols) := NULL]
    }
  }
  
  newData <- suppressWarnings(
    # This used to be a tidyr function but it kept stripping the data.table attribute
    data.table::melt(nd, id.vars=keys, variable.name="Key", value.name="Value", na.rm = !keepNA)
  )
  
  newData[, c("Key", denormalizedKey) := tstrsplit(Key, split = sprintf("_%s_", denormalizedKey))]
  
  newData <- dcast(newData, 
                   formula = paste0(paste(c(keys, denormalizedKey), collapse="+"), "~Key"), 
                   value.var="Value")
  
  newData[, Value := as.numeric(Value)]
  
  if(blankit){
    return(newData[0,])
  }
  
  if(!returnKeyed){
    setkey(newData, NULL)
  }
  
  return(newData[])
}

SaveData.buildMetadataDefJSON <- function(metadata, keys) {
  
  # Prepare key definitions element.
  #
  jsonElement <- list()
  jsonElement[["keyDefinitions"]] <- list()
  for(i in 1:length(keys)) {
    jsonElement[["keyDefinitions"]][[i]] <- list()
    jsonElement[["keyDefinitions"]][[i]][["code"]] <- keys[[i]]
  }
  
  jsonElement
}

SaveData.buildMetadataContentJSON <- function(metadata, dimensions) {
  
  keys <- dimensions
  
  metadata <- metadata[, c(keys, "Metadata", "Metadata_Language",
                           "Metadata_Element", "Metadata_Value"), with = FALSE]

  json <- apply(metadata, 1, function(x) list(x))
  json <- lapply(json, function(x){
    out = x[[1]]
    names(out) = NULL
    out
  })
  
  json
}

SaveData.getDenormalizedKey <- function(keys, data){
  
  valCols <- grep("^Value_", names(data), value = TRUE)
  dKeyPos <- vapply(keys, grepl, logical(length(valCols)),
                    fixed = TRUE, x = valCols)
  dKeyPos <- apply(dKeyPos, 2, all)
  if(sum(dKeyPos) != 1L){
    stop("Unable to determine denormalisation key for data. Check if data is a valid format")
  }
  dKey <- keys[dKeyPos]

}

SaveData.validateFlagValues <- function(data, flagCols){
  # Disallow writing flags without values
  flagNoValues <- data[is.na(Value), rowSums(is.na(.SD)) > 0, .SDcols = flagCols]
  if(length(flagNoValues) > 0){
    stop(paste0("There are ", length(flagNoValues), " records with missing values and non-missing flags. 
                Flags may not be present without values"))
  }
  
  return(invisible(NULL))
}
