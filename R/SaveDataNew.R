##' Save Data
##' 
##' This is the main function used to write data back to a user's session.  The
##' data must be in a particular format, and thus it's recommended to first
##' read data via GetData, modify the data.table, and then write it back to the
##' database with this function.
##' 
##' Also, this function will throw an error if you attempt to write to invalid
##' data.  For example, some countries have restrictions on which time period
##' they have data (as they recently became a country, or are no longer a
##' country).  See ?GetCodeList.
##' 
##' @param domain A character value specifying the domain for which the code
##' list is required.
##' @param dataset A character value specifying the dataset for which the code
##' list is required.
##' @param data A data.table object containing keys, data and flags.
##' @param metadata A data.table object containing the metadata.
##' @param normalized Logical, indicates whether data is in a normalized or
##' denormalized format.
##' 
##' @return No object is returned, as this function just writes data to the
##' SWS.
##' 

SaveDataNew <- function(domain, dataset, data, metadata, normalized = TRUE, waitMode = "wait", waitTimeout = 600) {

	# Validate passed arguments.
	#
	SaveDataNew.validate(domain, dataset, data, metadata, normalized)

	# Prepare JSON for REST call.
	#
	json <- SaveDataNew.buildJSON(domain, dataset, data, metadata, normalized)

	# Perform REST call.
	#
	url <- paste0(swsContext.baseRestUrl, "/r/data/new/")

	out <- PutRestCall(
	  paste0(url, swsContext.executionId, "/", domain, "/", dataset, "?token=", 
      swsContext.token,  "&normalized=true&waitMode=", waitMode),
    json)

	## STATUS:NOT-FOUND, STATUS:STARTED, STATUS:COMPLETED
	## STATUS:VALID-ERRS
	## 		VALID-ERR-1, VALID-ERR-2, ..., VALID-ERR-N
	## 	STATUS:FAILED
	## 		STACKTRACE
	## 	ALL STATUSES (EXCEPT NOT-FOUND):
	## 		STARTED-AT, EXEC-ID, USER-ID

	# wait mode -> has to check for execution result any 3 seconds
	if (waitMode == "wait") {
		# counts time elapsed from invokation , to stop when the timeout is exeeded
		elapsed <- 0
		# subtracted in abovementioned check
		p1 <- proc.time()[['elapsed']]
		# it is used as condition for the subsequent loop
		statusOut <- NULL

    # checks until one of the two is given: 1. the timeout is not exeeded; 2. a final result is obtained
		while ((elapsed < waitTimeout) & (is.null(statusOut))) {

      # out[['message']] contains the UUID for the exec. status check, it invokes the get by concatenating it to the 
			# previously used PUT url
			statusOut = GetRestCall(paste0(url, out[['message']]))

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
	}
}

SaveDataNew.validate <- function(domain, dataset, data, metadata, normalized) {

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
	}

	# Validate metadata argument, if passed.
	#
	if(!missing(metadata)) {
		if(!is.data.table(metadata)) {
			stop("The passed metadata argument is not a data table.")
		}
	}
}


SaveDataNew.buildJSON <- function(domain, dataset, data, metadata, normalized) {

	json <- list()

	if(!missing(data)) {
		if(normalized) {
			json[["data"]] <- SaveDataNew.buildNormalizedDataJSON(data)
		} else {
			#json[["denormalizedData"]] <- SaveDataNew.buildDenormalizedDataJSON(data)
			json[["data"]] <- SaveDataNew.buildDenormalizedDataJSON(data)
		}
	}

	if(!missing(metadata)) {
		json[["metadata"]] <- SaveDataNew.buildMetadataJSON(metadata)
	}

	json
}


SaveDataNew.buildNormalizedDataJSON <- function(data) {

	# Save the original key of the passed data table.
	#
	origKey <- key(data)

	# Do not consider metadata column, if they have been passed.
	#
	metadataColumnsFilter <- colnames(data) != "Metadata"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Language"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Group"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Element"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Value"


	# Prepare list to hold JSON data.
	#
	json <- list()

	# Extract key column names.
	#
	filteredColumnNames <- colnames(data[, metadataColumnsFilter, with = FALSE])
	if(length(which(filteredColumnNames == "Value")) <= 0) {
		stop("Unexpected data table structure detected: could not locate Value column.")
	}
	index <- tail(which(filteredColumnNames == "Value"), 1)
	if(index <= 0) {
		stop("Unexpected data table structure detected: Value column located before any key column")
	}
	keys <- filteredColumnNames[1:index - 1]

	# Set up section declaring data key definition.
	#
	json[["keyDefinitions"]] <- list()
	for(i in 1:length(keys)) {
		json[["keyDefinitions"]][[i]] <- list()
		json[["keyDefinitions"]][[i]][["code"]] <- keys[i]
	}

	# Check if flag columns are present. They are all those immediately following
	# the Value column.
	#
	flags <- c()
	if(length(filteredColumnNames) > index) {
		flags <- filteredColumnNames[(index + 1):(length(filteredColumnNames))]
	}

	# Set up section declaring flags definition.
	#
	json[["flagDefinitions"]] <- list()
	if (length(flags) > 0) {
		for(i in 1:length(flags)) {
			json[["flagDefinitions"]][[i]] <- list()
			json[["flagDefinitions"]][[i]][["code"]] <- flags[i]
		}
	}


	# Set data table key.
	#
	setkeyv(data, keys, verbose = FALSE)

	# Extract the set of unique keys for external loop.
	#
	json[["data"]] <- list()
	uniqueKeys <- unique(data)
	for(i in 1:nrow(uniqueKeys)) {
		
		jsonElement <- list()
		jsonElement[["keys"]] <- list()
		jsonElement[["keys"]] <- c(jsonElement[["keys"]], as.character(uniqueKeys[i, keys, with = FALSE]))
		jsonElement[["value"]] <- uniqueKeys[i, Value]
		jsonElement[["flags"]] <- list()
		jsonElement[["flags"]] <- c(jsonElement[["flags"]], as.character(uniqueKeys[i, flags, with = FALSE]))
		json[["data"]][[i]] <- jsonElement
	}

	# Restore original key.
	#
	setkeyv(data, origKey, verbose = FALSE)

	json
}


SaveDataNew.buildDenormalizedDataJSON <- function(data) {

	# Save the original key of the passed data table.
	#
	origKey <- key(data)

	# Do not consider metadata column, if they have been passed.
	#
	metadataColumnsFilter <- colnames(data) != "Metadata"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Language"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Group"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Element"
	metadataColumnsFilter <- metadataColumnsFilter & colnames(data) != "Metadata_Value"


	# Prepare list to hold JSON data.
	#
	json <- list()

	# Extract fixed key column names.
	#
	filteredColumnNames <- colnames(data[, metadataColumnsFilter, with = FALSE])
	if(length(which(grepl("^Value_", filteredColumnNames))) <= 0) {
		stop("Unexpected data table structure detected: could not locate the first Value column.")
	}
	index <- head(which(grepl("^Value_", filteredColumnNames)), 1)
	if(index <= 0) {
		stop("Unexpected data table structure detected: Value column located before any key column")
	}
	keys <- filteredColumnNames[1:index - 1]

	# Extract denormalized key column name.
	#
	denormalizedKey <- substr(filteredColumnNames[index], nchar("Value_") + 1, regexpr("_[^_]+$", filteredColumnNames[index]) - 1)
	allKeys <- append(keys, denormalizedKey)

	# Set up section declaring data key definition.
	#
	json[["keyDefinitions"]] <- list()
	for(i in 1:length(allKeys)) {
		json[["keyDefinitions"]][[i]] <- list()
		json[["keyDefinitions"]][[i]][["code"]] <- allKeys[i]
	}

	# Check if flag columns are present. They are all those immediately following
	# the Value column.
	#
	flags <- c()
	for(col in filteredColumnNames[(index + 1):(length(filteredColumnNames))]) {
		if(grepl("^Value_", col)) {
			break
		}
		flags <- append(flags, substr(col, 1, regexpr(paste0("_", denormalizedKey), col) - 1))
	}

	# Set up section declaring flags definition.
	#
	json[["flagDefinitions"]] <- list()
	if (length(flags) > 0) {
		for(i in 1:length(flags)) {
			json[["flagDefinitions"]][[i]] <- list()
			json[["flagDefinitions"]][[i]][["code"]] <- flags[i]
		}
	}

	# Extract all denormalized column keys.
	#
	denormalizedKeys <- c()
	for(col in filteredColumnNames[which(grepl("^Value_", filteredColumnNames))]) {
		denormalizedKeys <- append(denormalizedKeys, substr(col, regexpr("_[^_]+$", col) + 1, nchar(col)))
	}


	# Set data table key.
	#
	setkeyv(data, keys, verbose = FALSE)

	# Extract the set of unique keys for external loop.
	#
	json[["data"]] <- list()
	uniqueKeys <- unique(data)
	k = 1
	for(i in 1:nrow(uniqueKeys)) {
		for(j in 1:length(denormalizedKeys)) {
			
			value <- unlist(uniqueKeys[i, paste0("Value_", denormalizedKey, "_", denormalizedKeys[[j]]), with = FALSE])
			if (!is.na(value))
			{
				jsonElement <- list()
				jsonElement[["keys"]] <- c(as.character(uniqueKeys[i, keys, with = FALSE]), denormalizedKeys[[j]])

				if(is.null(value)) {
					jsonElement[["value"]] <- NA
				} else {
					jsonElement[["value"]] <- as.numeric(value)
				}

				flagValues <- unlist(uniqueKeys[i, paste0(flags, "_", denormalizedKey, "_", denormalizedKeys[[j]]), with = FALSE])

				jsonElement[["flags"]] <- list()
				for(f in uniqueKeys[i, paste0(flags, "_", denormalizedKey, "_", denormalizedKeys[[j]]), with = FALSE]) {

					u <- unlist(f)
					if(is.null(u)) {
						jsonElement[["flags"]] <- c(jsonElement[["flags"]], "")
					}
					else {
						jsonElement[["flags"]] <- c(jsonElement[["flags"]], as.character(u))
					}
				}

#				json[["data"]][[(i - 1) * length(denormalizedKeys) + j]] <- jsonElement
				json[["data"]][[k]] <- jsonElement
				k <- k + 1
			}
		}
	}

	# Restore original key.
	#
	setkeyv(data, origKey, verbose = FALSE)

	json
}


SaveDataNew.buildMetadataJSON <- function(metadata) {

	# Save the original key of the passed data table.
	#
	origKey <- key(metadata)

	# Extract key column names.
	#
	if(length(which(colnames(metadata) == "Metadata")) <= 0) {
		stop("Unexpected data table structure detected: could not locate Metadata column.")
	}
	index <- tail(which(colnames(metadata) == "Metadata"), 1)
	if(index <= 0) {
		stop("Unexpected data table structure detected: Metadata column located before any key column")
	}
	keys <- colnames(metadata)[1:index - 1]

	# Set data table key.
	#
	setkeyv(metadata, keys, verbose = FALSE)


	# Prepare list to hold JSON data.
	#
	json <- list()


	# Prepare key definitions element.
	#
	jsonElement <- list()
	jsonElement[["keyDefinitions"]] <- list()
	for(i in 1:length(keys)) {
		jsonElement[["keyDefinitions"]][[i]] <- list()
		jsonElement[["keyDefinitions"]][[i]][["code"]] <- keys[[i]]
	}
	json[[1]] <- jsonElement


	# Extract the set of unique keys for external loop.
	#
	uniqueKeys <- unique(metadata)[,keys, with = FALSE]
	for(i in 1:nrow(uniqueKeys)) {

		jsonElement <- list()
		jsonElement[["keys"]] <- as.character(uniqueKeys[i])
		jsonElement[["metadata"]] <- list()

		slicedByKey <- metadata[uniqueKeys[i]]
		setkeyv(slicedByKey, c("Metadata_Group", "Metadata_Language", "Metadata"))
		uniqueMetadata <- unique(slicedByKey)[, c("Metadata", "Metadata_Language", "Metadata_Group"), with = FALSE]

		for(j in 1:nrow(uniqueMetadata)) {

			jsonElement[["metadata"]][[j]] <- list()
			jsonElement[["metadata"]][[j]][["typeCode"]] <- uniqueMetadata[j, Metadata]
			jsonElement[["metadata"]][[j]][["language"]] <- uniqueMetadata[j, Metadata_Language]
			jsonElement[["metadata"]][[j]][["elements"]] <- list()

			metadataElements <- slicedByKey[uniqueMetadata[j]]
			for(k in 1:nrow(metadataElements)) {
				jsonElement[["metadata"]][[j]][["elements"]][[k]] <- list()
				jsonElement[["metadata"]][[j]][["elements"]][[k]][["typeCode"]] <- metadataElements[k, Metadata_Element]
				jsonElement[["metadata"]][[j]][["elements"]][[k]][["value"]] <- metadataElements[k, Metadata_Value]
			}
		}

		json[[i + 1]] <- jsonElement
	}

	# Restore original key.
	#
	setkeyv(metadata, origKey, verbose = FALSE)

	json
}
