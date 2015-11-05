##' SetTableData
##' 
##' This function allows altering the content of a table for a specified
##' schema.
##' 
##' @param schemaName A character value containing the name of the schema to be
##' accessed for the query.
##' @param tableName A character value containing the name of the table to be
##' read by the query.
##' @param data A non empty data.table containing records with values to be
##' appended/edited on the table. No need to put all the columns that are in
##' the DB, BUT at least the column/s that compose the primary key (if any). If
##' no primary key is set for the table, all the records will be appended as
##' they are
##' @param replace Logical, if TRUE, then for a row that exists in the
##' data.table and a corresponding record is fuond in the DB, with same key,
##' the record is UPDATED.  If FALSE, it is skipped.
##' @param purge Logical.  If true, before any inserts/updates, rows are
##' deleted from the database table; which rows depends on the purgeFilter
##' parameter (see below).
##' @param purgeFilter Can be specified only for purge=TRUE.  This allows the
##' user to specify a filter for the deletion in the form of an SQL WHERE
##' clause.  Specifying purge=TRUE and purgeFilter=NULL will remove all rows
##' from the table before the update.
##' 
##' @details The system checks if the data passed as data.table are either
##' conforming to the expected column type to which any cell refers, or can be
##' parsed (e.g. a string containing "1" to the number 1, or a string
##' containing "01-01-2001" to the corresponding date by assuming the format
##' "dd-MM-yyyy"). If the conversion fails, or any other SQL error is arisen by
##' the underlying DB (e.g. a column constraint for NOT NULL), the function
##' aborts (using stop()) with the error message being placed on the R error
##' message queue.  If otherwise the function succeed, statistics on the
##' updates are returned, indicating how many records have been:
##' \itemize{
##'     \item deleted (if purge was TRUE)
##'     \item updated (if replace was TRUE and rec found by key)
##'     \item inserted (if rec not found by key)
##'     \item skipped (if replace was FALSE and rec found by key)
##' }
##' 
##' @export SetTableData

SetTableData <- function(schemaName, tableName, data, replace = FALSE, purge = FALSE, purgeFilter = NULL) {

	# concats the base URL with the sub-path and the execution ID from session
	url <- paste0(swsContext.baseRestUrl, "/r/settabledata/", swsContext.executionId) 

  # transforms the data.table received as input in a list set to properly fit via JSON marshalling to the
  # corresponding List<Map<String, Object>> Java object, where:
  # - any record is an element in the List
  # - any entry in the map, for any given record, is a "cell" identified by:
  #     - a key (the name of the column)
  #     - a value (the object corresponding to the actual value***)
  # ***Dates and POSIXct Timestamps are formatted as string
	names <- names(data)

	strRow <- paste0("c(", paste0("field", seq_len(ncol(data)), collapse = ", "))

	jsonData  <- list()
	for (i in 1:nrow(data)) {
	  for (y in 1:ncol(data)) {
	    if(class(data[[i, y]])[[1]] == "POSIXct") {
	      eval(parse(text=paste("field", y, " <- list(", names[[y]], "=as.character(data[[i, y]], format=\"%Y-%m-%d %H:%M:%S\"))", sep="")))
	    } else {
	      if(class(data[[i, y]])[[1]] == "Date") {
	        eval(parse(text=paste("field", y, " <- list(", names[[y]], "=as.character(data[[i, y]], format=\"%Y-%m-%d\"))", sep="")))
	      } else {
	        eval(parse(text=paste("field", y, " <- list(", names[[y]], "=data[[i, y]])", sep="")))
	      }
	    }
	  }
	  eval(parse(text=paste("jsonData[[", i, "]] <- ", strRow, sep="")))
	}

  # prepares JSON request object conforming to args received as input
	json <- list(
	  reqToken = swsContext.token,
	  schemaName = schemaName,
	  tableName = tableName,
	  data = jsonData,
	  replace = replace,
	  purge = purge,
	  purgeFilter = purgeFilter)
	
	# performs the invocation through the corresposnding SWS RESTful POST service
	# which returns a composite JSON object with statistics on the execution of the command
	# and an "rc" property which is 0 if execution succeeded
	# if "rc" is != 0, an "error" property is contained in the result, containing details of the error
  # errors may be of many types, mostly referred to SQL exceptions (e.g. a wrong purgeFilter has been specified), or
  # problems referred to marshalling/unmarshalling the values of the data as follows: R data.table->JSON->Java Objects
	jsonOut <- PostRestCall(url, json)
	
	# if rc != 0 a error occurred: halts by giving details
	if (!(identical(jsonOut[["rc"]], 0))) {
	
		# throws the error with details
	  stop(paste("An error occurred executing the command: ", jsonOut[["error"]]))
	  
	} else {
	
	  if ((jsonOut[["deleted"]] == 0) & (jsonOut[["inserted"]] == 0) & (jsonOut[["updated"]] == 0)) {
	    if (jsonOut[["skipped"]] == 0) {
	      
	      message("The command did not produce any change on the DB")
	    } else {
	      
	      message("The command did not produce any change on the DB (", jsonOut[["skipped"]], " record/s skipped)")
	    }
	  } else {
	    
	    message(paste("Command successfully processed: ",
	                  "deleted=", jsonOut[["deleted"]], ", inserted=", jsonOut[["inserted"]],
	                  ", updated=", jsonOut[["updated"]], ", skipped=", jsonOut[["skipped"]]))
	    
	  }
	}
}
