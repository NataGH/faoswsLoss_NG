##' Get Table Data
##' 
##' Invokes a SWS RESTful POST to perform a query on a specific schema/table,
##' returning the list of records matched, or NULL if none.  If the SQL request
##' and resulting query is incorrect, SQL error details are pasted and
##' execution blocked; if HTTPS connection fails (e.g. a runtime error on the
##' server), HTTP status != 200 is pasted and execution blocked.
##' 
##' @param schemaName A string containing the name of the schema to be accessed
##' for the query.
##' @param tableName A string containing the name of the table to be read by
##' the query.
##' @param whereClause (optional) A string containing whichever SQL clauses to
##' take place after the "FROM" section (usually WHERE conditions). Example
##' values:
##'     - "WHERE (id IN (1,2,3,4) AND descr = 'A') OR (id = 5 AND status = 0) "
##'     - "ORDER BY descr DESC"
##'     - "WHERE descr = 'aaa' ORDER BY id LIMIT 5"
##' If NULL, nothing is appended in the SQL query composition after the FROM
##' section.
##' @param selectColumns (optional) The list of columns to be returned by the
##' query. it can be:
##'     - NULL (the query will start with "SELECT * ")
##'     - a list of strings, such as: "list('id', 'descr')"
##' 
##' @return A data.table object containing the queried dataset.
##' 
##' @export GetTableData

GetTableData <- function(schemaName, tableName, whereClause = NULL, selectColumns = NULL) {

	# concats the base URL with the sub-path and the execution ID from session
	url <- paste0(swsContext.baseRestUrl, "/r/gettabledata/", swsContext.executionId) 

	# prepares JSON request object conforming to args received as input
	json <- list(
		reqToken = swsContext.token,
		schemaName = schemaName,
		tableName = tableName,
		whereClause = whereClause,
		selectColumns = selectColumns)
	
	# performs the invocation through the corresposnding SWS RESTful POST service
	# which returns a composite JSON object with a "rows" field with the list of records from the DB
	# and an "rc" property which is 0 if execution succeeded
	# if "rc" is != 0, an "error" property is contained in the result, instead of "rows", containing details of the error
	# NOTE: the only kind of errors described by the "error" property
	# are the ones referred to incorrect SQL used basing on the request arguments
	# (any other server-side or network errors will result in 'http status != 200' and pasted by the PostRestCall function)
  # if rc != 0, another object is returned "columnsMetadata", which contains a list of "column" objects, any of which with
  # the following properties: name, type (SQL java name for the DB column) and position in the list. These attributes are used
  # to build the resulting data.table with columns conforming with the DB SQL types.
  # Please note only the most common SQL types are mapped/transformed, excluding binary and complex ones: BLOB, CLOB, ARRAY, ...
	jsonOut <- PostRestCall(url, json)
	
	# if rc != 0 a SQL error occurred: halts by giving details
	if (!(identical(jsonOut[["rc"]], 0))) {
	
		# throws the error with details
		stop(paste("An error occurred executing the SQL query statement: ", jsonOut[["error"]]))
	
	} else {
	
		# if rows is NULL, means no records have been extracted from the DB by the requested SQL query
		if ((is.null(jsonOut[["rows"]])) || length(jsonOut[["rows"]]) == 0) {
	
			NULL
	
		} else {

			# transforms the list of json-like objects in a data.table
      constructor <- "data.frame(stringsAsFactors=FALSE"
      #sanitise column names
      jsonOut$columnsMetadata <- lapply(jsonOut$columnsMetadata,
                                        function(x){
                                          nom <- names(x)
                                          setNames(make.names(x), nom)
                                          }
                                        )
      
      for (i in 1:length(jsonOut$columnsMetadata)) {
        colname <- jsonOut$columnsMetadata[[i]][["name"]]
        coltype <- jsonOut$columnsMetadata[[i]][["type"]]
        coltype <- switch(coltype, 
          varchar={ "=character()" },
          int8={ "=numeric()" },
          bool={ "=logical()" },
          date={ "=as.Date(character(), format='%Y-%m-%d')" },
          float4={ "=numeric()" },
          float8={ "=numeric()" },
          timestamp={ "=as.POSIXct(character(), format='%Y-%m-%d %H:%M:%S')" },
          int4={ "=integer()" },
          numeric={ "=numeric()" },
          { "=character()" }
        )
        constructor <- paste(constructor, ",", colname, coltype)
      }
      constructor <- paste(constructor, ')')
      eval(parse(text=paste("dataframe <- ", constructor)))

      for (i in 1:length(jsonOut$columnsMetadata)) {
        colname <- jsonOut$columnsMetadata[[i]][["name"]]
        coltype <- jsonOut$columnsMetadata[[i]][["type"]]
        for (y in 1:length(jsonOut$rows)) {
          eval(parse(text=paste("value <- jsonOut$rows[[y]][['", colname, "']]", sep="")))
          expr <- "resValue <- value"
          if (is.null(value)) { 
            expr = "resValue <- NA"
          } else {
            expr <- switch(coltype, 
              date={ "resValue <- as.Date(value, format='%Y-%m-%d')" },
              timestamp={ "resValue <- as.POSIXct(value, format='%Y-%m-%d %H:%M:%S')" },
              int4={ "resValue <- as.integer(value)" },
              { expr}
            )
          }
          eval(parse(text=expr))
          dataframe[y,i] <- resValue
        }
      }
      data.table(dataframe)
    }
  }	
}
