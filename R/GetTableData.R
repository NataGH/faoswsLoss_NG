# FUNCTION: GetTableData
#
# invokes a SWS RESTful POST to perform a query on a specific schema/table, returning the list of records matched, or NULL if none.
# If the SQL request and resulting query is incorrect, SQL error details are pasted and execution blocked ;
# if HTTPS connection fails (e.g. a runtime error on the server), HTTP status != 200 is pasted and execution blocked.
#
# ARGS:
#
# - schemaName, MANDATORY: a string containing the name of the schema to be accessed for the query;
# - tableName, MANDATORY: a string containing the name of the table to be read by the query;
# - whereClause, OPTIONAL: a string containing whichever SQL clauses to take place after the "FROM" section
#   (usually WHERE conditions). Example values:
#                - "WHERE (id IN (1,2,3,4) AND descr = 'A') OR (id = 5 AND status = 0) "
#                - "ORDER BY descr DESC"
#                - "WHERE descr = 'aaa' ORDER BY id LIMIT 5"
#                if NULL, nothing is appended in the SQL query composition after the FROM section;
# - selectColumns, OPTIONAL: the list of columns to be returned by the query. it can be:
#                - NULL (the query will start with "SELECT * ")
#                - a list of strings, such as: "list('id', 'descr')"

GetTableData <- function(schemaName, tableName, whereClause = NULL, selectColumns = NULL) {

	# concats the base URL with the sub-path and the execution ID from session
	url <- paste0(swsContext.baseRestUrl, "/r/tabledata/", swsContext.executionId) 

	# prepares JSON request object conforming to args received as input
	json <- list(
		reqToken = swsContext.token,
		schemaName = schemaName,
		tableName = tableName,
		whereClause = whereClause,
		selectColumns = selectColumns)
	
	# performs the invokation through the corresponding SWS RESTful POST service
	# which returns a composite JSON object with a "rows" field with the list of records from the DB
	# and an "rc" property which is 0 if execution succeeded
	# if "rc" is != 0, an "error" property is contained in the result, instead of "rows", containing details of the error
	# NOTE: the only kind of errors described by the "error" property
	# are the ones referred to incorrect SQL used basing on the request arguments
	# (any other server-side or network errors will result in 'http status != 200' and pasted by the PostRestCall function)
	jsonOut <- PostRestCall(url, json)
	
	# if rc != 0 a SQL error occurred: halts by giving details
	if (!(identical(jsonOut[["rc"]], 0))) {
	
		# throws the error with details
		stop(paste("An error occurred executing the SQL query statement: ", jsonOut[["error"]]))
	
	} else {
	
		# if rows is NULL, means no records have been extracted from the DB by the requested SQL query
		if ((is.null(jsonOut[["rows"]]))) {
	
			NULL
	
		} else {

			# transforms the list of json-like objects in a data.table
			do.call("rbind", 
				lapply(
					jsonOut[["rows"]], 
					function(x) {
						x[sapply(x, is.null)] <- NA
						unlist(x)
					}))
      
		}
	}	

}
