#' Read Datatable
#' 
#' This function allows retrieval of a Datatable from the SWS. To get a full 
#' list of config data, use \code{\link{ReadDatatableList}}.
#' 
#' @param table character. Name of desired Datatable. By default, the one from 
#'   the token.
#' @param columns either a character vector of column names or list of 
#'   \code{\link{DtDimension}} objects, each representing a desired column, how 
#'   to sort it and by what value to filter it. If a character vector is
#'   supplied, \code{includeAll} is automatically set to FALSE.
#' @param includeAll logical. If TRUE, include all columns in the result, 
#'   including those not specified in \code{columns}. Otherwise, return only 
#'   specified columns.
#' @param where character. WHERE clause of the for "WHERE x in y" or any other 
#'   valid form.
#' @param readOnly logical. Determines whether the table should include columns 
#'   only relevant for writing back to a table (id and timestamp) or just the 
#'   data.
#' @param limit numeric. Used mainly for testing, limit results to a certain 
#'   number of rows.
#'   
#' @return data.table as specified by params.
#'   
#' @author Sebastian Campbell and Carlo Cancellieri
#'   
#' @examples 
#' \dontrun{
#' table <- "ct_tariffline_adhoc"
#' tariffdefault <- ReadDatatable(table, readOnly = FALSE, limit=1e3)
#' 
#' ntariff10K <- ReadDatatable(table, limit=1e4)
#' 
#' tariffwrite <- ReadDatatable(table, limit=1e4, readOnly = FALSE) 
#' 
#' tariffwhere <- ReadDatatable(table, where="qunit = '1'", limit=1e4)
#' 
#' tariffselect <- ReadDatatable(table, columns=c("rep", "tyear", "hsrep"), limit=1e3)
#' 
#' cols <- list(DtDimension(id="rep", ascending=TRUE),
#'              DtDimension(id="tyear"),
#'              DtDimension(id="hsrep", value="H4"))
#'
#' tariffcselect <- ReadDatatable(table, columns=cols, includeAll = FALSE, limit = 1e3)
#' }
#'   
#' @import curl
#' @import bit64
#' @export ReadDatatable




ReadDatatable <- function(table=FetchDatatable(), columns = list(), includeAll = TRUE, where=NULL, readOnly = TRUE, limit=NULL) {

  baseurl <- paste(swsContext.baseRestUrl, "api", "datatable", table, "export", sep = "/")
  queryparams <- paste(paste0("_xid=", swsContext.token), collapse= "&")
  url <- paste(baseurl, queryparams, sep="?")  
  
  
  if(is.character(columns)){
    columns <- Map(DtDimension, id = columns)
    includeAll <- FALSE
  }
  
  colspec <- ifelse(includeAll, "partial", "total")
  
  columns <- lapply(columns, as.list)
  names(columns) <- NULL

  json <- list(
    filter = list(
      colspec = colspec,
      columns = columns,
      where = where,
      limit = limit,
      page = NULL
    ),
    directives = list(format = "rowset",
                      readonly = readOnly)
  )

  runStreaming(url, json)
   
}

typeChange <- Vectorize(function(x){
  switch(x,
         autoid = "integer",
         smalltext = "character",
         text = "character",
         bigtext = "character",
         integer = "integer",
         bigint = "integer64",
         decimal = "numeric",
         bool = "logical",
         timestamp = "integer64"
         )
})

asSwitch <- function(object, class){
  if(class == "integer64"){
    return(as.integer64(object))
  } else {
    return(as(object, class))
  }
}

streamIn <- function (con, colstats, pagesize = 500, verbose = FALSE, ...) {
  if (!is(con, "connection")) {
    stop("Argument 'con' must be a connection.")
  }

  count <- 0
  cb <- {
    out <- new.env()
    function(x) {
      if (length(x)) {
        #browser()
        #Convert NULL to NA
        x <- lapply(x, function(x) lapply(x, function(x) {if(is.null(x)){ x <- NA}; x}))
        #Make data.table
        x <- rbindlist(x)
        #Set names appropriately
        setnames(x, colstats$id)
        #set types
        x[, (colstats$id) := mapply(asSwitch, .SD, colstats$type, SIMPLIFY=FALSE)]
        count <<- count + length(x)
        out[[as.character(count)]] <<- x
      }
    }
  }
  
  repeat {
    page <- readLines(con, n = pagesize, encoding = "UTF-8")
    if (length(page)) {
      cb(lapply(page, jsonlite:::parseJSON))
      if (verbose) 
        cat("\r Found", count, "records...")
    }
    if (length(page) < pagesize) 
      break
  }
  
  if (verbose) cat("\r Imported", count, "records. Simplifying into dataframe...\n")
  out <- as.list(out, sorted = FALSE)
  rbindlist(out[order(as.numeric(names(out)))])
  
}

runStreaming <- function(url, json){
  
  h <- new_handle()
  handle_setheaders(h, "Accept: application/jsonl",
                    "Content-Type: application/json",
                    "Accept-Encoding: gzip, deflate")
  
  handle_setopt(h, customrequest="POST",
                verbose = FALSE,
                noproxy = .swsenv$swsContext.noProxy,
                ssl_verifypeer = FALSE, 
                accept_encoding = "gzip, deflate",
                sslcert = path.expand(.swsenv$swsContext.clientCertificate),
                sslkey = path.expand(.swsenv$swsContext.clientKey),
                ssl_verifyhost = 2,
                post = 1,
                postfields = RJSONIO::toJSON(json, digits = 30))
  
  on.exit(
    if(exists("conn")) {
      if(inherits(conn, "connection")){
        if(isOpen(conn)) close(conn)
        }
      }
    )
  conn <- try(.Call(curl:::R_curl_connection, url, "r", h, FALSE))
  
  ## If there's an error before the connection can be properly created, close it
  if(inherits(conn, "try-error")){
    cons <- showConnections(all = TRUE)
    closableconn <- getConnection(as.numeric(row.names(cons)[cons[,"description"]  ==  url]))
    close(closableconn)
  }
  
  responseCode <- curl:::handle_response_data(h)$status_code
  
  if(responseCode != 200){
    
    errmessage <- paste0(readLines(conn, warn = FALSE), collapse="\n")
    HandleHTTPError(responseCode, errmessage)
    
    }
  
  #Assign in handler environment
  colstats <- jsonlite::fromJSON(readLines(conn, 1))
  colstats$type <- typeChange(colstats$type)
  
  obj <- streamIn(conn, colstats = colstats)
  obj
}