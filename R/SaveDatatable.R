#' Save data to Datatable
#'
#' Functions to save to Datatables on SWS.
#'
#' @rdname SaveDatatable
#' @aliases SaveDatatable AddInsertions AddModifications AddDeletions
#'  
#' @param changeset \code{\link{Changeset}} object
#' @param data data.table. 
#'  \itemize{ 
#'  \item AddInsertions Any __id or __ts column 
#'  will be stripped and then sent to the server as additional lines 
#'  \item
#'  AddModifications __id and __ts columns must be present and all data will be
#'  sent to the server as modifications 
#'  \item AddDeletions Only __id and __ts
#'  columns are required, all others will be stripped. Removes those entries
#'  from the table on the server
#'  }
#'  
#' @examples \dontrun{
#' library(data.table)
#' origtable <- "world_bank_climate_data"
#' table <- "world_bank_climate_data_campbells_20151123_faodomain_campbells_2311174949"
#' 
#' newdat <- ReadDatatable(table, readOnly = FALSE)
#' dat <- ReadDatatable(origtable, limit=10)
#' 
#' # Erase all data
#' changeset <- Changeset(table)
#' AddDeletions(changeset, newdat)
#' Finalise(changeset)
#' 
#' # Add new data
#' AddInsertions(changeset, dat)
#' AddInsertions(changeset, dat)
#' Finalise(changeset)
#' 
#' # Reload new table
#' newdat <- ReadDatatable(table, readOnly = FALSE)
#' 
#' # Double precipitation in first half
#' newdat[1:ncol(dat),precipitation := precipitation * 2]
#' AddModifications(changeset, newdat[1:nrow(dat),])
#' 
#' # Delete the second
#' AddDeletions(changeset, newdat[nrow(dat):(nrow(dat)*2),])
#' Finalise(changeset)
#'}
#'
#'  
#' @export AddInsertions

AddInsertions <- function(changeset, data){
  data <- copy(data)
  syscols <- which(colnames(data) %in% c("__id", "__ts"))
  
  if(length(syscols) > 0){
    set(data, j = syscols, value = NULL)
  }
  
  jsonlines <- vapply(split(data, seq_len(nrow(data))), function(x){
    jsonlite::toJSON(
      list(values=as.list(x), remove = FALSE),
      auto_unbox = TRUE)}, character(1)
  )
  
  combine_jsonlines(changeset, jsonlines)
  send_jsonlines(changeset)
}

#' @rdname SaveDatatable
#' @export AddModifications

AddModifications <- function(changeset, data){
  if(!all(c("__id", "__ts") %in% colnames(data))){
    stop("If rows are to be modified, they must have ids and timestamps")
  }
  
  jsonlines <- vapply(split(data, seq_len(nrow(data))), function(x){
    jsonlite::toJSON(
      list(values=as.list(x), remove = FALSE),
      auto_unbox = TRUE)}, character(1)
  )
  
  combine_jsonlines(changeset, jsonlines)
  send_jsonlines(changeset)
}

#' @rdname SaveDatatable
#' @export AddDeletions

AddDeletions <- function(changeset, data){
  if(!all(c("__id", "__ts") %in% colnames(data))){
    stop("If rows are to be deleted, they must have ids and timestamps")
  }
  
  data <- data[ , .(`__id`, `__ts`)]
  
  jsonlines <- vapply(split(data, seq_len(nrow(data))), function(x){
    jsonlite::toJSON(
      list(values=as.list(x), remove = TRUE),
      auto_unbox = TRUE)}, character(1)
  )
  
  combine_jsonlines(changeset, jsonlines)
  send_jsonlines(changeset)
}

#' Define changeset object
#' 
#' Changeset is called to initiate a Changeset object, an object which tracks 
#' the rows requests sent to it (be they insert, modify or delete). When it 
#' accumulates enough requests (by default, 5000 rows), it sends that chunk to 
#' the server. To finalise your script, use \code{Finalize} or \code{Finalise}
#' with the changeset object to indicate that there are no more rows to add and
#' that the process of writing to a table is finished.
#' 
#' @rdname Changeset
#' @aliases Finalise Finalize
#' @export Changeset
Changeset <- function(table){
  
  changeset <- new.env()
  class(changeset) <- append(class(changeset), "Changeset")
  
  pagesize <- FetchSWSVariable("changeset", .swsenv)$pagesize
  
  assign("jsonlines", character(0), envir = changeset)
  assign("pagesize", pagesize, envir = changeset)
  assign("config", FetchDatatableConfig(table), envir = changeset)
  
  changeset
}

#' @rdname Changeset
#' @param table character. Name of Datatable on server. Use 
#'   \code{\link{FetchDatatableConfig}} to get a list of tables on the server.
#' @param changeset Changeset object
#'   
#' @return \code{Changeset} returns a Changeset object that can be used with the
#'   \code{\link{SaveDatatable}} class of functions.
#' @export Finalize Finalise
#'   
Finalize <- Finalise <- function(changeset){
  jsonlines <- get("jsonlines", changeset)
  
  len <- seq_along(jsonlines)
  
  json <- paste0(jsonlines, collapse="\n")
  
  table <- names(get("config", envir=changeset))
  
  post_json(json, table)
  
  jsonlines <- jsonlines[-len]
  assign("jsonlines", jsonlines, envir=changeset)
}

combine_jsonlines <- function(changeset, jsonlines){
  
  assign("jsonlines", c(get("jsonlines", envir=changeset), jsonlines), envir=changeset)
  
}

send_jsonlines <- function(changeset){
  
  pagesize <- get("pagesize", envir = changeset)
  
  while(length(get("jsonlines", envir = changeset)) > pagesize) {
    
    jsonlines <- get("jsonlines", changeset)
    page <- jsonlines[seq_len(pagesize)]
    
    json <- paste0(page, collapse="\n")
    table <- names(get("config", envir=changeset))
    post_json(json, table)
    
    assign("jsonlines", jsonlines[-seq_len(pagesize)], envir = changeset)
    
  }
  
}

post_json <- function(json, table){
  baseurl <- paste(swsContext.baseRestUrl, "api", "datatable", table, "import", sep = "/")
  queryparams <- paste(paste0("_xid=", swsContext.token), collapse= "&")
  url <- paste(baseurl, queryparams, sep="?")  
  
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Accept: application/json",
                          "Content-Type: application/jsonl",
                          "Accept-Encoding: gzip, deflate")
  
  if (Sys.info()['sysname'] == 'Darwin') {
    
    curl::handle_setopt(h, customrequest="POST",
                        verbose = FALSE,
                        noproxy = .swsenv$swsContext.noProxy,
                        ssl_verifypeer = FALSE, 
                        sslcert = path.expand(.swsenv$swsContext.clientP12),
                        keypasswd = .swsenv$swsContext.p12Password,
                        ssl_verifyhost = 2,
                        post = 1,
                        postfields = json)
  } else {
    
    curl::handle_setopt(h, customrequest="POST",
                        verbose = FALSE,
                        noproxy = .swsenv$swsContext.noProxy,
                        ssl_verifypeer = FALSE, 
                        sslcert = path.expand(.swsenv$swsContext.clientCertificate),
                        sslkey = path.expand(.swsenv$swsContext.clientKey),
                        ssl_verifyhost = 2,
                        post = 1,
                        postfields = json)
  }
  
  
  
  on.exit(
    if(exists("conn")) {
      if(inherits(conn, "connection")){
        if(isOpen(conn)) close(conn)
      }
    }
  )
  
  conn <- try(.Call(curl:::R_curl_connection, url, "r", h, FALSE))
  responseCode <- curl:::handle_response_data(h)$status_code
  
  ## If there's an error before the connection can be properly created, close it
  if(inherits(conn, "try-error")){
    cons <- showConnections(all = TRUE)
    closableconn <- getConnection(as.numeric(row.names(cons)[cons[,"description"]  ==  url]))
    close(closableconn)
  }
  
  if(!(responseCode >= 200 && responseCode < 300)){
    errmessage <- paste0(readLines(conn, warn = FALSE), collapse="\n")
    HandleHTTPError(responseCode, errmessage)
  }
}
