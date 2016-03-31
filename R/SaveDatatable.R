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
#' table <- "world_bank_climate_data_test"
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
    
    url <- get("url", envir = changeset)
    
    post_json(url, json)
    
    assign("jsonlines", jsonlines[-seq_len(pagesize)], envir = changeset)
    
  }
  
}

post_json <- function(url, json){
  
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Accept" = "application/json",
                          "Content-Type" = "application/jsonl",
                          "Accept-Encoding" = "gzip, deflate")
  
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
    errmessage <- paste0(readLines(conn, warn = FALSE, encoding = "UTF-8"), collapse="\n")
    HandleHTTPError(responseCode, errmessage)
  }
}
