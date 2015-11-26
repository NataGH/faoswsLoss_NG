#' Save data to Datatable
#' 
#' validation hasn't been implemented yet
#' 
#' @rdname SaveDatatable
#' 
#' @examples \dontrun{
#' table <- "world_bank_climate_data"
#' ntariff10K <- ReadDatatable(table, limit = 1e3, readOnly = F)
#'  
#' table <- "world_bank_climate_data_campbells_20151123_faodomain_campbells_2311174949"
#' changeset <- Changeset(table, pagesize = 10)
#' 
#' 
#' i <- 0 
#' insertRows(changeset, table, ntariff10K[(i+1):(i+5),])
#' i <- i+5
#' finalise(changeset)
#'}
#' 
#' @export Changeset
#' @export insertRows
#' @export finalise
#' @export finalize

insertRows <- function(changeset, table, data){
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

Changeset <- function(table, pagesize = 5000){
  
  changeset <- new.env()
  class(changeset) <- c(class(changeset), "Changeset")
  
  assign("pagesize", pagesize, envir = changeset)
  assign("metadata", ReadDatatableList(table), envir = changeset)
  
  changeset
}

finalize <- finalise <- function(changeset){
  jsonlines <- get("jsonlines", changeset)
  json <- paste0(jsonlines, collapse="\n")
  
  table <- names(get("metadata", envir=changeset))
  
  post_json(json, table)
  rm("jsonlines", envir = changeset)
}

combine_jsonlines <- function(changeset, jsonlines){
  
  if(exists("jsonlines", envir=changeset)){
    assign("jsonlines", c(get("jsonlines", envir=changeset), jsonlines), envir=changeset)
  } else {
    assign("jsonlines", jsonlines, envir=changeset)
  }
}

send_jsonlines <- function(changeset){
  
  pagesize <- get("pagesize", envir = changeset)
  
  while(length(get("jsonlines", envir = changeset)) > pagesize) {
    
    jsonlines <- get("jsonlines", changeset)
    page <- jsonlines[seq_len(pagesize)]
    
    json <- paste0(page, collapse="\n")
    table <- names(get("metadata", envir=changeset))
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
  
  curl::handle_setopt(h, customrequest="POST",
                      verbose = FALSE,
                      noproxy = .swsenv$swsContext.noProxy,
                      ssl_verifypeer = FALSE, 
                      accept_encoding = "gzip, deflate",
                      sslcert = path.expand(.swsenv$swsContext.clientCertificate),
                      sslkey = path.expand(.swsenv$swsContext.clientKey),
                      ssl_verifyhost = 2,
                      post = 1,
                      postfields = json)
  
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