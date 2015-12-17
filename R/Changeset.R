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
#' 
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
  
  table <- names(get("config", envir = changeset))
  
  baseurl <- paste(swsContext.baseRestUrl, "api", "datatable", table, "import", sep = "/")
  queryparams <- paste(paste0("_xid=", swsContext.token), collapse= "&")
  url <- paste(baseurl, queryparams, sep="?")  
  
  post_json(url, json)
  
  jsonlines <- jsonlines[-len]
  assign("jsonlines", jsonlines, envir = changeset)
}