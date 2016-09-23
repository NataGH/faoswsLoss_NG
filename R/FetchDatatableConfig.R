#' Read Datatable metadata list
#' 
#' @aliases FetchDatatableNames
#'   
#' @description These functions pull metadata regarding Datatables. If 
#'   \code{\link{GetTestEnvironment}} is not called beforehand, a URL may be 
#'   supplied, allowing access to this metadata without a token. Of particular 
#'   note in each field is the 'label' element which gives the proper name of
#'   the table as is displayed on the web interface of the Statistical Working
#'   System
#'   
#'   \code{FetchDatatableNames} is a simpler version of 
#'   \code{FetchDatatableConfig} which gives the names of all available tables
#'   
#' @param tables character. Optional specification of tables of interest.
#' @param baseurl character. If specified, uses this URL instead of looking for 
#'   one obtained from a token. Must be of the form 
#'   \code{http(s)://hostname:port/sws/rest}.
#'   
#' @return For \code{FetchDatatableConfig}, a list containing Datatable metadata.
#'   For \code{FetchDatatableNames}, a character vector of available Datatables.
#'   
#' @export FetchDatatableConfig FetchDatatableNames

FetchDatatableConfig <- function(tables, baseurl = swsContext.baseRestUrl){
  rawDt <- GetRestCall(paste0(baseurl, "/datatable"))
  Dt <- setNames(rawDt, vapply(rawDt, getID, character(1)))

  if(!missing(tables)) {
    nonexistingtables <- !(tables %in% names(Dt))
    if(any(nonexistingtables)) stop("Table(s) don't exist: ", paste(tables[nonexistingtables], collapse = ", "))
    Dt <- Dt[tables]
    }

  Dt <- lapply(Dt, function(x){
    cols <- x[["columns"]]
    x[["columns"]] <- setNames(cols, vapply(cols, getID, character(1)))
    x
  })

  Dt
}

#' @rdname FetchDatatableConfig
FetchDatatableNames <- function(baseurl = swsContext.baseRestUrl){
  rawDt <- GetRestCall(paste0(baseurl, "/datatable"))
  vapply(rawDt, getID, character(1))
}


getID <- function(cols){
  cols[["id"]]
}
