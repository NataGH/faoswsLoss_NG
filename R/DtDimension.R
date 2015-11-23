#' DtDimension class
#' 
#' This class is for specifying Datatable columns
#' 
#' @aliases DtDimension
#'   
#' @slot id character. Column name.
#' @slot ascending. logical. Should this column be ascending (TRUE) or
#'   descending (FALSE) order?
#' @slot value. For character columns, specify a single filter value. More
#'   complex filtering can be placed in the where clause of
#'   \code{\link{ReadDatatable}}
#'   
#' @examples
#' mapply(new, id=letters[1:3], value=1:3, MoreArgs = list(Class="DtDimension"))
#' 
#' @export DtDimension
#' @export as.list.DtDimension
#' @export



DtDimension <- setClass("DtDimension", slots = list(id = "character", ascending = "logical", value = "ANY"),
         prototype = prototype(ascending = NA, value = NA_real_))

setValidity("DtDimension", function(object) {
  
  msg <- NULL
  valid <- TRUE
  
  #id validation
  if (length(object@id) != 1 | is.na(object@id)) {
    valid <- FALSE
    if (length(object@id) == 0 | is.na(object@id)) {
      msg <- c(msg, "An id attribute must be specified")
    } else {
      msg <- c(msg, "The id attribute was not properly set (not length 1)")
    }
  }
  
  #ascending validation
  if (length(object@ascending) != 1) {
    valid <- FALSE
    msg <- c(msg, "The ascending attribute was not properly set (not length 1)")
  }
  
  #value validation
  if (length(object@value) != 1) {
    valid <- FALSE
    msg <- c(msg, "The value attribute was not properly set (not length 1)")
  }
  
  if (valid) {
    TRUE
  } else {
    msg
  }
})

as.list.DtDimension <- function(x, ...){
  
  list(id = x@id, sort = ifelse(x@ascending, "ASCENDING", "DESCENDING"), value = x@value)
  
}
