#' Add violation to faosws
#' 
#' To mark a row as valid, just return NA for everything except ID
#'
#' @param changeset \link{Changeset} object.
#' @param violationtable. data.table with the following columns:
#' \itemize{
#'   \item __id id columns from data
#'   \item column character. Column name in data
#'   \item type character. One of 'error' or 'warning'
#'   \item severity integer from 1 to 10.
#'   \item message character. Reason for invalidating}
#'
#' @export 
#' 


AddViolation <- function(changeset, violationtable) {
  
  if (get("type" , envir = changeset) != "validation") stop("Changeset is not of type 'validation'")
  
  valid_cols <- c("__id", "column", "type", "severity", "message")
  
  violationtable <- violationtable[, .SD, .SDcols = valid_cols]

  ## Validate table
  stopifnot(is.character(violationtable[, column]) || all(is.na(violationtable[, column])))
  stopifnot(all(violationtable[!is.na(column), type] %in% c("error", "warning")))
  stopifnot(all(violationtable[!is.na(column), severity] %in% 1:10))
  stopifnot(is.character(violationtable[, message]) || all(is.na(violationtable[, message])))
  
  newids <- unique(violationtable[,`__id`])
  
  # Check that the row hasn't already been submitted
  validation_ids <- get("validation_ids", envir = changeset)
  
  if (any(newids %in% validation_ids)) {
    stop("The following ids already have a validation: ", 
         paste0(intersect(newids, get("validation_ids", envir = changeset)), collapse = ", "))
  }
  
  # Reject if there are duplicates
  
  dups <- duplicated(violationtable[!is.na(column), .(`__id`, column)])
  if(any(dups)){
    stop("The ids: ", unique(violationtable[dups, `__id`]), " have duplicate elements")
  }
  
  ## CREATE LINES
  ## This can be better optimised with a data.table function
  dtl <- split(violationtable[, .SD, .SDcols = -"__id"], violationtable[, `__id`])
  sp <- function(x){
    lapply(split(x[,!"column", with = FALSE], x[,column]), as.list)
  }
  
  dtl <- lapply(dtl, sp)
  dtl <- Map(function(x,y){y <- c(id_row = as.integer(x), list(violations = y))}, names(dtl), dtl)
  
  jsonlines <- vapply(dtl, jsonlite::toJSON, character(1), auto_unbox = TRUE, na = "null")
  
  combine_jsonlines(changeset, jsonlines)
  assign("validation_ids", c(validation_ids, newids), envir = changeset)
  
}

