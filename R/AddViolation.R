#' Add violation to faosws
#' 
#' To mark a row as valid, just return NA for everything except ID Any element
#' which is not otherwise marked as invalid will be considered valid. When a
#' changset is finalised, the whole table is considered as validated until a
#' cell changes value.
#'
#' @param changeset \link{Changeset} object.
#' @param violationtable. data.table with the following columns:
#' \itemize{
#'   \item __id id columns from data
#'   \item column character. Column name in data
#'   \item type character. One of 'error' or 'warning'
#'   \item severity integer from 1 to 10.
#'   \item message character. Reason for invalidating
#'   }
#' 
#' @examples 
#' \dontrun{
#' table <- "world_bank_climate_data_test"
#' newdat <- ReadDatatable(table, validationOptions = list(incremental = FALSE))
#' inval <- newdat[precipitation > temperature, .(id)]
#' inval[,`:=`(col = "precipitation", type="error", gravity=9, message="precipitation is greater than temperature")]
#' vchangeset <- Changeset(table, type = "validation")
#' AddViolation(vchangeset, inval)
#' Finalise(vchangeset)
#' }
#' 
#' @return Doesn't return anything, but modifies the provided changeset
#' @export AddViolation
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
  
  dups <- duplicated(violationtable[, .(`__id`, column)])
  
  if(any(dups)){
    stop("The ids: ", unique(violationtable[dups, `__id`]), " have duplicate elements")
  }
  
  # Prevent user from adding valid and invalid rows together.
  
  find_na_add <- function(sd){
    any(is.na(sd[, column])) && nrow(sd > 1)
  }
  
  na_add <- violationtable[, .(badnas = find_na_add(.SD)) , by=`__id`][,badnas]
  
  if(any(na_add)){
    stop("Trying to have validated and unvalidated at the same time")
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
