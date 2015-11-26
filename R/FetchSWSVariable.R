#' Function to fetch name of current Datatable
#' 
#' This function gets the name of the current Datatable that the token is
#' pointed toward. The internals may change at any time and it is not
#' recommended to access the object directly.
#' 
#' @export BoundDatatable

BoundDatatable <- function(){
  
  tableName <- try(FetchSWSVariable(variable = "swsContext.computationParams", environment = .GlobalEnv)[[".DATATABLE"]], silent = TRUE)
  
  if(inherits(tableName, "try-error") || is.null(tableName)){
    stop("No table found")
  }
  tableName
}

FetchSWSVariable <- function (variable, environment) {
  
  if(missing(environment)){
    
    returnedVariable <- get(variable , envir = .swsenv, inherits = FALSE)
    
    if(inherits(returnedVariable, "try-error")) {
      if(grepl("^object '.+' not found", attr(returnedVariable, "condition")[["message"]])){
        returnedVariable <- get(variable, envir = .GlobalEnv, inherits = FALSE)
      } else {
        stop(attr(returnedVariable, "condition")[["message"]])
      }
    }
  } else {
    returnedVariable <- get(variable , envir = environment)
  }
  
  returnedVariable
}

