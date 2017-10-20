#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English
#' 

factortoNumeric <- function(variable){
  # Adjusts the factors to numerical  
  # Args:
  #  variable: IN the tables in R the variables become levels and are no longer numeric
  # Returns: 
  #   variable
  if(typeof(variable) == "list"){
    variable<- as.factor(unlist(variable))
    variable<- as.numeric(levels(variable))[variable]
  }
  if(is.factor(variable) ){
    variable<- as.numeric(levels(variable))[variable]
  }else{variable<-variable}
  return(variable)
} 