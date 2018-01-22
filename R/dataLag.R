#' Part of the FAO Loss Module
#' 
#' @author Alicia English

# DataIn <- datamod
# indexVar <- keys_lower
# var <- "losstransf" 
# timeVar <- "timepointyears" 
# LType <- 'fullset'

dataLag <- function(DataIn,indexVar,var,timeVar,lag,LType){ 
  # Data in is the dataset
  # indexVar is the index of varaibles that should remain constant
  # var is the variable to be lagged
  if(LType == 'fullset'){
    
    timelag = DataIn[,unique(c(timeVar,indexVar,var)),with=F]
    timelag[,'lag' :=  timelag[,timeVar,with=F] - lag]
    timelag[, noquote(timeVar):= NULL]
    names(timelag)[names(timelag)=='lag']<-timeVar
    names(timelag)[names(timelag)==var]<-paste(var,"_lag",as.character(lag),sep="")
    
    DataIn = merge(DataIn,timelag, by=keys_lower, all.x= TRUE)
  }
    return(DataIn)   
}