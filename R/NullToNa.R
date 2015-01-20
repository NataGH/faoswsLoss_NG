##' Null to NA
##' 
##' @param nullList
##' 

NullToNa = function(nullList) {
  if (is.null(nullList) || !is.list(nullList)) {
    nullList
  } else {
    if (length(nullList) == 0) {
      vector(mode = "character")
    } else {
      listType =
        unique(na.omit(sapply(nullList,
            FUN = function(x){
              ifelse(is.null(x), NA, typeof(x))
            }
        )))
      if(length(listType) == 0)
        listType = "character"
      vector = as.vector(rep(NA, length = length(nullList)),
                         mode = listType)
      validEntry = which(sapply(nullList, FUN = function(x) !is.null(x)))
      vector[validEntry] =
        unlist(nullList[validEntry])
      vector
    }
  }
}
