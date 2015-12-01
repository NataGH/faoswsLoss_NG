#' Merge All Loss Data
#'



mergeAllLossData = function(lossData, ...){
  explanatoryData = list(...)
  Reduce(f = function(x, y){
    keys = intersect(colnames(x), colnames(y))
    setkeyv(x, keys)
    setkeyv(y, keys)
    merge(x, y, all.x = TRUE)
  },
  x = explanatoryData, init = lossData
  )
}