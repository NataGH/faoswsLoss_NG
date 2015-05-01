##' This is a function checking whether the shares sums up to 100.
##'
##' @param shareData The share data returned from getShare.
##' 
##' @return This function is not currently used.
##' 
##' @export
##' 

checkShareUnity = function(shareData){
    checkShare =
        shareData[, list(sum_check = sum(Value_share)),
                  by = c(aupusParam$keyNames$countryName,
                      aupusParam$keyNames$itemChildName,
                      aupusParam$keyNames$yearName)]
    checkShare[sum_check != 100, ]
}
