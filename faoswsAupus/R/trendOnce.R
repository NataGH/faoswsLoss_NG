##' Trend Once
##' 
##' "Trending" the data refers to carrying over past values.  For example,
##' trending c(1,2,NA,3) would yield c(1,2,2,3).  However, missing values are
##' only replaced if their corresponding symbols are "T" or "C".
##' 
##' This function trends the passed data (Num) assuming a vector of symbols
##' (Symb).  The "transfer" argument allows the user to specify if the symbol
##' should also be transferred when the number is copied forward.
##'
##' @param Num A vector of numeric values to be trended.
##' @param Symb A vector of the same length as Num containing the symbols (or
##' "flags") for the observations in Num.
##' @param applyIndex This argument allows the user to restrict which values
##' can be trended.  By default, all values are included.
##' @param transfer Should the symbol be converted by the function
##' transferSymb.
##' 
##' @return A list containing the values and symbols after the trending has
##' occurred.
##' 
##' @export
##' 

trendOnce = function(Num, Symb, applyIndex = 1:length(Num),
                     transfer = FALSE){
    value = c(NA, Num)
    symb = c(NA, Symb)
    newTrendIndex = intersect(applyIndex + 1,
        which(is.na(value) & symb %in% c("T", "C")))
    value[newTrendIndex] = value[newTrendIndex - 1]
    if(transfer){
        symb[newTrendIndex] = transferSymb(symb[newTrendIndex - 1])
    } else {
        symb[newTrendIndex] = "T"
    }
    trendedOnceValue = value[-1]
    trendOnceSymb = symb[-1]
    list(trendedOnceValue, trendOnceSymb)
}
