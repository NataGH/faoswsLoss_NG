##' Function to fill symbol based on calculated value
##'
##' The function will take a vector of values and fill in the new
##' symbol if the value is not missing. If the value is missing, it
##' will insert the missing symbol.
##'
##' @param value The vector of calculated values
##' @param newSymbol The new symbol to be assigned
##' @param missingSymbol The symbol filled in when value is missing
##' 
##' @return A list of two elements: the vector of passed values and the new
##' derived vector of symbols.
##'
##' @export
##' 

appendSymbol = function(value, newSymbol, missingSymbol = "M"){
    symb = rep(missingSymbol, length(value))
    symb[which(!is.na(value))] = newSymbol
    list(value, symb)
}
