##' Transfer Symbol
##' 
##' This function contains the logic for updating a symbol when it is
##' transfered to a new cell.
##'
##' @param symb The original symbol to be updated.
##' 
##' @return The updated symbol.
##' 
##' @export

transferSymb = function(symb){
    transferedSymb = symb
    transferedSymb[symb == "*"] = "X"
    transferedSymb[symb %in% c("F", "T")] = "C"
    transferedSymb[!symb %in% c("*", "F", "T")] = "/"
    transferedSymb
}
