##' Returns the index of which values can be replaced
##'
##' Only data with replaceable symbols are replaced by the
##' module. This function will return the index in which the values
##' can be replaced.
##' 
##' @param symb The vector of symbols to be tested.
##' @param newValue The new value which can be written to the cell.  The AUPUS
##' logic states that the old value should not be replaced if it was trended
##' and the new value is missing (0 in the old framework).
##' @param replaceableSymb A vector specifying which symbols can be replaced.
##' 
##' @return A logical vector indicating if each individual element can be
##' replaced.
##' 
##' @export
##' 

replaceable = function(symb, newValue = 0, replaceableSymb = c("C", "T", "M")){
    symb %in% replaceableSymb &
        ## don't replace if trended and replace value is NA
        !(symb == "T" & is.na(newValue))
}
