##' Balance Production Elements
##' 
##' This function takes a data.table and balances the production elements (i.e.
##' elements 31, 41, and 51).  The relationship is
##' 51 = 31 * 41 / divisionFactor, where divisionFactor is either 1, 1000, or
##' 10000 depending on the item type.
##' 
##' If two elements are available, the third will be computed.  If only one or
##' zero elements are available, no values will be updated.  divisionFactor
##' should always be supplied.
##' 
##' @param value31 A vector of values corresponding to element 31 (Actual
##' Producing Factor).
##' @param value41 A vector of values corresponding to element 41 (Productivity
##' Element).
##' @param value51 A vector of values corresponding to element 51 (Output).
##' @param symb31 A vector of sybmols corresponding to element 31.  These are
##' provided so that they can be returned (updated, if necessary, when a
##' balance occurs).
##' @param symb41 A vector of sybmols corresponding to element 41.  These are
##' provided so that they can be returned (updated, if necessary, when a
##' balance occurs).
##' @param symb51 A vector of sybmols corresponding to element 51.  These are
##' provided so that they can be returned (updated, if necessary, when a
##' balance occurs).
##' @param divisionFactor A numeric vector taking values of 1, 1000, or 10000.
##' This number is required for the balance equation, and is determined based
##' on item type.
##' 
##' @return A list of 6 elements containing values 31, 41, and 51 and symbols
##' 31, 41, and 51.  All elements will be the same as what was passed except
##' when a balance was possible, and in this case one of the three values will
##' be updated along with it's symbol set to "C".
##' 

balanceProductionElements = function(value31, value41, value51,
                                     symb31, symb41, symb51,
                                     divisionFactor){
    ## Data Quality Checks
    stopifnot(divisionFactor %in% c(1, 1000, 10000))
    stopifnot(is.numeric(c(value31, value41, value51)))
    stopifnot(is.character(c(symb31, symb41, symb51)))
    
    ## Replace element 31
    replace31 = is.na(value31) & !is.na(value41) & !is.na(value51) &
        replaceable(symb31)
    value31[replace31] = divisionFactor *
        faoswsUtil::computeRatio(value51, value41)
    symb31[replace31] = "C"

    ## Replace element 41
    replace41 = !is.na(value31) & is.na(value41) & !is.na(value51) &
        replaceable(symb41)
    value41[replace41] = divisionFactor *
        faoswsUtil::computeRatio(value51, value31)
    symb41[replace41] = "C"

    ## Replace element 51
    replace51 = !is.na(value31) & !is.na(value41) & is.na(value51) &
        replaceable(symb51)
    value51[replace51] = value31 * value41 / divisionFactor
    symb51[replace51] = "C"
    
    ## Return the list
    return(list(value31, value41, value51, symb31, symb41, symb51))
}