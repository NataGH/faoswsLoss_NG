##' Fill Balance
##' 
##' This function determines how the balance and statistical discrepancy
##' columns should be filled in.  There are two scenarios:
##' 
##' \itemize{
##'     \item If the symbol currently in the balance column is "replaceable"
##'     (see ?replaceable) and if the computed balance is positive, then the
##'     computed balance is placed in the balance column and statistical
##'     discrepancy is set to 0.
##'     \item If the computed balance is negative, or if the current balance
##'     symbol is not replaceable, then the computed balance is placed in the
##'     statistical discrepancy column.
##' }
##'
##' @param calculatedBalance The column name of data corresponding to the
##' balance calculated by the function calculateBalance.
##' @param balanceNum The column name of data corresponding to the value of
##' the balancing element.
##' @param balanceSymb The column name of data corresponding to the symbol
##' column for the balancing element.
##' @param element181Num The column name of data corresponding to the value of
##' element 181 (Statistical Discrepancies).
##' @param element181Symb The column name of data corresponding to the symbol
##' of element 181 (Statistical Discrepancies).
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' 
##' @return A list with 5 vectors of the same length:
##' \itemize{
##'     \item originalValue: The numeric vector of the updated balance column.
##'     \item originalSymb: The symbol vector of the updated balance column.
##'     \item discrepancyValue: The numeric vector for the statistical
##'     discrepancy column.
##'     \item discrepancySymb: The symbol vector for the statistical
##'     discrepancy column.
##'     \item replaced: A logical vector which is TRUE if the computed value
##'     has been placed in the balance element column and FALSE otherwise.
##' }
##' 
##' @export
##' 

fillBalance = function(calculatedBalance, balanceNum, balanceSymb,
    element181Num, element181Symb, data){
    ## Replace if the balancing element is replaceable and calculated
    ## balancing value is greater than zero
    replaceIndex = (replaceable(data[, balanceSymb, with = FALSE]) &
                        data[, calculatedBalance, with = FALSE] > 0)[, 1]

    originalValue = unlist(data[, balanceNum, with = FALSE])
    originalValue[replaceIndex] =
        unlist(data[replaceIndex, calculatedBalance, with = FALSE])
    originalSymb = unlist(data[, balanceSymb, with = FALSE]) 
    originalSymb[replaceIndex] = "B"

    discrepancyValue = unlist(data[, element181Num, with = FALSE])
    discrepancyValue[replaceIndex] = 0
    discrepancySymb = unlist(data[, element181Symb, with = FALSE])
    discrepancySymb[replaceIndex] = "B"

    ## If either is not satisfied, then set the balancing element to
    ## 0 and the calculated balancing value to statistical
    ## discrepancy.
    discrepancyValue[!replaceIndex] =
        unlist(data[!replaceIndex, calculatedBalance, with = FALSE])
    discrepancySymb[!replaceIndex] = "B"
    replaced = rep(FALSE, NROW(data))
    replaced[replaceIndex] = TRUE
    list(originalValue, originalSymb, discrepancyValue,
         discrepancySymb, replaced)
}
