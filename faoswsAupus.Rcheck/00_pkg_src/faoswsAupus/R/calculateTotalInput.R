##' Calculate Total Input
##' 
##' It's not clear what this function was intended for, as it is not
##' called by any other functions in this package.
##' 
##' @param inputData The data containing the input extracted from the
##' function getInputFromProcess after running the
##' updateInputFromProcess function.
##' @param inputNum ???
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function is not currently used.
##' 
##' @export
##' 

calculateTotalInput = function(inputData, inputNum, aupusParam){
    newKey = with(aupusParam$keyNames,
        c(countryName, itemChildName, yearName))
    evalString = paste0("list(", inputNum, " = sum(", inputNum, "))")
    aggregatedInput =
        inputData[, eval(parse(text = evalString)),
                  by = newKey]
    setkeyv(aggregatedInput, newKey)
    setnames(aggregatedInput,
             old = aupusParam$keyNames$itemChildName,
             new = aupusParam$keyNames$itemName)
    aggregatedInput
}
