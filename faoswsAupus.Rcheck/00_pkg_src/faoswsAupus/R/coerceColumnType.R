##' Coerce Column Types
##' 
##' Currently, the working system may not provide the correct column types for
##' datasets, and this can cause problems during joins at later stages (as keys
##' are sometimes numeric and sometimes character, and a failure to match leads
##' to errors).  Thus, this function coerces all column types that it can,
##' based on the aupusParam argument.  For example,
##' aupusParam$keyNames$areaName gives the name of the area id column of data,
##' and thus this column should always be coerced to a numeric column.
##' 
##' Note: there is currently a ticket in the SWS (issues SWS-797) that should
##' resolve this problem.  However, we need a work-around until that issue has
##' been resolved.
##' 
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param data The data.table whose column types should be coerced.
##' 
##' @return A data.table with updated column types.
##' 
##' @export
##' 

coerceColumnTypes = function(aupusParam, data){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(data, "data.table"))

    ## Determine which columns should be numeric and which should be character
    par = aupusParam$keyNames
    ## Build a regular expression to match column names.  Use ^<name>$ to match
    ## some columns exactly, such as areaName or itemName.  Also, all the
    ## columns like Value_measuredElement_51 should be matched, and the last
    ## two regex expressions do that.
    numericColumns = grep(paste0("(^", par$areaName, "$|^", par$itemName,
            "$|^", par$itemParentName, "$|^", par$itemChildName,
            "$|^", par$itemTypeName, "$|^", par$extractionRateName,
            "$|^", par$balanceElementName, "$|^", par$inputName,
            "$|^", par$shareName, "$|^", par$yearName,
            "$|^", par$valuePrefix, par$elementName, "_[0-9]{2,3}",
            "$|^", par$ratioPrefix, par$elementName, "_[0-9]{2,3}$)" ),
        colnames(data))
    characterColumns = grep(paste0("^", par$flagPrefix, par$elementName,
                                   "_[0-9]{2,3}$"), colnames(data))
    
    ## Update the column types
    originalKey = key(data)
    for(name in colnames(data)[numericColumns])
        data[, c(name) := as.numeric(get(name))]
    for(name in colnames(data)[characterColumns])
        data[, c(name) := as.character(get(name))]
    setkeyv(x = data, cols = originalKey)
    
    return(data)
}