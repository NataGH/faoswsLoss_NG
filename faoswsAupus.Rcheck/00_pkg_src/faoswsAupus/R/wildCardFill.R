##' Wild Card Fill
##' 
##' The function takes wild card data to fill in original data for a
##' particular variable.
##'
##' Shares, ratio and balance element have area and year specific
##' rates, but at the same time they have wild card values which are
##' to be applied when the specific rates are not available.
##'
##' This function fills in the gap with wild card values when year
##' specific values are not available.
##' 
##' Note: this function is usually called several times to fill in one main
##' data.table (originalData) with data from other data.tables (passed as
##' wildCardData, and these tables have varying levels of specificity).  The
##' first time this function is called, it should be given the most specific
##' data.  Then, future calls should receive more and more general tables.  The
##' rationale is that the later tables will only overwrite values which are
##' currently missing and no more.
##'
##' @param originalData The data to be filled in.  This is typically a
##' data.table with all year/location pairs.
##' @param wildCardData The wild card data, see ratio or shares data extracted
##' from the getShare and getRatio function.  This should be a data.table with
##' values to be inserted into originalData.
##' @param variable The column name (of both originalData and wildCardData) of
##' the variable to be filled.
##' @param verbose Whether the output should be printed.
##' 
##' @return No data is returned, but missing records in originalData are
##' replaced by their corresponding records in wildCardData.
##' 
##' @export
##' 

wildCardFill = function(originalData, wildCardData, variable,
    verbose = FALSE){
    
    ## Data Quality Checks
    stopifnot(is(originalData, "data.table"))
    stopifnot(is(wildCardData, "data.table"))
    stopifnot(variable %in% colnames(originalData))
    stopifnot(variable %in% colnames(wildCardData))
    stopifnot(is(verbose, "logical"))
    
    if(verbose)
        cat("Number of missing observations for variable", variable, ":",
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
    
    ## Fill in the missing values
    evalText = paste0(variable, " := i.", variable)
    okey = key(originalData)
    ## Reorder originalData key's to match with wildCardData keys
    setkeyv(originalData, c(key(wildCardData),
                            okey[!okey %in% key(wildCardData)]))
    index = unique(wildCardData[originalData[is.na(get(variable)),
        key(originalData), with = FALSE], ][!is.na(get(variable)), ],
        by = NULL) #ensure all columns are used for uniqueness check
    setkeyv(index, key(originalData))
    originalData[index[!is.na(get(variable)), 
                       c(key(index), variable),
                       with = FALSE],
                 eval(parse(text = evalText))]
    setkeyv(originalData, okey)
    
    if(verbose)
        cat("New number of missing observations for variable", variable, ":",
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
}
