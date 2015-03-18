##' This function creates columns which are missing.
##'
##' Some countries may not have a certain element calculated and thus
##' when the data is extracted, the column is missing. This function
##' will recreate the missing column.
##'
##' @param data The data
##' @param allColumn The theoretical set of all column names.
##' 
##' @return This function does not seem to be currently in use.
##' 
##' @export
##' 

fillMissingColumn = function(data, allColumn){
    missingColumn = allColumn[!allColumn %in% colnames(data)]
    columnType = sapply(data[, allColumn[allColumn %in% colnames(data)],
        with = FALSE], typeof)[1]
    if(length(missingColumn) > 0)
        data[, `:=`(c(missingColumn),
                    list(as.vector(NA, mode = columnType)))]
}
