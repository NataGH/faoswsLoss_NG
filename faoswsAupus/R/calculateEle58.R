##' Calculate Element 58 (Production Crop Year)
##' 
##' Calculation of element 58 is done by adding together element 3158 and 3159
##' for the source commodities.
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 58's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle58 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = 58, aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    
    sumCropYear = function(data){
        unlist(data[get(itemTypeCol) == 3158, get(element58Num)]) +
            unlist(data[get(itemTypeCol) == 3159, get(element58Num)])
    }

    data[, cropYear := sumCropYear(.SD), by = c(aupusParam$keyNames$yearName)]
    replaceIndex1 = which(data[get(itemTypeCol) == 57 &
                          replaceable(get(element58Symb), get(cropYear)), ])
    data[replaceIndex1,
         `:=`(c("element58Num", "element58Symb"),
              appendSymbol(cropYear, "C"))]
    data[, cropYear := NULL]
    replaceIndex1
}
