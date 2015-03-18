##' Calculate Element 151 (Reemployment other sector)
##' 
##' In almost all cases, element 151 (Reemployment other sector) is computed as
##' a ratio of the total supply: E_151 = R_151 * (total supply) / 100.
##' However, for one particular commodity (code 1687, Charcoal), element 151 is
##' instead computed as the value of element 131 for commodity 1648 minus the
##' value of element 51 for commodity 1687.
##' 
##' @param stotal The column name of data corresponding to total supply.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector
##' containing the row numbers of observations which were updated.
##' However, it also has a side effect: rows within the
##' passed data.table ("data") have element 151's value and symbol updated.
##' 
##' @export
##' 

calculateEle151 = function(stotal, data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(51, 131, 151), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    itemCol = aupusParam$keyNames$itemName

    replaceIndex1 = data[, get(itemTypeCol) != 1687 &
                              replaceable(get(element151Symb),
                                          get(ratio151Num) * get(stotal))]
    data[replaceIndex1,
         `:=`(c(element151Num, element151Symb),
              appendSymbol(get(ratio151Num) * get(stotal) / 100, "C"))]

    ## Item Charcoal is a special case
    groupKey = key(data)[key(data) != itemCol]
    charcoalSubset = data[get(itemCol) == "1684",
                          list(charcoalUpdateValue = get(element131Num)),
                          by = groupKey]
    if(nrow(charcoalSubset) >= 1){
        setkeyv(charcoalSubset, groupKey)
        ## Reorder data's key so joining works (if two datasets are joined and one
        ## dataset's keys are a subset of the other dataset's keys, then the
        ## dataset with more keys should have all the first keys matching the other
        ## dataset).
        setkeyv(data, c(groupKey, itemCol))
        data[charcoalSubset, charcoalUpdateValue := charcoalUpdateValue]
        replaceIndex2 = data[, get(itemCol) == "1687" &
            !is.na(charcoalUpdateValue) &
            replaceable(get(element151Symb), charcoalUpdateValue)]
        data[replaceIndex2,
             `:=`(c(element151Num, element151Symb),
                  appendSymbol(charcoalUpdateValue - get(element51Num),
                               "C"))]
        data[, charcoalUpdateValue := NULL]
    }
    
    if(exists("replaceIndex2"))
        replaceIndex1 = replaceIndex1 | replaceIndex2
    which(replaceIndex1)
}
