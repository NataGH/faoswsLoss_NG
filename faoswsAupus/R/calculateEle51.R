##' Calculate Element 51
##' 
##' There is no calculation of element 51 (Output) for almost all cases.  Two
##' special cases exist, however:
##' \itemize{
##'     \item If the commodity item type is in aupusGroups$escrSugarProduction
##'     and if element 58 (Production Crop Year) exists for the commodity of
##'     interest, then element 51 is set to the value of element 58.
##'     \item If the commodity code is 3183, then element 51 (Output) is set
##'     to the sum of element 51 (Output) for commodity codes 3158 and 3159.
##' }
##'
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns an integer vector containing the row numbers
##' of observations which were updated.  However, it also has a side effect:
##' rows within the passed data.table ("data") have element 51's value and
##' symbol updated.
##' 
##' @export
##' 

calculateEle51 = function(data, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    defineElementVariables(elements = c(51, 58), aupusParam = aupusParam)
    itemTypeCol = aupusParam$keyNames$itemTypeName
    itemCol = aupusParam$keyNames$itemName
        
    ## For elements of ESCR sugar commodity types, element 51 is replaced with
    ## element 58
    replaceIndex1 = data[, itemTypeCol %in% aupusGroups$escrSugarProduction &
                         replaceable(get(element51Symb), get(element58Num))]
    data[replaceIndex1, 
         `:=`(c(element51Num, element51Symb),
              appendSymbol(get(element58Num), "C"))]

    ## For one special commodity (3183), the value of element 51 is updated by
    ## adding the values of element 51 for commodities 3158 and 3159.  This is
    ## accomplished here in R by computing the sum of element 51 for 3158 and
    ## 3159 in the sugarSubset data.table and then updating data with these new
    ## values.  Note: the documentation from Nick (aupus_code_analysis.pdf)
    ## indicates two different possibilities: summing 3158 with 3159 or 3158 with
    ## 5158.  The first is correct, and is implemented here.
    groupKey = key(data)[key(data) != itemCol]
    sugarSubset = data[get(itemCol) %in% c(3158, 3159),
                       list(newSum = sum(get(element51Num))), by = groupKey]
    if(nrow(sugarSubset) >= 1){
        setkeyv(sugarSubset, groupKey)
        ## Reorder data's key so joining works (if two datasets are joined and one
        ## dataset's keys are a subset of the other dataset's keys, then the
        ## dataset with more keys should have all the first keys matching the other
        ## dataset).
        setkeyv(data, c(key(sugarSubset), itemCol))
        data[sugarSubset, sugarSum := newSum]
        replaceIndex2 = data[, get(itemCol) == "3183" &
            replaceable(get(element51Symb), get(sugarSum)) &
            !is.na(get(sugarSum))]
        data[replaceIndex2,
             `:=`(c(element51Num, element51Symb),
                  appendSymbol(sugarSum, "C"))]
        data[, sugarSum := NULL]
    } else {
        replaceIndex2 = rep(F, nrow(data))
    }

    which(replaceIndex1 & replaceIndex2)
}
