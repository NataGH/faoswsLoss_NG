##' Calculate Element 111 (Use for Same Product)
##' 
##' This function imputes element 111, which can be thought of as the amount
##' used for seeding.  The rules are as follows:
##' 
##' \itemize{
##'     \item If Ratio 171 (presumably a seeding ratio) is not available for
##'     the current commodity, a similar formula to 101 is used:
##'     E_111 = R_111 * (total supply) / 100.
##'     \item If Ratio 171 is available, then an estimate for total production
##'     is used to multiply by Ratio 171 to compute the amount used for seed.
##'     The preferred estimate for production value is element 21 (potential
##'     producing factor) in the next year; if this is unavailable then element
##'     31 (actual producing factor) in the next year is used; then element 21
##'     in the current year; and lastly element 31 in the current year.  The
##'     value for element 111 is then filled in as this computed seed value
##'     times R_171 divided by 1000.
##' }
##' 
##' @param stotal The column name of data corresponding to the total supply.
##' @param data The AUPUS node dataset, typically as produced by buildNodes.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return This function returns a list (of length 2) of integer vectors
##' containing the row numbers of observations which were updated (the first
##' vector when a value was computed using ratio 111, the second for when
##' ratio 171 was used).  However, it also has a side effect: rows within the
##' passed data.table ("data") have element 111's value and symbol updated.
##' 
##' @export
##' 

calculateEle111 = function(stotal, data, aupusParam){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    
    ## Define columns of interest using param
    defineElementVariables(elements = c(21, 31, 111, 171),
                           aupusParam = aupusParam)
    yearCol = aupusParam$keyNames$yearName
    if(!ratio111Num %in% colnames(data))
        data[, c(ratio111Num) := NA_real_]
        
    ## If R_171 is missing, it's the same as calculateEle101 but with R_111
    replaceIndex1 = data[, is.na(get(ratio171Num)) & !is.na(get(ratio111Num)) &
                             replaceable(get(element111Symb),
                                         get(ratio111Num) * get(stotal))]
    data[replaceIndex1,
         `:=`(c(element111Num, element111Symb),
              list(get(ratio111Num) * get(stotal) / 100, "C"))]

    yearSearch = function(subData){
        n = NROW(subData)
        newValue = as.vector(rep(NA, n), mode = "numeric")
        newSymb = subData[, get(element111Symb)]
        ## Does R_171 have a valid value?
        ## Find element 21 and 31 for current and next year
        ele21t1 = c(subData[, get(element21Num)], NA)[1:nrow(subData) + 1]
        ele31t1 = c(subData[, get(element31Num)], NA)[1:nrow(subData) + 1]
        ele21t0 = subData[, get(element21Num)]
        ele31t0 = subData[, get(element31Num)]
        computed.mat = cbind(ele21t1, ele31t1, ele21t0, ele31t0) *
            subData[, get(ratio171Num)]
        ## Use one of these four elements, in the order of priority 21t1, 31t1,
        ## 21t0, 31t0.  The na.omit(x)[1] function will pull the first
        ## non-missing value and use that.
        proposedValue = apply(computed.mat, 1, FUN = function(x) na.omit(x)[1])
        replaceIndex = which(subData[, !is.na(get(ratio171Num)) &
                                 replaceable(get(element111Symb), newValue)])
        newValue[replaceIndex] = proposedValue[replaceIndex]
        newSymb[replaceIndex] = "C"
        replaced = rep(FALSE, NROW(subData))
        replaced[replaceIndex] = TRUE
        list(newValue, newSymb, replaced)
    }
    data[, c(element111Num, element111Symb, "replaced") :=
             yearSearch(.SD), by = c(key(data)[key(data) != yearCol]),
        ## Only grab the columns of .SD that you need (for efficiency purposes)
         .SDcols = c(element111Symb, ratio171Num, element21Num, element31Num)]
    replaceIndex2 = which(unlist(data[, replaced]))
    data[, replaced := NULL]

    list(which(replaceIndex1), replaceIndex2)
}    
