##' Calculate AUPUS Elements
##' 
##' This is the wrapper function that performs the full AUPUS module.  It
##' computes each individual element, then proceeds to compute utilization and
##' balances.  This function should be called several times to process all the
##' processing levels of the items.
##'
##' @param aupusFinalData The data.table object containing the nodes data,
##' typically as created by the suaToNetworkRepresentation function.
##' @param itemTypeCol The column name of aupusFinalData corresponding to the
##' item type of the commodity item.
##' @param balanceElementNum The column name of aupusFinalData corresponding to
##' the balance element column.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##'
##' @return No value is returned.  Instead, the passed data.table
##' (aupusFinalData) has each of it's elements computed and is balanced.
##'
##' @export
##' 

calculateAupusElements = function(aupusFinalData, itemTypeCol,
                                  balanceElementNum, aupusParam){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(is(aupusFinalData, "data.table"))
    stopifnot(itemTypeCol %in% colnames(aupusFinalData))
    stopifnot(balanceElementNum %in% colnames(aupusFinalData))
    
    ## Element 11 - check the symbol, the value seems correct
    replaced11Index = calculateEle11(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced11Index, 11)

    ## Element 21
    replaced21Index = calculateEle21(data = aupusFinalData,
                                     aupusParam = aupusParam)
    ## compareFunction(replaced21Index[[1]], 21)
    ## compareFunction(replaced21Index[[2]], 21)

    ## Denormalize population
    ## denormalizePopulation(element11Num = "Value_measuredElementFS_11",
    ##                       element21Num = "Value_measuredElementFS_21",
    ##                       data = aupusFinalData)

    ## Element 31
    ## replaced31Index =
    ##     calculateEle31(element31Num = "Value_measuredElementFS_31",
    ##                    element31Symb = "flagFaostat_measuredElementFS_31",
    ##                    inputNum = inputNum,
    ##                    itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                    data = aupusFinalData)
    ## compareFunction(replaced31Index[[1]], 31)
    ## compareFunction(replaced31Index[[2]], 31)

    ## Element 41
    replaced41Index = calculateEle41(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced41Index, 41)

    ## Element 51
    replaced51Index = calculateEle51(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced51Index, 51)

    ## Element 31, 41, 51 balance
    replaced314151 = calculateEle314151(aupusParam = aupusParam, data = aupusFinalData)
    
    ## Element 58
    ## calculateEle58(element58Num = "Value_measuredElementFS_58", element58Symb = "flagFaostat_measuredElementFS_58",
    ##                itemTypeCol = aupusParam$keyNames$itemTypeName, data = aupusFinalData)

    ## Element 61, 62, 63
    replaced63Index = calculateEle63(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced63Index[[1]], 63)

    ## Element 66
    ## replaced66Index =
    ##     calculateEle66(element41Num = "Value_measuredElementFS_41",
    ##                    element61Num = "Value_measuredElementFS_61",
    ##                    element66Num = "Value_measuredElementFS_66",
    ##                    element66Symb = "flagFaostat_measuredElementFS_66",
    ##                    shares = shareNum, itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                    data = aupusFinalData, shareData = shareData)
    ## compareFunction(replaced66Index, 66)

    ## Element71
    replaced71Index =
        calculateEle71(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced71Index[[1]], 71)
    ## compareFunction(replaced71Index[[2]], 71)

    ## Element 91, 92, 93
    replaced93Index = calculateEle93(data = aupusFinalData, aupusParam = aupusParam)
    ## compareFunction(replaced93Index[[1]], 93)
    ## compareFunction(replaced93Index[[2]], 93)


    ## Element 96
    ## calculateEle96(element41Num = "Value_measuredElementFS_41",
    ##                element91Num = "Value_measuredElementFS_91",
    ##                element96Num = "Value_measuredElementFS_96",
    ##                element96Symb = "flagFaostat_measuredElementFS_96",
    ##                shares = shareNum, itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                data = aupusFinalData, shareData = shareData)


    ## Calculate total supply
    calculateTotalSupply(data = aupusFinalData, aupusParam = aupusParam)

    ## Elemet 101
    replaced101Index =
        calculateEle101(stotal = "TOTAL_SUPPLY", data = aupusFinalData,
                        aupusParam = aupusParam)
    ## compareFunction(replaced101Index, 101)

    ## Element 111
    replaced111Index = calculateEle111(stotal = "TOTAL_SUPPLY",
                                       data = aupusFinalData,
                                       aupusParam = aupusParam)
    ## compareFunction(replaced111Index, 111)

    ## Element 121
    calculateEle121(stotal = "TOTAL_SUPPLY", data = aupusFinalData,
                    aupusParam = aupusParam)
    ## compareFunction(replaced121Index, 121)

    ## Element 131
    calculateEle131(stotal = "TOTAL_SUPPLY", data = aupusFinalData,
                    aupusParam = aupusParam)
    ## compareFunction(replaced131Index, 131)

    ## Element 141
    calculateEle141(stotal = "TOTAL_SUPPLY", data = aupusFinalData,
                    aupusParam = aupusParam)

    ## compareFunction(replaced141Index[[1]], 141)
    ## compareFunction(replaced141Index[[2]], 141)

    ## Element 144
    calculateEle144(population11Num = "Value_population_11",
                    data = aupusFinalData, aupusParam = aupusParam)

    ## compareFunction(replaced144Index[[1]], 144)
    ## compareFunction(replaced144Index[[2]], 144)

    ## Element 151
    replaced151Index =
        calculateEle151(stotal = "TOTAL_SUPPLY", data = aupusFinalData,
                        aupusParam = aupusParam)

    ## compareFunction(replaced151Index, 151)

    ## Element 161
    replaced161Index = calculateEle161(data = aupusFinalData,
                                       aupusParam = aupusParam)

    ## compareFunction(replaced161Index, 161)

    ## Element 171
    replaced171Index = calculateEle171(data = aupusFinalData,
                                       aupusParam = aupusParam)
    ## compareFunction(replaced171Index, 171)

    ## Element 174
    replaced174Index = calculateEle174(population11Num = "Value_population_11",
                                       data = aupusFinalData,
                                       aupusParam = aupusParam)
    ## compareFunction(replaced174Index, 174)

    ## Element 261
    replaced261Index =
        calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_261",
                                elementNum = 261, data = aupusFinalData,
                                aupusParam = aupusParam)
    ## compareFunction(replaced261Index, 261)

    ## Element 264
    calculateDailyNutritive(population11Num = "Value_population_11",
                            population21Num = "Value_population_21",
                            dailyElement = 264, totalElement = 261,
                            data = aupusFinalData, aupusParam = aupusParam)

    ## Element 271
    replaced271Index =
        calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_271",
                                elementNum = 271, data = aupusFinalData,
                                aupusParam = aupusParam)

    ## Element 274
    calculateDailyNutritive(population11Num = "Value_population_11",
                            population21Num = "Value_population_21",
                            dailyElement = 274, totalElement = 271,
                            data = aupusFinalData, aupusParam = aupusParam)

    ## Element 281
    replaced281Index =
        calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_281",
                                elementNum = 281, data = aupusFinalData,
                                aupusParam = aupusParam)

    ## Element 284
    calculateDailyNutritive(population11Num = "Value_population_11",
                            population21Num = "Value_population_21",
                            dailyElement = 284, totalElement = 281,
                            data = aupusFinalData, aupusParam = aupusParam)


    ## Element 541
    replaced541Index = calculateEle541(data = aupusFinalData,
                                       aupusParam = aupusParam)

    ## compareFunction(replaced541Index[[1]], 541)
    ## compareFunction(replaced541Index[[2]], 541)

    ## Element 546
    replaced546Index = calculateEle546(data = aupusFinalData,
                                       aupusParam = aupusParam)

    ## compareFunction(replaced546Index[[1]], 546)
    ## compareFunction(replaced546Index[[2]], 546)

    ## Total utilization
    calculateTotalUtilization(data = aupusFinalData,
                              aupusParam = aupusParam)
    ## compareFunction(1, "TOTAL_UTILIZATION")
    
    ## Balance
    replacedBalance = calculateBalance(supply = "TOTAL_SUPPLY",
                                       utilization = "TOTAL_UTILIZATION",
                                       data = aupusFinalData,
                                       aupusParam = aupusParam)
    ## compareFunction(1, "BALANCE")
    
    return()
}
