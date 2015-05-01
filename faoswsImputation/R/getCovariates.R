##' Get Covariates
##' 
##' This function pulls data from the World Bank datasets, such as GDP
##' indicators and other such data.  These kinds of variables can be useful
##' for estimating production, loss, etc. and thus form a good set of covariate
##' variables for imputation models.
##' 
##' @param data The data.table object where imputation is to be performed.
##' This object is used to determine which countries and times should be
##' queried from the World Bank dataset.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
## ##' @param covariates A character vector of the covariates which should be
## ##' pulled.  Acceptable values are currently "gdpPerCapita", "gdpPPP",
## ##' "pavedRoads", "temperature", and "precipitation".
##' 
##' @return
##' 
##' @export
##' 

getCovariates = function(data, imputationParameters){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot("geographicAreaM49" %in% colnames(data))
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
#     stopifnot(covariates %in% c("gdpPerCapita", "gdpPPP", "pavedRoads",
#                                 "temperature", "precipitation"))
#     stopifnot(length(covariates) == 0)
    
#     allCountries =
#         GetCodeList(domain = "WorldBank",
#                     dataset = "wb_ecogrw",
#                     dimension = "geographicAreaM49")[type == "country", code]
    allCountries = as.character(unique(data$geographicAreaM49))
    allYears = as.character(unique(data[[imputationParameters$yearValue]]))
   
    infrastructureKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_infrastructure",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = allCountries),
                           Dimension(name = "wbIndicator",
                                     keys = "IS.ROD.PAVE.ZS"),
                           Dimension(name = "timePointYears",
                                     keys = allYears)
                       )
                   )
    
    ## Create the gdp key by modifying the infrastructure one
    gdpKey = infrastructureKey
    gdpKey@dataset = "wb_ecogrw"
    gdpKey@dimensions[[2]] =  Dimension(name = "wbIndicator",
                                        keys = c("NY.GDP.MKTP.PP.KD",
                                                 "NY.GDP.PCAP.KD"))

    ## Create the climate key by modifying the infrastructure one
    climateKey = infrastructureKey
    stop("Figure out this dataset and codes!")
    climateKey@dataset = "wb_climate"
    climateKey@dimensions[[2]] =  Dimension(name = "wbIndicator",
                                            keys = c("NY.GDP.MKTP.PP.KD",
                                                     "NY.GDP.PCAP.KD"))

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "wbIndicator", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE)
    )

#     base =
#         data.table(geographicAreaM49 = character(),
#                    wbIndicator = character(),
#                    timePointYears = character(),
#                    Value = numeric())

    worldBankData = rbind(
        GetData(gdpKey, pivoting = newPivot),
        GetData(infrastructureKey, pivoting = newPivot)
        GetData(climateKey, pivoting = newPivot)
    )
    
    worldBankData = dcast.data.table(worldBankData,
                         geographicAreaM49 + timePointYears ~ wbIndicator,
                         value.var = "Value")
    setnames(worldBankData,
             old = c("IS.ROD.PAVE.ZS", "NY.GDP.MKTP.PP.KD",
                 "NY.GDP.PCAP.KD"),
             new = c("sharePavedRoad", "gdpPPP", "gdpPerCapita"))
    worldBankData[, timePointYears := as.numeric(timePointYears)]
    setkeyv(worldBankData, cols = c("geographicAreaM49", "timePointYears"))
    worldBankData
}
