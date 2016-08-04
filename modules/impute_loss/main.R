suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(igraph)
  library(plyr)
  library(dplyr)
})

library(faoswsLoss)

updateModel = TRUE
## server = "QA"
## Set up for the test environment and parameters

if(CheckDebug()){

  ## ?devtools::install_github
  ## devtools::install_github("SWS-Methodology/faoswsModules", ref = "fbb838f8f7d0d53446af18d96ad7300c5d0ac1c6")
  library(faoswsModules)
  settings <- ReadSettings(file = file.path("modules", "impute_loss", "sws.yml"))
  SetClientFiles(dir = settings[["certdir"]])
  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )
  ## test connection
  ## map = faosws::ReadDatatable(table = "fcl_2_cpc")
  ## sessionKey = swsContext.datasets[[1]]
  ## faoswsModules::CopyKey()
  ## faoswsModules::CopyKey(swsContext.datasets[[1]])
  ## swsContext.computationParams # empty list

}

## Year should be a paramameter selected.
## selectedYear = as.character(1961:2015)
selectedYear = as.character(1991:2015)


areaVar = "geographicAreaM49"
## areaVarFS = "geographicAreaFS"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
## itemVarFS = "measuredItemFS"
elementVar = "measuredElement"
## elementVarFS = "measuredElementFS"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"




if(updateModel){

  finalModelData = 
    {
      ## requiredItems <<- getRequiredItems()
      production <<- getProductionData() # Value_measuredElement_5510
      import <<- getImportData()         # Value_measuredElementTrade_5610
      ## loss <<- getOfficialLossData()     # Value_measuredElement_5016
      loss <<- getLossData(protected = TRUE)     # Value_measuredElement_5016
      ## lossFoodGroup <<- getLossFoodGroup()
      assign("lossFoodGroup", getLossFoodGroup(), envir = .GlobalEnv)
    } %>%
    mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
    ## temp <- mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
    ## returns around 13 000 obs
    subset(x = .,
           ## only use observations where:
           ## a) production is zero and imports are positive
           ## b) production is positive and imports are zero or positive
           ## returns around 700 obs
           subset = ((Value_measuredElement_5510 == 0 &
                      Value_measuredElementTrade_5610 > 0) |
                     (Value_measuredElement_5510 > 0 &
                      Value_measuredElementTrade_5610 >= 0)),
           select = c("geographicAreaM49", 
                      "measuredItemCPC", 
                      "timePointYears",
                      "Value_measuredElement_5016", # loss
                      "Value_measuredElement_5510", # production
                      "Value_measuredElementTrade_5610", # import
                      "foodGroupName",
                      "foodPerishableGroup")) %>%
    ## another filter: returns around 280 obs
    removeCarryLoss(data = ., lossVar = "Value_measuredElement_5016") %>%
    ## Convert variables to factor
    .[, `:=`(c("geographicAreaM49",
               "measuredItemCPC", 
               "foodGroupName", 
               "foodPerishableGroup"),
             lapply(c("geographicAreaM49",
                      "measuredItemCPC", 
                      "foodGroupName", 
                      "foodPerishableGroup"),
                    FUN = function(x) as.factor(.SD[[x]])
                    )
             )
      ]

  ## modeldata <- file.path(file.path(drypath, "faofbs", "data", "derived", "orig_imputeLoss_finalModelData_csv.Rdata"))
  ## imputeLoss_finalModelData_csv_orig <- finalModelData
  ## save(imputeLoss_finalModelData_csv_orig, file = modeldata)
  ## load(modeldata)
  
  lossLmeModel =
    lmer(log(Value_measuredElement_5016 + 1) ~
           -1 +
           timePointYears +
           log(Value_measuredElement_5510 + 1) + 
           (-1 + log(Value_measuredElement_5510 + 1)|
            foodPerishableGroup/foodGroupName/measuredItemCPC/geographicAreaM49)+
           log(Value_measuredElementTrade_5610 + 1) +
           (-1 + log(Value_measuredElementTrade_5610 + 1)|
            measuredItemCPC/geographicAreaM49),
         data = finalModelData)
  
}


## names(finalPredictData)
## names(production)
## names(loss)
finalPredictData = 
  {
    if(!updateModel){
      ## requiredItems <<- getAllItemCPC()
      production <<- getProductionData()
      import <<- getImportData()
      lossFoodGroup <<- getLossFoodGroup()
      ## lossRegionClass <<- getLossRegionClass()
      ##  countryTable <<-
      ##    GetCodeList(domain = "agriculture",
      ##                dataset = "agriculture",
      ##                dimension = "geographicAreaM49")[type == "country",
      ##                                                 list(code, description)]
      ##  setnames(countryTable,
      ##           old = c("code", "description"),
      ##           new = c("geographicAreaM49", "geographicAreaM49Name"))
    }
    ## loss <<- getSelectedLossData()
    loss <<- getLossData(protected = FALSE)
  } %>%
  mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
  subset(x = .,
         subset = ((Value_measuredElement_5510 == 0 & Value_measuredElementTrade_5610 > 0) |
                   (Value_measuredElement_5510 > 0 & Value_measuredElementTrade_5610 >= 0)),
         select = c("geographicAreaM49", 
                    "measuredItemCPC", 
                    "timePointYears",
                    "Value_measuredElement_5016", # loss
                    "flagObservationStatus_measuredElement_5016", # column not found
                    ## "flagFaostat_measuredElementFS_5016", # use faostat flag
                    "flagMethod_measuredElement_5016", # column not found
                    "Value_measuredElement_5510", # production
                    "Value_measuredElementTrade_5610", # import
                    "foodGroupName",
                    "foodPerishableGroup")) %>%
  removeCarryLoss(data = ., lossVar = "Value_measuredElement_5016") %>%
  ## Convert variables to factor
  .[, `:=`(c("geographicAreaM49",
             "measuredItemCPC", 
             "foodGroupName", 
             "foodPerishableGroup"),
           lapply(c("geographicAreaM49",
                    "measuredItemCPC", 
                    "foodGroupName", 
                    "foodPerishableGroup"),
                  FUN = function(x) as.factor(.SD[[x]])
                  )
           )
    ]



## Impute selected data

finalPredictData %>%
  imputeLoss(data = .,
             lossVar = "Value_measuredElement_5016",
             lossObservationFlagVar =
               "flagObservationStatus_measuredElement_5016",
               ## "flagFaostat_measuredElementFS_5016", # use faostat flag
             lossMethodFlagVar = "flagMethod_measuredElement_5016",
             lossModel = lossLmeModel) %>%
  saveImputedLoss(data = .)
## ## write.csv(finalPredictData, file.path(file.path(drypath, "faofbs", "data", "original", "imputeLoss_finalPredictData.csv")), row.names = FALSE)
## predictdata <- file.path(file.path(drypath, "faofbs", "data", "derived", "orig_imputeLoss_finalPredictData_csv.Rdata"))
## imputeLoss_finalPredictData_csv_orig <- finalPredictData
## save(imputeLoss_finalPredictData_csv_orig, file = predictdata)

print("Loss Module completed successfully")
