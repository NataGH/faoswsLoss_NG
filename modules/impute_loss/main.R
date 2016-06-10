suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(igraph)
  library(plyr)
  library(dplyr)
  library(faoswsModules)
})

library(faoswsLoss)

updateModel = TRUE
## server = "QA"
## Set up for the test environment and parameters

if(CheckDebug()){

  settings <- ReadSettings(file = file.path("modules", "impute_loss", "sws.yml"))
  SetClientFiles(dir = settings[["certdir"]])
  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )
  ## test connection
  ## map = faosws::ReadDatatable(table = "fcl_2_cpc")

}

## Year should be a paramameter selected.
selectedYear = as.character(1961:2015)


areaVar = "geographicAreaM49"
areaVarFS = "geographicAreaFS"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
itemVarFS = "measuredItemFS"
elementVar = "measuredElement"
elementVarFS = "measuredElementFS"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"




if(updateModel){

  finalModelData = 
        {
            requiredItems <<- getRequiredItems()
            production <<- getProductionData() # Value_measuredElement_5510
            import <<- getImportData()         # Value_measuredElement_5600
            loss <<- getOfficialLossData()     # Value_measuredElement_5120
            lossFoodGroup <<- getLossFoodGroup()
            ## countryTable <<-
            ##   GetCodeList(domain = "agriculture",
            ##               dataset = "agriculture",
            ##               dimension = "geographicAreaM49")[type == "country",
            ##                                                list(code, description)]
            ## setnames(countryTable,
            ##          old = c("code", "description"),
            ##          new = c("geographicAreaM49", "geographicAreaM49Name"))
        } %>%
        mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
        subset(x = .,
               subset = ((Value_measuredElement_5510 == 0 &
                          Value_measuredElement_5600 > 0) |
                         (Value_measuredElement_5510 > 0 &
                          Value_measuredElement_5600 >= 0)),
               select = c("geographicAreaM49", 
                          "measuredItemCPC", 
                          "timePointYears",
                          "Value_measuredElement_5120", # loss
                          "Value_measuredElement_5510", # production
                          "Value_measuredElement_5600", # import
                          "foodGroupName",
                          "foodPerishableGroup")) %>%
        removeCarryLoss(data = ., lossVar = "Value_measuredElement_5120") %>%
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

  ## modeldata <- file.path("module", "finalModelData.Rdata")
  ## modeldata <- file.path(file.path(drypath, "faofbs", "data", "original", "finalModelData.Rdata"))
  ## save(finalModelData, file = modeldata)
  ## load(modeldata)
  
    lossLmeModel =
        lmer(log(Value_measuredElement_5120 + 1) ~
                 -1 +
                 timePointYears +
                 log(Value_measuredElement_5510 + 1) + 
                 (-1 + log(Value_measuredElement_5510 + 1)|
                  foodPerishableGroup/foodGroupName/measuredItemCPC/geographicAreaM49)+
                 log(Value_measuredElement_5600 + 1) +
                 (-1 + log(Value_measuredElement_5600 + 1)|
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
  loss <<- getSelectedLossData()
} %>%
  mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
  subset(x = .,
         subset = ((Value_measuredElement_5510 == 0 & Value_measuredElement_5600 > 0) |
                     (Value_measuredElement_5510 > 0 & Value_measuredElement_5600 >= 0)),
         select = c("geographicAreaM49", 
                    "measuredItemCPC", 
                    "timePointYears",
                    "Value_measuredElement_5120", # loss
                    ## "flagObservationStatus_measuredElement_5120", # column not found
                    "flagFaostat_measuredElementFS_5120", # use faostat flag
                    "Value_measuredElement_5510", # production
                    "Value_measuredElement_5600", # import
                    "foodGroupName",
                    "foodPerishableGroup")) %>%
  removeCarryLoss(data = ., lossVar = "Value_measuredElement_5120") %>%
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
             lossVar = "Value_measuredElement_5120",
             lossObservationFlagVar =
               ## "flagObservationStatus_measuredElement_5120",
               "flagFaostat_measuredElementFS_5120", # use faostat flag
             lossMethodFlagVar = "flagMethod_measuredElement_5120",
             lossModel = lossLmeModel) %>%
  saveImputedLoss(data = .)
## write.csv(finalPredictData, file.path(file.path(drypath, "faofbs", "data", "original", "imputeLoss_finalPredictData.csv")), row.names = FALSE)

# finalPredictDataSet %>%
#   filter(flagObservationStatus_measuredElement_5120 == " ")

