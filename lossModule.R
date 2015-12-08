suppressMessages({
  #library(bit64)
  #library(curl)
  library(faosws)
  library(faoswsUtil)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(igraph)
  library(plyr)
  library(dplyr)
  #library(RJDBC)
})

library(faoswsLoss)

updateModel = TRUE

## Set up for the test environment and parameters

if(CheckDebug()) {
  if(Sys.info()["user"] == "campbells"){ # Seb's work computer
    SetClientFiles(dir = "~/certificates/production")
    token = "e16a500b-2077-471b-8edf-6d810a85814d" # Nata's token 
  } else if(Sys.info()["user"] == "Golini"){
    # Nata's work computer
    SetClientFiles(dir = "~/R certificate files/Production/")
    token = "e16a500b-2077-471b-8edf-6d810a85814d" # Nata's token 
  } else {
    stop("User not yet implemented!")
  }
  
  
  GetTestEnvironment(
    ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = token
  )  
}

## Year should be a paramameter selected.
selectedYear = as.character(1961:2015)

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"




if(updateModel){
  finalModelData = 
{
  requiredItems <<- getRequiredItems()
  production <<- getProductionData()
  import <<- getImportData()
  loss <<- getOfficialLossData()
  lossFoodGroup <<- getLossFoodGroup()
#   countryTable <<-
#     GetCodeList(domain = "agriculture",
#                 dataset = "agriculture",
#                 dimension = "geographicAreaM49")[type == "country",
#                                                  list(code, description)]
#   setnames(countryTable,
#            old = c("code", "description"),
#            new = c("geographicAreaM49", "geographicAreaM49Name"))
} %>%
  mergeAllLossData(lossData = loss, production, import, lossFoodGroup) %>%
  subset(x = .,
         subset = ((Value_measuredElement_5510 == 0 & Value_measuredElement_5600 > 0) |
                     (Value_measuredElement_5510 > 0 & Value_measuredElement_5600 >= 0)),
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

# par(mfrow=c(1,1))
# qqnorm(residuals(lossLmeModel))
# qqline(residuals(lossLmeModel))
}



finalPredictData = 
{
  if(!updateModel){
#    requiredItems <<- getAllItemCPC()
    production <<- getProductionData()
    import <<- getImportData()
    lossFoodGroup <<- getLossFoodGroup()
#    lossRegionClass <<- getLossRegionClass()
#     countryTable <<-
#       GetCodeList(domain = "agriculture",
#                   dataset = "agriculture",
#                   dimension = "geographicAreaM49")[type == "country",
#                                                    list(code, description)]
#     setnames(countryTable,
#              old = c("code", "description"),
#              new = c("geographicAreaM49", "geographicAreaM49Name"))
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
                    "flagObservationStatus_measuredElement_5120",
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
               "flagObservationStatus_measuredElement_5120",
             lossMethodFlagVar = "flagMethod_measuredElement_5120",
             lossModel = lossLmeModel) %>%
  saveImputedLoss(data = .)

# finalPredictDataSet %>%
#   filter(flagObservationStatus_measuredElement_5120 == " ")

