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
  library(dtplyr)
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

loss <- getLossData() 
loss
head(loss)

loss[, .N, timePointYears]
loss[, .N, geographicAreaM49]
loss[, .N, measuredItemCPC]

flagValidTable

setnames(loss, "flagObservationStatus_measuredElement_5016", "flagObservationStatus")
setnames(loss, "flagMethod_measuredElement_5016", "flagMethod")

lossData <- merge(loss, flagValidTable, 
              by = c("flagObservationStatus", "flagMethod"), 
              all.x = T)

lossData <- nameData("agriculture", "aproduction", lossData)

tabFlags <- lossData[, .N, c("flagObservationStatus", "flagMethod", "Valid", "Protected")]
tabFlags[, Percent := 100*round(N/sum(N), 3)]
setnames(tabFlags, "N", "NumbRows")
tabFlags

# m49
tabFlagsM49 <- lossData[, .N, c("geographicAreaM49_description", "flagObservationStatus", "flagMethod", "Valid", "Protected")]

tabFlagsM49[, Percent := 100*round(N/sum(N), 3), 
            by = list(geographicAreaM49_description)]

setnames(tabFlagsM49, "N", "NumbRows")
setkey(tabFlagsM49, geographicAreaM49_description)


# time
tabFlagsYear <- lossData[, .N, c("timePointYears", "flagObservationStatus", "flagMethod", "Valid", "Protected")]

tabFlagsYear[, Percent := 100*round(N/sum(N), 3), 
            by = list(timePointYears)]

setnames(tabFlagsYear, "N", "NumbRows")
setkey(tabFlagsYear, timePointYears)
tabFlagsYear

# library(xlsx)
?write.xlsx
## , "Flags by Country", "Flags by Year"
getwd()
write.xlsx(tabFlags, "data/Loss - Summary Table.xlsx", 
           sheetName = c("Flags"), row.names = F)

write.xlsx(tabFlagsM49, "data/Loss - Summary Table.xlsx", 
           sheetName = c("Flags by Country"), append=TRUE, row.names = F)

write.xlsx(tabFlagsYear, "data/Loss - Summary Table.xlsx", 
           sheetName = c("Flags by Year"), append=TRUE, row.names = F)


