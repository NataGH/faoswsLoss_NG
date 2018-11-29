library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(openxlsx)
library(tidyr)
library(magrittr) 

library(faosws)
library(faoswsUtil)
library(faoswsLoss)

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(data.table)
  library(magrittr)
  library(plyr)
  library(dplyr)
  
})

############# Computation Parameters #####################################
savesws <- TRUE
LocalRun <- TRUE # For if you are running the model on a local environment and loading data tables from local fiiles

if (!exists('selectedYear_start', inherits = FALSE)| !exists('selectedYear_end', inherits = FALSE)) {
  ## Year should be a paramameter selected.
  selectedYear_start <- swsContext.computationParams$selectedyear_start
  selectedYear_end <- swsContext.computationParams$selectedyear_end
  selectedYear = as.character(as.numeric(selectedYear_start):as.numeric(selectedYear_end))
}
if (!exists('aggregation', inherits = FALSE)) {
  # Options are: #  "iso2code","isocode","countryname","sdgregion_code","sdg_regions"           
  # "m49_level1_code","m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions"            
  # "ldcs_code","ldcs","lldcssids_code","lldcssids","Country","geographicaream49", "WORLD"   
  aggregation <- swsContext.computationParams$aggregation
}
if (!exists('weights', inherits = FALSE)) {
  # Options are: #"intl_prices"    
  weights <- swsContext.computationParams$weights
}
if (!exists('basketn', inherits = FALSE)) {
  # Options are: "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"  
  basketn<- swsContext.computationParams$basketn
}
if (!exists('ComparisonYear_start')| !exists('ComparisonYear_end')) {
  ## Year should be a paramameter selected.
  ComparisonYear_start <- swsContext.computationParams$ComparisonYear_start
  ComparisonYear_end <- swsContext.computationParams$ComparisonYear_end
  ComparisonYear = as.character(as.numeric(ComparisonYear_start):as.numeric(ComparisonYear_end))
  gfli_compare <- TRUE
}
if (!exists('ReportingYear', inherits = FALSE)) {
  # Options are: "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"  
  ReportingYear<- swsContext.computationParams$ReportingYear
  gfli_Reporting <- TRUE
}
if(LocalRun){
  selectedYear <- as.character(1991:2016)
  ReportingYear<-  as.character(c(2015))
  aggregation <-  "geographicaream49" 
  weights <- "intl_prices" 
  basketn <- "top2perhead_byCtry"
  ComparisonYear <- (c(2005,2016))
  gfli_Reporting <- TRUE
  gfli_compare <- TRUE
}

#####################

BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)


##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
if(CheckDebug()){
  message("Not on server, so setting up environment...")
  USER <- if_else(.Platform$OS.type == "unix",
                  Sys.getenv('USER'),
                  Sys.getenv('USERNAME'))
  
  
  library(faoswsModules)
  settings <- ReadSettings(file = file.path(paste(getwd(),"sws.yml", sep='/')))
  SetClientFiles(settings[["certdir"]])
  
  
  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )
  
  
  
}else if(CheckDebug() & LocalRun){
  #Load local last dataset
  load("InputData.RData")
  
}else{
  # Remove domain from username
  USER <- regmatches(
    swsContext.username,
    regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
  )
  
  options(error = function(){
    dump.frames()
    
    filename <- file.path(Sys.getenv("R_SWS_SHARE_PATH"), USER, "PPR")
    
    dir.create(filename, showWarnings = FALSE, recursive = TRUE)
    
    save(last.dump, file = file.path(filename, "last.dump.RData"))
  })
} 

#### Data In #####
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)

library(readxl)
excel_sheets("~/faoswsLossa/SDG12_3/UsefulCodeNotfortheRPackage/loss_compare_fin.xlsx")
loss_compare_fin <- as.data.table(read_excel("~/faoswsLossa/SDG12_3/UsefulCodeNotfortheRPackage/loss_compare_fin.xlsx",sheet ="Changed manually or with routin"))
loss_compare_fin$geographicAreaM49 <- as.character(loss_compare_fin$geographicAreaM49)
Losses$geographicAreaM49 <- as.character(Losses$geographicAreaM49)
Losses$timePointYears <- as.numeric(Losses$timePointYears)
loss_compare_fin $timePointYears <- as.numeric(loss_compare_fin $timePointYears)
setnames(  loss_compare_fin, old =c("geographicAreaM49","measuredItemFbsSua" ,"timePointYears"), new=c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"))


rr <-merge(loss_compare_fin,Losses, by=c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = TRUE,all.y = FALSE)

cols <- c("NewModel_OldMod", "ratio loss module/production","ratio_loss FBS/production")
rr[,(cols) := round(.SD,3), .SDcols=cols]

rr$NewModel_OldMod <- rr$`ratio loss module/production`-rr$Value
rr$NewModel_diffFBS <- rr$`ratio_loss FBS/production`-rr$Value

write.xlsx(rr, "~/faoswsLossa/SDG12_3/UsefulCodeNotfortheRPackage/loss_compare_fin_update.xlsx")


