#' Part of the FAO Loss Module
#' 
#' @author Alicia English 
#' 
#' 
#' 
# ---
#   runtime: shiny
# output: html_document
# Color palates #https://www.quackit.com/css/css_color_codes.cfm
# ---


library(faoswsLoss)
library(shiny)
library(shinythemes)
library(rmarkdown)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)
library(plotly)
library(yaml)
library(rdrop2)


suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(plyr)
  library(dplyr)
  
})


############# Computation Parameters #####################################
savesws <- TRUE
LocalRun <- TRUE # For if you are running the model on a local environment and loading data tables from local fiiles

if(CheckDebug()){
  maxYear <- format(Sys.Date(), "%Y")
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

BaseYear = as.character(c(2014,2016)) ## This is not an option to choose after the movement to the SDG base yr
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

production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
nutrient_table <- getNutritionData(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE)
ConvFactor1  <- ReadDatatable('flw_lossperfactors_')
#fbsTree <- ReadDatatable("fbs_tree")
gfli_basket <- ReadDatatable("gfli_basket")
M_aggregates <- ReadDatatable('aggregate_loss_table')

CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
intPrice <-  ReadDatatable("int_dollar_prices_all") #int_$_prices_2005

names(production) <- tolower(names(production))
names(imports) <- tolower(names(imports))
names(nutrient_table) <- tolower(names(nutrient_table))
names(intPrice)[names(intPrice) == "measureditemcpc_description"] <- "crop"

production$geographicaream49 <- as.character(production$geographicaream49)
nutrient_table$geographicaream49 <- as.character(nutrient_table$geographicaream49)
production$timepointyears <- as.numeric(production$timepointyears)
imports$timepointyears<- as.numeric(imports$timepointyears)

prod_imports <- merge(production,imports, by= keys_lower, all.x = TRUE)
prod_imports[,prod_imports := rowSums(.SD, na.rm = TRUE), .SDcols=c("value.x","value.y")]

CountryGroup$geographicaream49 <- CountryGroup$m49_code
CountryGroup$country <- CountryGroup$m49_region

opt <- as.data.table(cbind( c("m49_code","iso2code","isocode","m49_region","sdgregion_code","sdg_regions","m49_level1_code",
                              "m49_level1_region","m49_level2_code","m49_level2_region","mdgregions_code","mdgregions_region","ldcs_code","ldcs_region",
                              "lldcssids_code","lldcssids_region","fao_region","fao_operational_agg", "worldbank_income2018_agg", "sofa_agg"),
                            c("m49_code","ISO2","ISO3","Country","sdgregion_code","SDG Regions","m49_level1_code",
                              "Geographic Regions(m49) Level1","m49_level2_region_code","Geographic Regions(m49) Level2","mdgregions_code","MDG Regions","ldcs_code","Least Developed Countries (LDC)",
                              "lldcssids_code","Land Locked Developing Countries (LLDC)","FAO Operational Region","FAO Operational Coverage", "World Bank Income Groups", "SOFA Aggregations")))
names(opt) <- c("code", "Aggregates")
opt2 <- c("m49_code","sdgregion_code","m49_level1_code",
          "m49_level2_code","mdgregions_code","ldcs_code",
          "lldcssids_code","fao_operational_agg", "worldbank_income2018_agg","gfli_basket", "sofa_agg","sofa_wu_agg", "basket_sofa_wu")


FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]


gfli_basket[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]


ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- Base_Prod[timepointyears == as.numeric(BaseYear[2])-1,]
Base_Prod <- Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F]

f <- list(
  family = "Times New Roman",
  size = 12,
  face="bold",
  color = "#7f7f7f"
)


InputData_Out <- ConvFactor1
InputData_Out$fsc_location1 = sapply(strsplit(InputData_Out$fsc_location,"/"), '[', 1)
InputData_Out <- merge(InputData_Out,gfli_basket[,c("gfli_basket", "measureditemcpc"),with=F], by= c("measureditemcpc"), all.x = T)
InputData_Out <- merge(InputData_Out,CountryGroup, by= c("geographicaream49","country"), all.x = T)
dataStages <- unique(InputData_Out$fsc_location1)
datatags <- unique(InputData_Out$tag_datacollection)


#Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan3.RData")
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan-FFAll.RData") ## all data sources
#oad("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan-s2lb.RData") # smoothed data
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markovadj_noProtected.RData")
load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markovadj_Protected.RData")
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markov_1.RData")

Losses <- timeSeriesDataToBeImputed_5126 %>% filter(timePointYears <= 2016)
names(Losses) <- tolower(names(Losses))

Losses$geographicaream49 <- as.character(Losses$geographicaream49)
Losses[ ,flagcombo := paste(flagobservationstatus,flagmethod, sep=";")]

names(Losses)[names(Losses) =="value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measureditemsuafbs"] <- "measureditemcpc"
#intPrice <- merge(intPrice, FAOCrops[,c("measureditemcpc","crop"),with=F], by= c("measureditemcpc"))

# ProdQtySWS <- subset(prod_imports,
#                      select = c(keys_lower,"prod_imports")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])


Losses_Out <-  merge(Losses,unique(FAOCrops[,c("crop", "measureditemcpc"),with=F]), by= c("measureditemcpc"), all.x = T)
Losses_Out <- merge(Losses_Out,gfli_basket[,c("gfli_basket","basket_sofa_wu", "measureditemcpc"),with=F], by= c("measureditemcpc"), all.x = T)
Losses_Out <- merge(Losses_Out,CountryGroup, by= c("geographicaream49"), all.x = T)
Losses_Out <- Losses_Out %>% filter(timepointyears <2016)

Losses_Out <-Losses_Out[!value_measuredelement_5126==1,]

# ## Remove working system packagesto upload to shinyapps ###########
#if(shinyapps){
  rm( swsContext.computationParams,swsContext.datasets,swsContext.executionId,swsContext.token,swsContext.userEmail,
      swsContext.userId,swsContext.username,swsContext.baseRestUrl,settings,USER)
  #rm (token)

  detach("package:faoswLosss", unload=TRUE)
  detach("package:faosws", unload=TRUE)
  detach("package:faoswsUtil", unload=TRUE)
  detach("package:faoswsModules", unload=TRUE)
# }
####
save.image("~/faoswsLossa/shiny/Soup2Nuts/data_InternalFAO_jan.RData")
drop_upload("~/faoswsLossa/shiny/Soup2Nuts/data_InternalFAO_jan.RData", path ="shiny")


# if(shinyappsA){  
# #   ###
    load(file.path(paste(getwd(),"shiny", "Soup2Nuts", "data_InternalFAO.RData", sep='/')))
    settings <- yaml.load_file(file.path(paste(getwd(),"shiny", "Shiny.yml", sep='/')))
    rsconnect::setAccountInfo(name = settings$shinyio$name, token= settings$shinyio$token, secret=settings$shinyio$secret)
   rsconnect::deployApp(file.path(paste(getwd(),"shiny","Soup2Nuts", sep='/')))
# #   ## Remove working system packagesto upload to shinyapps ###########
# # }
# 
# 
