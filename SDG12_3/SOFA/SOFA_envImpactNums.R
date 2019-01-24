library(readxl)
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


FWF_Impact_factors <- as.data.table(read_excel("~/faoswsLossa/data-raw/SOFA/FWF Impact factors.xlsx", sheet = "Database"))
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
gfli_basket <- ReadDatatable("gfli_basket")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
Loss_per_stage <- ReadDatatable("sn_vc_est")
Loss_per_stage_envF <- ReadDatatable("snv_environ_factors")


LossPer_SVC <- ReadDatatable("sn_vc_est")
FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]

load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markovadj_Protected.RData")
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markov_1.RData")

Losses <- timeSeriesDataToBeImputed_5126 %>% filter(timePointYears <= 2016)



names(FWF_Impact_factors)
FWF_Impact_factors[, c("# rows" ,"# columns","Commodity name abbreviation" ,
                       "Region * Commodity" ,"Sub-region * Sub-commodity" ,"Region * Commodity * Phase",
                       "Sub-region * Sub-commodity * Phase" ,"Region * Sub-commodity","**END**"  ):= NULL ]

keep <- c("Sub region name" , "Sub region #" ,"FSC Step name","Sub production name","Sub production #","Emission factors\r\n(kg CO2 eq.  / kg food)" ,
          "Carbon footprint\r\n(1000 tons CO2 eq.)","BLUE Water - Impact factors\r\n(m3  / ton food)","GREEN Water - Impact factors\r\n(m3  / ton food)",                            
          "GREY Water - Impact factors\r\n(m3  / ton food)", "Land use - Impact factors\r\n(Ha  / ton food)", "Economic assessment - Impact factors\r\n (USD / kg of food)",
          "fsc_location", "geographicaream49" ,"measureditemcpc"   )

keepNew <- c("Sub_region_name" , "Sub_region_Num" ,"FSC_Step_name","Sub_production_name","Sub_production_Num","emission_kgco2perkgfood" ,
             "carbon_1000tonsco2eq","water_blue_m3_tonfood","water_green_m3_tonfood",                            
             "water_grey_m3_tonfood", "land_ha_tonfood", "econ_USD_kgfood",
             "fsc_location", "geographicaream49" ,"measureditemcpc"   )


setnames(Losses, old = c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") ,
         new =   c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ))

LossQty <- merge(Losses,prod_imports, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
LossQty[,value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510]

LossQty_stage <- merge(LossQty,Loss_per_stage, by = (keys_lower), all.y = TRUE)
LossQty_stage[,value_measuredelement_5016_vcs := value_measuredelement_5016*value]

LossQty_stage2 <- merge(LossQty_stage,Loss_per_stage_envF , by.x = c("geographicaream49","measureditemcpc", "fsc_location"), by.y = c("geographicaream49", "measureditemcpc", "fsc_locations"), all.x = TRUE, all.y = FALSE)
LossQty_stage2[, vcs_emission := value_measuredelement_5016_vcs*emission_kgco2perkgfood*1000]
LossQty_stage2[, vcs_carbon := value_measuredelement_5016_vcs*carbon_1000tonsco2eq]
LossQty_stage2[, vcs_water_blue := value_measuredelement_5016_vcs*water_blue_m3_tonfood]
LossQty_stage2[, vcs_water_green := value_measuredelement_5016_vcs*water_green_m3_tonfood]
LossQty_stage2[, vcs_water_grey := value_measuredelement_5016_vcs*water_grey_m3_tonfood]
LossQty_stage2[, vcs_land := value_measuredelement_5016_vcs*land_ha_tonfood]
LossQty_stage2[, vcs_econ := value_measuredelement_5016_vcs*econ_usd_kgfood*1000]

keep <- c("geographicaream49","measureditemcpc","timepointyears","fsc_location","vcs_emission","vcs_carbon","vcs_water_blue","vcs_water_green","vcs_water_grey","vcs_land","vcs_econ")
LossQty_stage2a <- LossQty_stage2[,keep,with=F]
LossQty_Env <- as.data.table(LossQty_stage2a %>% 
                               group_by(geographicaream49,measureditemcpc,timepointyears) %>%
                               dplyr:: summarise(
                                 Aggvcs_emission    = sum(vcs_emission, na.rm=T),
                                 Aggvcs_carbon      = sum(vcs_carbon, na.rm=T) ,
                                 Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                 Aggvcs_water_green = sum(vcs_water_green, na.rm=T),
                                 Aggvcs_water_grey  = sum(vcs_water_grey, na.rm=T),
                                 Aggvcs_land        = sum(vcs_land, na.rm=T),
                                 Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                               ))
