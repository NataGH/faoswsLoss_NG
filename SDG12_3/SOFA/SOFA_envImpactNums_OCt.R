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
Loss_per_stage <-   ReadDatatable("sn_vc_est")
Loss_per_stage_envF <- ReadDatatable("snv_environ_factors")
Baskets <- ReadDatatable("sdg123_commoditybasket")
CountryGroup$geographicaream49 <- CountryGroup$m49_code
FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]

Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
LossesQty <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5016')
#Loss_per_stage$value_measuredelement_5126 <- Loss_per_stage$loss_per_clean

setnames(Losses, old = c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") ,
         new =   c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ))
setnames(LossesQty, old = c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") ,
         new =   c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ))


##################### Double checking all country/commodity/year are in the stage estimates ###
Losses[,combo:= paste(geographicaream49, timepointyears,measureditemcpc, sep=";" )]
LossesQty[,combo:= paste(geographicaream49, timepointyears,measureditemcpc, sep=";" )]
Loss_per_stage[,combo:= paste(geographicaream49, timepointyears,measureditemcpc, sep=";" )]
Loss_per_stage[,combo2:= paste(geographicaream49, measureditemcpc, sep=";" )]

Losses <- Losses %>% filter(timepointyears== 2015)
LossesQty <- LossesQty %>% filter(timepointyears== 2015)
Loss_per_stage <- Loss_per_stage %>% filter(timepointyears== 2015)

#prod_imports$geographicaream49 <- as.character(prod_imports$geographicaream49)
gfli_basket[gfli_basket == "Cereals" | gfli_basket == "Pulses", gfli_basket := "Cereals & Pulses"]
# gfli_basket[foodgroupname == 2918, gfli_basket := "Vegetables"]
# gfli_basket[foodgroupname == 2919, gfli_basket := "Fruits"]
# Multiplies loss percentages by production


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




Losses$geographicaream49 <- as.character(Losses$geographicaream49)

LossesQty$geographicaream49 <- as.character(LossesQty$geographicaream49)


#LossQty <- merge(Losses,prod_imports, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)

#LossQty[,value_measuredelement_5016 := value_measuredelement_5126*value.x] #_measuredelement_5510]
Loss_per_stage <- merge(Loss_per_stage,gfli_basket[,c("measureditemcpc", "gfli_basket")], by = c("measureditemcpc"),all.x=T)
Loss_per_stage <- merge(Loss_per_stage,CountryGroup[, c("geographicaream49","sdg_regions")], by = c("geographicaream49"),all.x=T)

LossQty_stage1 <- merge(LossesQty,Loss_per_stage, by = c(keys_lower), all.x = TRUE)
LossQty_stage1$value <- LossQty_stage1$value_measuredelement_5126
LossQty_stage1$fsc_location <- LossQty_stage1$fsc_location1
#LossQty_stage1[,value_measuredelement_5016_vcs := value_measuredelement_5016*value_measuredelement_5126]
LossQty_stage1[,value_measuredelement_5016_vcs := value_measuredelement_5016*value]


# #### Not all countries had subnational data So the stages were not estimated.
# 
# missingStages <- LossQty_stage1[is.na(value_measuredelement_5016_vcs),]
# LossQty_stage1 <- LossQty_stage1[!is.na(value_measuredelement_5016_vcs),]
# 
# 
# missingStagesGrid  <- as.data.table(expand.grid(timepointyears = 2015,
#                           geographicaream49 = as.character(unique(missingStages$geographicaream49)),
#                           measureditemcpc = as.character(unique(missingStages$measureditemcpc)),
#                           fsc_location = sort(unique(LossQty_stage1$fsc_location))))
# missingStagesGrid[,combo:= paste(geographicaream49, timepointyears,measureditemcpc, sep=";" )]
# missingStagesGrid  <- missingStagesGrid[combo %in% missingStages$combo.x]
# 
# missingStagesGrid[,value := 0]
# missingStagesGrid <- merge(missingStagesGrid,gfli_basket[,c("measureditemcpc", "gfli_basket")], by = c("measureditemcpc"),all.x=T)
# missingStagesGrid <- merge(missingStagesGrid,CountryGroup[, c("geographicaream49","sdg_regions")], by = c("geographicaream49"),all.x=T)
# 
# 
# Loss_per_stage_Av <- as.data.table(Loss_per_stage %>%
#   group_by( sdg_regions,gfli_basket,fsc_location) %>%
#   dplyr:: summarise(GAverage = mean(value, na.rm=T)))
# 
# missingStagesGrid <- merge(missingStagesGrid,Loss_per_stage_Av , by = c("gfli_basket","sdg_regions", "fsc_location"), all.x = TRUE)
# missingStagesGrid[, value :=  GAverage]
# missingStagesGrid[, GAverage := NULL]
# missingStagesGrid[is.na(value),]
# 
# 
# 
# Loss_per_stage_Av_byCmdyGrp <- as.data.table(Loss_per_stage  %>%
#                                      group_by(gfli_basket ,timepointyears ,fsc_location) %>%
#                                      dplyr:: summarise(GAverage = mean(value, na.rm=T)))
# 
# missingStagesGrid <-merge(missingStagesGrid,gfli_basket[,c("measureditemcpc", "gfli_basket")], by = c("measureditemcpc","gfli_basket"),all.x=T)
# missingStagesGrid <- merge(missingStagesGrid,Loss_per_stage_Av_byCmdyGrp, by = c("gfli_basket", "timepointyears", "fsc_location"), all.x = TRUE)
# missingStagesGrid[, value :=  GAverage]
# missingStagesGrid[, GAverage := NULL]
# unique(missingStagesGrid[is.na(value),"measureditemcpc"])
# missingStagesGrid <- missingStagesGrid[!is.na(value),]
# missingStagesGrid[, c("gfli_basket","sdg_regions") := NULL]
# 
# LossQty_stage1[, c("combo.x","combo.y","combo2","sdg_regions", "gfli_basket"):= NULL]
# 
# missingStagesGrid[,value_measuredelement_5016_vcs := 0]
# 
# LossQty_stage1 <- rbind(LossQty_stage1,missingStagesGrid ,fill=T)

########################## Proper multiplication with different denominators!! ############
LossQty_stage1[,value_measuredelement_5016_vcs_p := (1- value)]
loc2 <- c("farm","transport","storage", "trader","wholesale", "processing", "retail")


ctry = "108"
comd = "0111"
sum(LossQty_stage1[(geographicaream49 == ctry)  &  (measureditemcpc == comd),"value_measuredelement_5016_vcs"])
LossQty_stage1[(geographicaream49 == ctry)  &  (measureditemcpc == comd),]

stage <- as.data.table(LossQty_stage1  %>% 
                         group_by(geographicaream49 ,measureditemcpc) %>%
                         dplyr:: summarise(value_measuredelement_5016_vcs_pe = (1-prod(value_measuredelement_5016_vcs_p, na.rm=T))))

stage <-unique(stage)
LossQty_stage1 <- join(LossQty_stage1,stage, by = c("geographicaream49", "measureditemcpc"))
LossQty_stage1$value_measuredelement_5016_vcs <- 0
LossQty_stage1[,value_measuredelement_5016_vcs2 := value_measuredelement_5016/value_measuredelement_5016_vcs_pe]

for( n in 1:length(loc2)){
  stage <- as.data.table(LossQty_stage1  %>% 
                           filter(fsc_location %in%  loc2[n])%>%
                           group_by(geographicaream49 ,measureditemcpc) %>%
                           dplyr:: summarise(value_measuredelement_5016_vcs_pe2 = value_measuredelement_5016_vcs2*value))
  
  stage <-unique(stage)
  stage[,fsc_location:= loc2[n]]
  LossQty_stage1 <- join(LossQty_stage1,stage, by = c("geographicaream49", "measureditemcpc","fsc_location"),type = "left")
  LossQty_stage1[!is.na(value_measuredelement_5016_vcs_pe2), value_measuredelement_5016_vcs := value_measuredelement_5016_vcs_pe2]
  LossQty_stage1[,value_measuredelement_5016_vcs_pe2 := NULL]
  stage[,fsc_location:= NULL]
  LossQty_stage1 <- join(LossQty_stage1,stage, by = c("geographicaream49", "measureditemcpc"),type = "left")
  LossQty_stage1[!is.na(value_measuredelement_5016_vcs_pe2), value_measuredelement_5016_vcs2 := value_measuredelement_5016_vcs2- value_measuredelement_5016_vcs_pe2]
  LossQty_stage1[,value_measuredelement_5016_vcs_pe2 := NULL]
  
   
}

## Converge retail transport and traders into distribution
LossQty_stage1[fsc_location %in% c("transport","trader"),fsc_location := "wholesale"]

Loss_per_stage_envFR <-Loss_per_stage_envF[fsc_locations == "wholesale",]
Loss_per_stage_envFR$fsc_locations <- "retail"
Loss_per_stage_envF <- rbind(Loss_per_stage_envF,Loss_per_stage_envFR )

LossQty_stage2 <- merge(LossQty_stage1,Loss_per_stage_envF , by.x = c("geographicaream49","measureditemcpc", "fsc_location"), by.y = c("geographicaream49", "measureditemcpc", "fsc_locations"), all.x = TRUE, all.y = FALSE)
LossQty_stage2[, vcs_emission := value_measuredelement_5016_vcs*emission_kgco2perkgfood]
#LossQty_stage2[, vcs_carbon := value_measuredelement_5016_vcs*carbon_1000tonsco2eq]
LossQty_stage2[, vcs_water_blue := value_measuredelement_5016_vcs*water_blue_m3_tonfood]
LossQty_stage2[, vcs_water_green := value_measuredelement_5016_vcs*water_green_m3_tonfood]
LossQty_stage2[, vcs_water_grey := value_measuredelement_5016_vcs*water_grey_m3_tonfood]
LossQty_stage2[, vcs_land := value_measuredelement_5016_vcs*land_ha_tonfood]
LossQty_stage2[, vcs_econ := value_measuredelement_5016_vcs*econ_usd_kgfood*1000]
Baskets$ctrycomd <- NA

Baskets[,"ctrycomd" := paste(geographicaream49,measureditemcpc, sep=";")]
LossQty_stage2[,ctrycomd := paste(geographicaream49,measureditemcpc, sep=";")]

LossQty_stage2 <- LossQty_stage2[ctrycomd %in% Baskets$ctrycomd ,]

keep <- c("geographicaream49","measureditemcpc","timepointyears","fsc_location","value_measuredelement_5016_vcs", "vcs_emission","vcs_water_blue","vcs_water_green","vcs_water_grey","vcs_land","vcs_econ", "emission_kgco2perkgfood", "water_blue_m3_tonfood","land_ha_tonfood" )
LossQty_stage2a <- LossQty_stage2[,keep,with=F]
LossQty_stage2a <-merge(LossQty_stage2a,gfli_basket[,c("measureditemcpc", "gfli_basket","basket_sofa_wu"), with= F], by = "measureditemcpc", all.x=T)
LossQty_stage2a <-merge(LossQty_stage2a,CountryGroup [,c("geographicaream49", "sdg_regions", "worldbank_income2018_agg","sofa_agg"), with= F], by = "geographicaream49", all.x=T)
LossQty_stage2a <- LossQty_stage2a[!is.na(sdg_regions),]


LossQty_stage1  %>% 
  filter((timepointyears== 2015) & (fsc_location%in% c("retail")))%>%
  group_by(fsc_location) %>%
  dplyr:: summarise(avePer = mean( value_measuredelement_5016_vcs))

LossQty_Env_stage_2015 <- as.data.table(LossQty_stage2a %>% 
                               filter(timepointyears== 2015)%>%
                               group_by(fsc_location) %>%
                               dplyr:: summarise(
                                 Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                 Aggvcs_emission    = sum(vcs_emission, na.rm=T),
                                # Aggvcs_carbon      = sum(vcs_carbon, na.rm=T) ,
                                 Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                 Aggvcs_water_green = sum(vcs_water_green, na.rm=T),
                                 Aggvcs_water_grey  = sum(vcs_water_grey, na.rm=T),
                                 Aggvcs_land        = sum(vcs_land, na.rm=T),
                                 Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                               ))
write.csv(LossQty_Env_stage_2015, "LossQty_Env_stage_2015_19June19.csv")
LossQty_Env_comodgrp_2015 <- as.data.table(LossQty_stage2a %>% 
                                          filter(timepointyears== 2015)%>%
                                          group_by(gfli_basket) %>%
                                          dplyr:: summarise(
                                            Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                            Aggvcs_emission    = sum(vcs_emission, na.rm=T),
                                            Aggvcs_carbon      = sum(vcs_carbon, na.rm=T) ,
                                            Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                            Aggvcs_water_green = sum(vcs_water_green, na.rm=T),
                                            Aggvcs_water_grey  = sum(vcs_water_grey, na.rm=T),
                                            Aggvcs_land        = sum(vcs_land, na.rm=T),
                                            Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                          ))
write.table(LossQty_Env_comodgrp_2015, "LossQty_Env_comodgrp_sdg_2015_19June19.csv", sep=",")
LossQty_Env_comodgrp_2015_2 <- as.data.table(LossQty_stage2a %>% 
                                             filter(timepointyears== 2015)%>%
                                             group_by(sdg_regions) %>%
                                             dplyr:: summarise(
                                               Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                               Aggvcs_emission    = sum(vcs_emission, na.rm=T),
                                               Aggvcs_carbon      = sum(vcs_carbon, na.rm=T) ,
                                               Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                               Aggvcs_water_green = sum(vcs_water_green, na.rm=T),
                                               Aggvcs_water_grey  = sum(vcs_water_grey, na.rm=T),
                                               Aggvcs_land        = sum(vcs_land, na.rm=T),
                                               Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                             ))

write.table(LossQty_Env_comodgrp_2015_2, "LossQty_Env_sdg_2015_19June19.csv", sep=",")
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


ConvFactor1  <- ReadDatatable('flw_lossperfactors')
ConvFactor1[,loss_per_clean := loss_per_clean/100]
ConvFactor1  <- ConvFactor1 %>% filter(tag_datacollection %in%  ExternalDataOpt)
ConvFactor1  <- ConvFactor1 %>% filter(!is.na(loss_per_clean ))
ConvFactor1 <- ConvFactor1 %>% filter(loss_per_clean < UB)
ConvFactor1$fsc_location1 = sapply(strsplit(ConvFactor1$fsc_location,"/"), '[', 1)

ConvFactor1a<-merge(ConvFactor1,gfli_basket[,c("measureditemcpc", "gfli_basket","basket_sofa_wu"), with= F], by = "measureditemcpc", all.x=T)
ConvFactor1a <-merge(ConvFactor1a,CountryGroup [,c("geographicaream49", "sdg_regions", "worldbank_income2018_agg","sofa_agg"), with= F], by = "geographicaream49", all.x=T)


quarts <- ConvFactor1a  %>% 
  filter(fsc_location1 %in% c("Farm","Transport","Storage", "Processing", "Retail") &
           timepointyears %in% seq(2003,2016,1) & 
           gfli_basket %in% c(na.omit(unique(gfli_basket))))%>%  
  group_by(sdg_regions, gfli_basket,fsc_location1) %>%  
  dplyr:: summarise(n= n()) %>% 
  do(data.frame(t(quantile(.$loss_per_clean,na.rm=T))))

write.table(quarts, "quarts_n.csv", sep=",")

### Extra Calculations

LossQty_Env_stage_2015_Extra_co2 <- as.data.table(LossQty_stage2a %>% 
                                                filter((timepointyears== 2015) & (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                group_by( gfli_basket) %>%
                                                dplyr:: summarise(
                                                  Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                  Minvcs_emissionF    = min(emission_kgco2perkgfood, na.rm=T),
                                                  Maxvcs_emissionF    = max(emission_kgco2perkgfood, na.rm=T),
                                                  Avgvcs_emissionF    = mean(emission_kgco2perkgfood, na.rm=T),
                                                  Aggvcs_emission    = sum(vcs_emission, na.rm=T)
                                                 
                                              #    Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                              #    Aggvcs_land        = sum(vcs_land, na.rm=T),
                                              #    Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                                ))

LossQty_Env_stage_2015_Extra_co2[, marginsvcs_emission := Aggvcs_emission/ Aggvcs_Qty]


LossQty_Env_stage_2015_Extra_h20 <- as.data.table(LossQty_stage2a %>% 
                                                filter((timepointyears== 2015) & (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                group_by( gfli_basket) %>%
                                                dplyr:: summarise(
                                                  Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                  Minvcs_H20F    = min(water_blue_m3_tonfood, na.rm=T),
                                                  Maxvcs_H20F    = max(water_blue_m3_tonfood, na.rm=T),
                                                  Avgvcs_H20F    = mean(water_blue_m3_tonfood, na.rm=T),
                                                  Aggvcs_H20    = sum(vcs_water_blue, na.rm=T)
                                                  
                                                  #    Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                                  #    Aggvcs_land        = sum(vcs_land, na.rm=T),
                                                  #    Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                                ))

LossQty_Env_stage_2015_Extra_h20[, marginsvcs_h20 := Aggvcs_H20/ Aggvcs_Qty]


LossQty_Env_stage_2015_Extra_land <- as.data.table(LossQty_stage2a %>% 
                                                    filter((timepointyears== 2015) & (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                    group_by( gfli_basket) %>%
                                                    dplyr:: summarise(
                                                      Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                      Minvcs_LandF    = min(land_ha_tonfood, na.rm=T),
                                                      Maxvcs_LandF    = max(land_ha_tonfood, na.rm=T),
                                                      Avgvcs_LandF    = mean(land_ha_tonfood, na.rm=T),
                                                      Aggvcs_Land    = sum(vcs_land, na.rm=T)
                                                      
                                                      #    Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                                      #    Aggvcs_land        = sum(vcs_land, na.rm=T),
                                                      #    Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                                    ))

LossQty_Env_stage_2015_Extra_land[, marginsvcs_Land := Aggvcs_Land/ Aggvcs_Qty]


production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
production$Value_measuredElement_5510 <-  production$value
nutrient_table <- getNutritionData(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE)
names(nutrient_table) <- tolower(names(nutrient_table))
names(production ) <- tolower(names(production))

ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value_measuredelement_5510")) %>% filter(timepointyears == 2015)

ProdQtySWS  <- merge(ProdQtySWS ,gfli_basket[,c("measureditemcpc", "gfli_basket")], by = c("measureditemcpc"),all.x=T)
ProdQtySWS_N  <- merge(ProdQtySWS,nutrient_table, by = c("geographicaream49","measureditemcpc"),all.x=T)
ProdQtySWS_N  <- merge(ProdQtySWS_N,CountryGroup, by = c("geographicaream49"),all.x=T)



ProdQtySWS_agg <- as.data.table(ProdQtySWS   %>% 
                                  filter((timepointyears== 2015)& ( gfli_basket != "Other" ))%>%
                                  group_by( gfli_basket) %>%
                                  dplyr:: summarise(
                                    Agg_Qty   = sum(value_measuredelement_5510, na.rm=T)))


ProdQtySWS_N_agg <- as.data.table(ProdQtySWS_N    %>% 
                                    filter((timepointyears== 2015)& ( gfli_basket != "Other" ))%>%
                                    group_by( gfli_basket,measuredelement) %>%
                                    dplyr:: summarise(
                                      Agg_Qty   = sum(value_measuredelement_5510*value, na.rm=T)))


#######################

LossQty_Env_stage_2015_Extra_co2_LA <- as.data.table(LossQty_stage2a %>% 
                                                    filter((timepointyears== 2015) & (sdg_regions == "Latin America and the Caribbean (MDG=M49)") & (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                    group_by( gfli_basket) %>%
                                                    dplyr:: summarise(
                                                      Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                      Minvcs_emissionF    = min(emission_kgco2perkgfood, na.rm=T),
                                                      Maxvcs_emissionF    = max(emission_kgco2perkgfood, na.rm=T),
                                                      Avgvcs_emissionF    = mean(emission_kgco2perkgfood, na.rm=T),
                                                      Aggvcs_emission    = sum(vcs_emission, na.rm=T)
                                                      
                                                   
                                                    ))

LossQty_Env_stage_2015_Extra_co2_LA[, marginsvcs_emission := Aggvcs_emission/ Aggvcs_Qty]


LossQty_Env_stage_2015_Extra_h20_LA <- as.data.table(LossQty_stage2a %>% 
                                                    filter((timepointyears== 2015)& (sdg_regions == "Latin America and the Caribbean (MDG=M49)") & (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                    group_by( gfli_basket) %>%
                                                    dplyr:: summarise(
                                                      Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                      Minvcs_H20F    = min(water_blue_m3_tonfood, na.rm=T),
                                                      Maxvcs_H20F    = max(water_blue_m3_tonfood, na.rm=T),
                                                      Avgvcs_H20F    = mean(water_blue_m3_tonfood, na.rm=T),
                                                      Aggvcs_H20    = sum(vcs_water_blue, na.rm=T)
                                                      
                                                      #    Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                                      #    Aggvcs_land        = sum(vcs_land, na.rm=T),
                                                      #    Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                                    ))

LossQty_Env_stage_2015_Extra_h20_LA[, marginsvcs_h20 := Aggvcs_H20/ Aggvcs_Qty]


LossQty_Env_stage_2015_Extra_land_LA <- as.data.table(LossQty_stage2a %>% 
                                                     filter((timepointyears== 2015) & (sdg_regions == "Latin America and the Caribbean (MDG=M49)")& (!is.na(fsc_location ))& (fsc_location != "retail") & ( gfli_basket != "Other" ))%>%
                                                     group_by( gfli_basket) %>%
                                                     dplyr:: summarise(
                                                       Aggvcs_Qty   = sum(value_measuredelement_5016_vcs, na.rm=T),
                                                       Minvcs_LandF    = min(land_ha_tonfood, na.rm=T),
                                                       Maxvcs_LandF    = max(land_ha_tonfood, na.rm=T),
                                                       Avgvcs_LandF    = mean(land_ha_tonfood, na.rm=T),
                                                       Aggvcs_Land    = sum(vcs_land, na.rm=T)
                                                       
                                                       #    Aggvcs_water_blue  = sum(vcs_water_blue, na.rm=T),
                                                       #    Aggvcs_land        = sum(vcs_land, na.rm=T),
                                                       #    Aggvcs_econ        = sum(vcs_econ, na.rm=T)
                                                     ))

LossQty_Env_stage_2015_Extra_land_LA[, marginsvcs_Land := Aggvcs_Land/ Aggvcs_Qty]


ProdQtySWS_agg <- as.data.table(ProdQtySWS   %>% 
                                  filter((timepointyears== 2015)& (sdg_regions == "Latin America and the Caribbean (MDG=M49)")& ( gfli_basket != "Other" ))%>%
                                  group_by( gfli_basket) %>%
                                  dplyr:: summarise(
                                    Agg_Qty   = sum(value_measuredelement_5510, na.rm=T)))


ProdQtySWS_N_agg <- as.data.table(ProdQtySWS_N    %>% 
                                    filter((timepointyears== 2015)& (sdg_regions == "Latin America and the Caribbean (MDG=M49)")& ( gfli_basket != "Other" ))%>%
                                    group_by( gfli_basket,measuredelement) %>%
                                    dplyr:: summarise(
                                      Agg_Qty   = sum(value_measuredelement_5510*value, na.rm=T)))

############


