

library(stats4)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(rpart)
library(scales)
library(plm)
library(broom)

library(lmtest)
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
LocalRun <- FALSE # For if you are running the model on a local environment and loading data tables from local fiiles
savesws <- FALSE
maxYear <- format(Sys.Date(), "%Y")

## Options for the user - See full documentation for the User Oriented Work Flow #
#updatemodel <- TRUE
if (!exists('updatemodel', inherits = FALSE)) {
  # the choice here is whether to run the model esetimates again potentially choosing new variables (TRUE)
  # or to use the parameters already estimated (FALSE)
  updatemodel <- swsContext.computationParams$updatemodel
}
if (!exists('subnationalestimates', inherits = FALSE)) {
  # the choice here is whether to use the Subnational Data aggregated via the Markov function and combine with current SWS Estimates (TRUE)
  # or to use the parameters already estimated (FALSE)
  subnationalestimates <- swsContext.computationParams$subnationalestimates
}
if (!exists('selectedYear_start')| !exists('selectedYear_end')) {
  ## Year should be a paramameter selected.
  selectedYear_start <- swsContext.computationParams$selectedyear_start
  selectedYear_end <- swsContext.computationParams$selectedyear_end
  selectedYear = as.character(as.numeric(selectedYear_start):as.numeric(selectedYear_end))
}
if (!exists('ctry_modelvar')) {
  ## IF just one country is modeled
  ctry_modelvar <- swsContext.computationParams$ctry_modelvar
  
}
if(CheckDebug()){
  ctry_modelvar <- 'All'
  updatemodel <- TRUE
  subnationalestimates <- TRUE
  selectedYear =  as.character(1990:maxYear)
  
}


print(paste("updatemodel: ", updatemodel))
print(paste("subnationalestimates: ", subnationalestimates))
print(paste('selectedYear:', paste(selectedYear, collapse = ', ')))

# These are all the potential tags on the SUbnational Estimates
# selecting data collection methods for aggregating the subnational estimates are
# based on those that will give the best range of representative data
DataCollectionTags_all <- c("Expert Opinion","-","SWS","NationalStatsYearbook" 
                            ,"NonProtected","Survey","Rapid Assessment","NationalAcctSys"              
                            ,"WRI Protocol","FBS/APQ","LitReview","Case Study"                   
                            ,"APHLIS","NP","Laboratory Trials","Modelled"                     
                            ,"Field Trial","Crop Cutting Field Experiment","Census" )
DataCollectionTags_represent <- c("Expert Opinion","-","SWS","NationalStatsYearbook" 
                                  ,"NonProtected","Survey","NationalAcctSys"              
                                  ,"WRI Protocol","FBS/APQ","LitReview" ,"Case Study"                    
                                  ,"APHLIS","NP","Laboratory Trials","Modelled", "Census" )

# DataCollectionTags_represent <- c("-","APHLIS","Case Study","Census","Declarative","Expert Opinion",
#                                   "FBS/APQ","LitReview","Modelled","NationalAcctSys",
#                                   "NationalStatsYearbook","NonProtected","NP","Survey","SWS")

LB <- 0.02
UB <- 0.65
#  c("SWS","NationalStatsYearbook","NonProtected","NationalAcctSys","FBS/APQ","Census",
#                                      "APHLIS", "Expert Opinion","Survey","Declarative","-","LitReview")
ExternalDataOpt <- DataCollectionTags_represent

# For aggregating the subnational using the markov function, 
# at present there is only the option for averaging the subnational estimates by stage. but could be altered in the future
# to model subnational-stages as functions 
MarkovOpt <- "aveatFSP"  # "model"

## This option is how the clusters are arranged. At the moment the best performing cluster was based on FBS Food Groups for estimation 
## This is not an option for the SWS user for consistency of estimates, should only be used to test the differences in estimates 
HierarchicalCluster <- "foodgroupname" # "isocode", "SDG.Regions"

############### Connection to the SWS ###########################################

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"

keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)
keys2 =c(areaVar,itemVar)

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
  
  # CountryGroup <- as.data.table(read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep='')))
  # FAOCrops <- as.data.table(read.csv(paste(githubsite, 'General/Cpc.csv', sep=''))) ## All Crops in the CPC system
  # ConvFactor1 <- as.data.table(read.csv(paste(githubsite, 'General/FLW_LossPercFactors.csv', sep='')))
  # names(CountryGroup) <- tolower(names(CountryGroup))
  # names(FAOCrops) <- tolower(names(FAOCrops))
  # names(ConvFactor1) <- tolower(names(ConvFactor1))
  # ConvFactor1[,loss_per_clean := as.numeric(levels(loss_per_clean))[loss_per_clean]]
  
  
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

#####  Collects the data from the SWS #####
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops     <- ReadDatatable("fcl2cpc_ver_2_1")
ConvFactor1  <- ReadDatatable('flw_lossperfactors')
#fbsTree      <- ReadDatatable("fbs_tree")
fbsTree      <- ReadDatatable("gfli_basket")


CountryGroup[,"geographicaream49":=CountryGroup$m49_code]
CountryGroup[,"country":=CountryGroup$m49_region]
CountryGroup$country <- tolower(CountryGroup$country)

FAOCrops[, "crop" := FAOCrops$description]
FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]


Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
nutrient_table <- getNutritionData(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE)
LossesQty <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5016')

names(Losses) <- tolower(names(Losses))
Losses$geographicaream49 <- as.character(Losses$geographicaream49)

gfli_basket <- ReadDatatable("gfli_basket")
Loss_per_stage <-   ReadDatatable("sn_vc_est")
Baskets <- ReadDatatable("sdg123_commoditybasket")
CountryGroup$geographicaream49 <- CountryGroup$m49_code
FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]
LossesQty <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5016')
Loss_per_stage$value_measuredelement_5126 <- Loss_per_stage$value

setnames(Losses, old = c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") ,
         new =   c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ))
setnames(LossesQty, old = c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") ,
         new =   c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ))

names(production) <- tolower(names(production))
Losses$geographicaream49 <- as.character(Losses$geographicaream49)
production$geographicaream49 <- as.character(production$geographicaream49)

LossesQty$geographicaream49 <- as.character(LossesQty$geographicaream49)
Baskets$geographicaream49 <- as.character(Baskets$geographicaream49)
Loss_per_stageB <- Loss_per_stage[timepointyears==2015,]
Loss_per_stageB[, comb := paste(geographicaream49, measureditemcpc, sep=";")]
Baskets[, comb := paste(geographicaream49, measureditemcpc, sep=";")]



LossQty_stage <- merge(Baskets,Loss_per_stageB, by = "comb",type= 'left')
LossQty_stage[,value_measuredelement_5016_vcs := p0q0*value]


LossQty_stage2a <-merge(LossQty_stage,gfli_basket[,c("measureditemcpc", "gfli_basket","basket_sofa_wu"), with= F], by.x = "measureditemcpc.x", by.y="measureditemcpc", all.x=T)
LossQty_stage2a <-merge(LossQty_stage2a,CountryGroup [,c("geographicaream49", "sdg_regions", "worldbank_income2018_agg","sofa_wu_agg", "sofa_agg"), with= F], by.x = "geographicaream49.x",  by.y = "geographicaream49",all.x=T)



production2a <-merge(production,gfli_basket[,c("measureditemcpc", "gfli_basket","basket_sofa_wu"), with= F], by.x = "measureditemcpc", by.y="measureditemcpc", all.x=T)
production2a <-merge(production2a,CountryGroup [,c("geographicaream49", "sdg_regions", "worldbank_income2018_agg","sofa_wu_agg", "sofa_agg"), with= F], by.x = "geographicaream49",  by.y = "geographicaream49",all.x=T)
production2a[, comb := paste(geographicaream49, measureditemcpc, sep=";")]
production2a <- merge(Baskets,production2a, by = "comb",type= 'left')

LossesQty2a <-merge(LossesQty,gfli_basket[,c("measureditemcpc", "gfli_basket","basket_sofa_wu"), with= F], by.x = "measureditemcpc", by.y="measureditemcpc", all.x=T)
LossesQty2a <-merge(LossesQty2a,CountryGroup [,c("geographicaream49", "sdg_regions", "worldbank_income2018_agg","sofa_wu_agg", "sofa_agg"), with= F], by.x = "geographicaream49",  by.y = "geographicaream49",all.x=T)
LossesQty2a[, comb := paste(geographicaream49, measureditemcpc, sep=";")]
LossesQty2a <- merge(Baskets,LossesQty2a, by = "comb",type= 'left')

### By Country #####
SumQt = as.data.table(production2a %>%
                          filter(timepointyears == 2015) %>%
                          group_by_(.dots = list("sofa_wu_agg","basket_sofa_wu")) %>%
                          dplyr:: summarise(Sum_D = sum(intprice*value, na.rm = TRUE)))

SumQ1= as.data.table(LossesQty2a  %>%
                        filter(timepointyears == 2015) %>%
                        group_by_(.dots = list("sofa_wu_agg","basket_sofa_wu")) %>%
                        dplyr:: summarise(Sum_N = sum(value_measuredelement_5016*intprice, na.rm = TRUE)))

SumP0Qt = as.data.table(LossQty_stage2a  %>%
  group_by_(.dots = list("sofa_wu_agg","basket_sofa_wu")) %>%
  dplyr:: summarise(Sum_D = sum(value_measuredelement_5016_vcs, na.rm = TRUE)))

SumP0Q0 = as.data.table(LossQty_stage2a  %>%
                        group_by_(.dots = list("sofa_wu_agg","basket_sofa_wu","fsc_location")) %>%
                        dplyr:: summarise(Sum_N = sum(value_measuredelement_5016_vcs, na.rm = TRUE)))

LossQty_reg= data.table( 
  merge(SumQ1,SumQt,
        by = c("sofa_wu_agg","basket_sofa_wu"),
        all.x = TRUE))

LossQty_stage_B = data.table( 
  merge(SumP0Q0,SumP0Qt,
        by = c("sofa_wu_agg","basket_sofa_wu"),
        all.x = TRUE))

LossQty_stage_B[, L_ijk := Sum_N/Sum_D]
LossQty_stage_B[,Sum_N := NULL]
LossQty_stage_B[,Sum_D := NULL]

# LossQty_reg[, L_ijk := Sum_N/Sum_D]
# LossQty_reg[,Sum_N := NULL]
# LossQty_reg[,Sum_D := NULL]
LossQty_reg <- SumQ1[!is.na(sofa_wu_agg),]

write.table(LossQty_stage_B, "LossPer_Stage_SDG_WUCGroup.csv", sep=",")
write.table(LossQty_reg, "LossPerGroup_SDG_WUCGroup.csv", sep=",")
################################
### By Country #####
SumP0Qt2 = as.data.table(LossQty_stage2a  %>%
                          group_by_(.dots = list("sdg_regions","gfli_basket.x")) %>%
                          dplyr:: summarise(Sum_D = sum(value_measuredelement_5016_vcs, na.rm = TRUE)))

SumP0Q02 = as.data.table(LossQty_stage2a  %>%
                          group_by_(.dots = list("sdg_regions","gfli_basket.x","fsc_location")) %>%
                          dplyr:: summarise(Sum_N = sum(value_measuredelement_5016_vcs, na.rm = TRUE)))


LossQty_stage_B2 = data.table( 
  merge(SumP0Q02,SumP0Qt2,
        by = c("sdg_regions","gfli_basket.x"),
        all.x = TRUE))

LossQty_stage_B2[, L_ijk := Sum_N/Sum_D]
LossQty_stage_B2[,Sum_N := NULL]
LossQty_stage_B2[,Sum_D := NULL]
write.table(LossQty_stage_B2, "LossPer_Stage_SDG_CGroup.csv", sep=",")


#####################################################
LossQty_reg= data.table( 
  merge(Loss_per_stage,LossesQty,
        by = c("geographicaream49","measureditemcpc", "timepointyears"),
        all.x = TRUE))


LossQty_reg= data.table( 
  merge(LossQty_reg,CountryGroup,
        by.x  = c("geographicaream49"), by.y = c("m49_code"),
        all.x = TRUE))

LossQty_reg= data.table( 
  merge(LossQty_reg,fbsTree,
        by.x  = c("measureditemcpc"), by.y = c("measureditemcpc"),
        all.x = TRUE))

LossQty_reg[,LossQtyStg := value*value_measuredelement_5016]
LossQty_reg = LossQty_reg[!is.na(LossQtyStg),]
Loss_Per_agg <- as.data.table(LossQty_reg %>%
                filter(timepointyears == 2015) %>%
                group_by_(.dots = list("sofa_wu_agg","gfli_basket","fsc_location")) %>%
                dplyr:: summarise(mean_N = mean( value, na.rm = TRUE),min_N = min( value, na.rm = TRUE), max_N = max( value, na.rm = TRUE) ))

Loss_Per_agg2 <- as.data.table(LossQty_reg %>%
                                filter(timepointyears == 2015) %>%
                                group_by_(.dots = list("sofa_wu_agg","gfli_basket","fsc_location")) %>%
                                dplyr:: summarise(mean_N = sum( LossQtyStg, na.rm = TRUE),min_N = min( LossQtyStg, na.rm = TRUE), max_N = max(LossQtyStg, na.rm = TRUE) ))

write.table(Loss_Per_agg2 , "LossPer_Stage_SDG_WUGeoAg_qty.csv", sep=",")
