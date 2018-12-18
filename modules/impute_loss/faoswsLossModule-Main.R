#' Module for estimating Food Loss and Waste 
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English Marco Mingione 
#' 
#' 
#  This module does two things - 
# 1) runs the Loss model for the Food Balance Sheets, which will select new variables and estimate new coefficients 
# 2) If updatemodel =FALSE then, the model will take the last estimation of the model, pull the previous coefficients and apply them to the 
#  selected year.

######### Load all libraries ###########
#install.packages('faosws')
#install_github(repo = "SWS-Methodology/faoswsFlag")
#install_github(repo = "SWS-Methodology/faoswsUtil")
#install.packages("faoswsLoss", repo = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/") 
#install.packages("faoswsModules", repos = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/")

library(stats4)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(rpart)
library(scales)
library(plm)

library(lmtest)
library(magrittr) 


library(faosws)
library(faoswsUtil)
library(faoswsLoss)


suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(lme4)
    library(data.table)
    library(magrittr)
    library(plyr)
    library(dplyr)
    
  })


############# Computation Parameters #####################################
LocalRun <- FALSE # For if you are running the model on a local environment and loading data tables from local fiiles
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
selectedModelYear = as.character(1961:maxYear)

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

DataCollectionTags_represent <- c("-","APHLIS","Case Study","Census","Declarative","Expert Opinion",
                                  "FBS/APQ","LitReview","Modelled","NationalAcctSys",
                                  "NationalStatsYearbook","NonProtected","NP","Survey","SWS")
UB<- 0.65
LB <- 0.05
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
ConvFactor1  <- ReadDatatable('flw_lossperfactors_')
fbsTree      <- ReadDatatable("fbs_tree")

CountryGroup[,"geographicaream49":=CountryGroup$m49_code]
CountryGroup[,"country":=CountryGroup$m49_region]
CountryGroup$country <- tolower(CountryGroup$country)

FAOCrops[, "crop" := FAOCrops$description]
FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]

names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"

fbsTree$GFLI_Basket <- 'NA'
fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals',]
fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Meat & Animals Products',] # |foodGroupName == "PRODUCTS FROM FISH",
fbsTree[GFLI_Basket == "NA", 'GFLI_Basket'] <- NA


### Importing the tables ###
Temperature <-  ReadDatatable("temp_climate_month_ctry")
Precipitation <- ReadDatatable("rain_climate_month_ctry")
CropCalendar <- ReadDatatable("crop_calendar_nov17")

## Yearly Variables ##
LossTablelist_Yr <- c('world_bank_pinksheets')
LossTables_Yr <- list()
LossTables_Yr <- lapply(LossTablelist_Yr,ReadDatatable)
names(LossTables_Yr) <- LossTablelist_Yr


## Variables by country and Year ##
LossTablelist_ctryYr <- c('bm_gsr_fcty_cd',
                          'bm_gsr_totl_cd',
                          'bm_trf_prvt_cd',
                          'bn_gsr_fcty_cd',
                          'bn_trf_curr_cd',
                          'bx_gsr_fcty_cd',
                          'bx_gsr_totl_cd',
                          'bx_trf_curr_cd',
                          'dt_dod_dect_ex_zs',
                          'dt_dod_dstc_xp_zs',
                          'dt_dod_pvlx_ex_zs',
                          'dt_int_dect_ex_zs',
                          'dt_oda_odat_mp_zs',
                          'dt_tds_dect_ex_zs',
                          'dt_tds_dppf_xp_zs',
                          'dt_tds_dppg_xp_zs',
                          'eg_elc_accs_zs',
                          'gc_tax_ypkg_cn',
                          'gc_tax_ypkg_rv_zs',
                          'gc_tax_ypkg_zs',
                          'ny_adj_nnty_cd',
                          ## 'ny_adj_nnty_kd',
                          'ny_adj_nnty_kd_zg',
                          'ny_adj_nnty_pc_cd',
                          'ny_adj_nnty_pc_kd',
                          'ny_adj_nnty_pc_kd_zg',
                          ## 'ny_gdy_totl_kn',
                          'ny_gsr_nfcy_cd',
                          'ny_gsr_nfcy_cn',
                          'ny_gsr_nfcy_kn',
                          'si_dst_02nd_20',
                          'si_dst_03rd_20',
                          'si_dst_04th_20',
                          'si_dst_05th_20',
                          'si_dst_10th_10',
                          'si_dst_frst_10',
                          'si_dst_frst_20',
                          'si_spr_pc40',
                          'si_spr_pc40_zg',
                          'si_spr_pcap',
                          'si_spr_pcap_zg',
                          'tm_val_mrch_hi_zs',
                          'tm_val_mrch_or_zs',
                          'tm_val_mrch_r1_zs',
                          'tm_val_mrch_r2_zs',
                          'tm_val_mrch_r3_zs',
                          'tm_val_mrch_r5_zs',
                          'tm_val_mrch_r6_zs',
                          'tm_val_mrch_wr_zs',
                          'tx_val_mrch_hi_zs',
                          'tx_val_mrch_or_zs',
                          'tx_val_mrch_r1_zs',
                          'tx_val_mrch_r2_zs',
                          'tx_val_mrch_r3_zs',
                          'tx_val_mrch_r4_zs',
                          'tx_val_mrch_r5_zs',
                          'tx_val_mrch_r6_zs',
                          'tx_val_mrch_wr_zs',
                          'wp_time_01_8',
                          'wp_time_01_9',
                          'wp15163_4_8',
                          'wp15163_4_9',
                          'credittoag',
                          'investment_consumptionfixedcapital',
                          'investment_grosscapitalstocks',
                          'investment_grossfixedcapitalformation_usd',
                          'investment_netcapitalstocks',
                          'ironsteelimport7055475',
                          'lpidata',
                          'sankey_diagram_iea20apr17',
                          'spendingonag_ifpri_com'
)
LossTables_ctryYr <- list()
LossTables_ctryYr <- lapply(LossTablelist_ctryYr,ReadDatatable)
names(LossTables_ctryYr) <- LossTablelist_ctryYr
#LossTables_ctryYr[[LossTablelist_ctryYr[ii]]]


finalModelData = 
  {
    production <- getProductionData(areaVar,itemVar,yearVar,elementVar, selectedYear) # Value_measuredElement_5510
    imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
    lossProtected <- getLossData(areaVar,itemVar,yearVar,elementVar,selectedYear,protected = TRUE)     # Value_measuredElement_5016
    lossProtected$value_measuredelement_5126 = 0
    
    names(production)[ names(production) == "Value"] <-  "value_measuredelement_5510"
    names(imports)[ names(imports) == "Value"] <-  "value_measuredelement_5610"
    names(lossProtected)[ names(lossProtected) == "Value"] <-  "value_measuredelement_5016"
    names(lossProtected)[ names(lossProtected) == "measuredItemSuaFbs"] <-  "measureditemcpc"
    names(lossProtected) <- tolower(names(lossProtected))
    names(production) <- tolower(names(production))
    names(imports) <- tolower(names(imports))
    
    production$geographicaream49 <- as.character(production$geographicaream49)
    production$timepointyears <- as.numeric(production$timepointyears)
    imports$timepointyears<- as.numeric(imports$timepointyears)
    
    prod_imports <- merge(production,imports, by= keys_lower, all.x = TRUE)
    prod_imports[,prod_imports := rowSums(.SD, na.rm = TRUE), .SDcols=c("value_measuredelement_5510","value_measuredelement_5610")]
    prod_imports <-prod_imports[,c(keys_lower,"value_measuredelement_5510","value_measuredelement_5610","prod_imports"),with=F]

    lossProtected$geographicaream49 <- as.character(lossProtected$geographicaream49)
    lossProtected$timepointyears <- as.numeric(lossProtected$timepointyears)
    #Data for the model
    lossData <-  merge(prod_imports,lossProtected,  by= keys_lower, all.y= TRUE)
    lossData[, loss_per_clean := (value_measuredelement_5016/value_measuredelement_5510)]
    lossData[, loss_per_clean_pi := (value_measuredelement_5016/prod_imports)]
    lossData[, per_diff :=loss_per_clean-loss_per_clean_pi]
    lossData[protected ==T,value_measuredelement_5126:=loss_per_clean]
    lossData[,value_measuredelement_5126:=loss_per_clean]
    ### Some countries are import dependent and protected losses are over 100%, therefore % should be applied to both production + imports
    comodities = lossData[per_diff>.1,c("geographicaream49","measureditemcpc","value_measuredelement_5016","prod_imports", "loss_per_clean",
                                                              "loss_per_clean_pi", "per_diff"),with=F]
    
    
    comodities[, combp := paste(geographicaream49,measureditemcpc, sep=";")]
    comb <- unique(comodities$combp)
    comb <- c(comb, "604;01216","604;01243", "780;01213","332;01233", "780;01239.01","756;01253.01","780;01290.90","780;01311", "604;01344.02",
              "604;01460","266;01540", "442;01701")
    #comodities[per_diff>1,]
    lossData[, fsc_location := "SWS"]
    for(t in unique(comodities$geographicaream49)){
      for(i in unlist(unique(comodities[geographicaream49== t,"measureditemcpc",with=F]))){
  
        lossData[geographicaream49 == t & measureditemcpc == unname(unlist(i)),value_measuredelement_5126:=loss_per_clean_pi]
        lossData[geographicaream49 == t & measureditemcpc == unname(unlist(i)),fsc_location := "SWS;Prod_imp"]
        
      }
    }
   
    
    
    
    ## Some commodities still produce greater than 100 % losses
    lossData[value_measuredelement_5126 > 1,]
    lossData[value_measuredelement_5126 > 1,value_measuredelement_5126:=0]
    
    names(lossData) <- tolower(names(lossData))
    lossData <- merge(lossData,CountryGroup[,c("isocode","geographicaream49", "country"), with=FALSE],  by = c("geographicaream49"), all.x = TRUE, all.y = FALSE)
    #lossData <- merge(lossData,FAOCrops[,c("measureditemcpc","crop")],  by = c("measureditemcpc"), all.x = TRUE, all.y = FALSE)
    lossData[,loss_per_clean:=value_measuredelement_5126]
    #write.table(highLosses,paste(githubsite, 'General/highLosses',date, '.csv', sep=''),sep=',' )
    
    # creating time series:
    timeSeriesData <- as.data.table(expand.grid(timepointyears = sort(unique(production$timepointyears)),
                                                geographicaream49 = as.character(unique(production$geographicaream49)),
                                                measureditemcpc = as.character(unique(production$measureditemcpc))))
    
    
    ###### Creates the Table of estimated data needed ####
    timeSeriesDataToBeImputed <- merge(timeSeriesData, lossData,  by= keys_lower, all.x = TRUE, all.y = TRUE)
    timeSeriesDataToBeImputed[is.na(loss_per_clean), loss_per_clean := 0]
    timeSeriesDataToBeImputed[is.na(value_measuredelement_5016), value_measuredelement_5016 := 0]
    
    timeSeriesDataToBeImputed[,value_measuredelement_5126 := loss_per_clean]
    timeSeriesDataToBeImputed[,flagcombination:="0"]
    timeSeriesDataToBeImputed <- subset(timeSeriesDataToBeImputed,
                                        select = c(keys_lower,"value_measuredelement_5016", "value_measuredelement_5126","flagcombination","flagobservationstatus.y","flagmethod.y","loss_per_clean")
    )
    setnames(timeSeriesDataToBeImputed, old =  c("timepointyears","geographicaream49","measureditemcpc","value_measuredelement_5016", "value_measuredelement_5126","flagcombination","flagobservationstatus.y","flagmethod.y","loss_per_clean"),
             new =  c("timepointyears","geographicaream49","measureditemcpc","value_measuredelement_5016", "value_measuredelement_5126","flagcombination","flagobservationstatus","flagmethod","loss_per_clean") )
    

    
    #### Protected Data ###
    flagValidTableLoss <- as.data.table(flagValidTable)
    protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
      .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
    timeSeriesDataToBeImputed$flagcombination <- " "
    timeSeriesDataToBeImputed[,flagcombination :=  paste(flagobservationstatus, flagmethod, sep = ";")] 
    
    timeSeriesDataToBeImputed[flagcombination %in% protectedFlag$flagCombination,Protected := TRUE,]
    timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed %>%  arrange(geographicaream49,timepointyears,measureditemcpc, flagcombination) 
    
    #### Extract Duplicates ####
    dd <-timeSeriesDataToBeImputed[duplicated(timeSeriesDataToBeImputed) | duplicated(timeSeriesDataToBeImputed, fromLast=TRUE)]
    timeSeriesDataToBeImputed <- unique(timeSeriesDataToBeImputed)

    names(timeSeriesDataToBeImputed) <-tolower(names(timeSeriesDataToBeImputed))

    
    lossData <- subset(lossData, 
                       select = c(keys_lower,"isocode","country","loss_per_clean","fsc_location","flagobservationstatus.y", "flagmethod.y"))
    
    
} 
print(paste(" Number of lossData available: ", dim(lossData)[1] ))
print(paste(" Number of losses to estimate: ", dim(timeSeriesDataToBeImputed[is.na(protected),])[1] ))

if(updatemodel){  
  ########### Loss Factor Data and Aggregation ################### 
  ## This section imports the data of the loss factors and subnational data and then merges it with the country designations for the SDG 
  if(subnationalestimates){
       # brings in the current file of converstion factors and divides by 100
       ConvFactor1[,loss_per_clean := loss_per_clean/100]
       # Filters out the non-representative observations
       #unique(ConvFactor1[!tag_datacollection %in%  ExternalDataOpt  ,"tag_datacollection",with=F]) 
       ConvFactor1  <- ConvFactor1 %>% filter(tag_datacollection %in%  ExternalDataOpt)
       ConvFactor1  <- ConvFactor1 %>% filter(!is.na(loss_per_clean ))
       ConvFactor1 <- ConvFactor1 %>% filter(loss_per_clean < UB)
       #ConvFactor1$measureditemcpc <- addHeadingsCPC(ConvFactor1$measureditemcpc)
       names(ConvFactor1)[names(ConvFactor1)=='year'] <-'timepointyears'
       
       ## Runs the Markov Model to standardize estimates 
       markov <- FSC_Markov(RawData=ConvFactor1,opt="aveatFSP")
       
       FullSet <- rbind(markov,lossData, fill=T)
      
       duplicates <- FullSet[duplicated(FullSet) | duplicated(FullSet, fromLast=TRUE),]
       
       FullSet <- unique(FullSet)
      
    }else{FullSet <- lossData}
  
  FullSet <-join(FullSet, FAOCrops[,c("measureditemcpc","crop"), with=FALSE], by= c("measureditemcpc"))
  FullSet <- FullSet %>% filter(loss_per_clean != 0)
  FullSet <- FullSet %>% filter(loss_per_clean > LB)
  FullSet <- FullSet %>% filter(loss_per_clean < UB)
  
  #write.table(FullSet,paste(githubsite, 'General/FullSet.csv', sep=''),sep=',' )
  ### Save the intermediate aggregation table  to the sws
  names(FullSet) <- tolower(names(FullSet))
  ## Delete
  table = "aggregate_loss_table"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset,  FullSet[,c("geographicaream49","timepointyears","measureditemcpc","isocode","country","crop","loss_per_clean","fsc_location"), with=FALSE])
  Finalise(changeset)
  
  print("Markov chain complete")
  
  #FullSet <- ReadDatatable("aggregate_loss_table")
  ########### Variables for the module  ###################   
  # Adds the explanatory Varaibles,
  
  Predvar <- c()
  
  Data_Use_train0  <- NULL 
  while(is.null(Data_Use_train0)){ 
    Data_Use_train0 <- tryCatch(VariablesAdd1(FullSet,keys_lower,Predvar,FALSE,'00',CountryGroup,fbsTree,Temperature,Precipitation,CropCalendar,LossTables_Yr,LossTables_ctryYr),
                               error = function(error_condition) {
                                return(NULL)
                              }
                              
                              )
  }
  ## These options change the method of estimating the missing data in the explanatory dataset
  #Data_Use_train <- VariablesAdd1(FullSet,keys_lower,Predvar,"ctry",'00')
  #Data_Use_train2 <- VariablesAdd1(FullSet,keys_lower,Predvar,"var",'00')
  #Data_Use_train3 <- VariablesAdd1(FullSet,keys_lower,Predvar,"RF",'00')
  print("Variables Added")
  
  
  ####### Model Estimation - Percentages only ############
  ctry_modelvar <-  c("all") #c("100", "191" ,"196", "203" ,"208", "233" ,"246", "250", "276" ,"300" ,"348" ,"372", "380", "40" , "428",
  # "440", "442" ,"470", "528", "56" , "616", "620" ,"642","703" ,"705")  
  # this model estimates by country and does carry-overs, once modeled the data is temporarily protected
  timeSeriesDataToBeImputed_ctry2 <- LossModel_ctry(Data= Data_Use_train0,timeSeriesDataToBeImputed,ctry_modelvar,HierarchicalCluster,keys_lower)


  #save(timeSeriesDataToBeImputed_ctry2 , file = "timeSeriesDataToBeImputed_ctry2.RData")
  timeSeriesDataToBeImputed2 <- LossModel(Data= Data_Use_train0[fsc_location !="SWS;Prod_imp",], timeSeriesDataToBeImputed, production,HierarchicalCluster,keys_lower)
  
  #timeSeriesDataToBeImputed2 <-timeSeriesDataToBeImputed
  timeSeriesDataToBeImputed2$protected = FALSE
  timeSeriesDataToBeImputed2[flagcombination %in% protectedFlag$flagCombination,protected := TRUE,]
  
  ### Check for duplicates ###
  dups <- timeSeriesDataToBeImputed2[duplicated(timeSeriesDataToBeImputed2) | duplicated(timeSeriesDataToBeImputed2, fromLast=TRUE)]
  timeSeriesDataToBeImputed2 <- timeSeriesDataToBeImputed2[!duplicated(timeSeriesDataToBeImputed2),]
  

  ### Adds the Production and Imports to get the quantities ########## 
  # Multiplies loss percentages by production
  timeSeriesDataToBeImputed_PI <- merge(timeSeriesDataToBeImputed2,prod_imports, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
  
  timeSeriesDataToBeImputed_PI[,value_measuredelement_5126 := loss_per_clean,]
  timeSeriesDataToBeImputed_PI[!is.na(protected),value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510]
  
  ## For import dependednt countries and for consistency, the loss factor is applied to production plus imports
  for(t in unique(comodities$geographicaream49)){
    for(i in unlist(unique(comodities[geographicaream49== t,"measureditemcpc",with=F]))){
      
      timeSeriesDataToBeImputed_PI[!is.na(protected) & geographicaream49 == t & measureditemcpc == unname(unlist(i)),
                                   value_measuredelement_5016 := value_measuredelement_5126*(value_measuredelement_5510+ value_measuredelement_5610)]
     
      
    }
  }
  
  nonEstCommod <- unique(timeSeriesDataToBeImputed_PI[value_measuredelement_5126==0,"measureditemcpc",with=F])
  timeSeriesDataToBeImputed_PI <- timeSeriesDataToBeImputed_PI %>% filter(!is.na(prod_imports))
  timeSeriesDataToBeImputed_PI <- timeSeriesDataToBeImputed_PI %>% filter(!flagcombination == "NA;NA" )
  timeSeriesDataToBeImputed_PI  <- timeSeriesDataToBeImputed_PI %>% filter(!is.na(value_measuredelement_5016))
  ## Double check that the protected loss elements haven't been overwritten
  timeSeriesDataToBeImputed_PI[ geographicaream49== 4 & measureditemcpc == "0111" ,]
  timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed_PI
  
  ### Narrows the data to the CPCs in the loss domain
  
  ### Splits the data tables for the SWS ####
  timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5016","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5016[, measuredElement := "5016"]
  setnames(timeSeriesDataToBeImputed_5016, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"  ,"Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5016, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  #timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed_5016 %>% filter(!is.na(flagMethod))
  
  ##---------------------
  timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5126","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5126[, measuredElement := "5126"]
  setnames(timeSeriesDataToBeImputed_5126, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5126, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  #timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed_5126 %>% filter(!is.na(flagMethod))
  
  ### SDG Calculations - for submission only using officially reported countries #####
  timeSeriesDataToBeImputed_ctry2[!flagcombination %in% c("NA;NA","I;e","M;-"), "flagcombination", with=F ]
  
  SDG_NM <- timeSeriesDataToBeImputed_ctry2[!flagcombination %in% c("NA;NA","I;e","M;-") & loss_per_clean>0 & timepointyears>1990,c("geographicaream49","measureditemcpc", "timepointyears"),with=F]
  SDG_NM <- SDG_NM[duplicated(timeSeriesDataToBeImputed_ctry2[!flagcombination %in% c("NA;NA","I;e","M;-"),c("geographicaream49","measureditemcpc"),with=F]),]
  print(paste("percent of data points estimated by carryover and country modeling: ",nrow(SDG_NM)/nrow(timeSeriesDataToBeImputed_5126)))
  
  
  ####
  
  DataSave <- rbind(timeSeriesDataToBeImputed_5016,timeSeriesDataToBeImputed_5126)
  # # Save to the SWS
  stats = SaveData(domain = "lossWaste",
                   dataset="loss",
                   data = timeSeriesDataToBeImputed_5016
  ) 
  # Save to the SWS
  stats = SaveData(domain = "lossWaste",
                   dataset="loss",
                   data = timeSeriesDataToBeImputed_5126
  )
   sprintf(
    "Module completed in %1.2f minutes.
    Values inserted: %s
    appended: %s
    ignored: %s
    discarded: %s",
    difftime(Sys.time(), startTime, units = "min"),
    stats[["inserted"]],
    stats[["appended"]],
    stats[["ignored"]],
    stats[["discarded"]]
  )
  print("Model Ran")                                          
 
}

if(updatemodel == FALSE){
  ###Computation parameters##
  LastRun <- TRUE
  Cluster2Update <- na.omit(unique(fbsTree$GFLI_Basket))

  ## Read DataTables of the existing model runs 
  modelRuns <- ReadDatatable("lossmodelruns")
  
  Cluster2Update <- na.omit(unique(fbsTree$GFLI_Basket))[vi]
  
  name <-unique(fbsTree[GFLI_Basket %in% Cluster2Update,foodgroupname])
  CPCs <-unique(fbsTree[GFLI_Basket %in% Cluster2Update,measureditemcpc])
    
  modelRuns2 <- modelRuns[(cluster %in% name) ,]
  modelRuns2 <- modelRuns2[(daterun == max(modelRuns2$daterun)),]

  formula  <- modelRuns2$formula 
  coeffSig <- unlist(strsplit(modelRuns2$coeffsig, "##"))
  Inters <- unlist(strsplit(modelRuns2$coeffnames, "##"))
  coeff <- unlist(strsplit(modelRuns2$coeff, "##"))
  modeEst <- as.data.table(cbind(Inters, coeff))
  coeffindex <- unlist(strsplit(modelRuns2$coeffindex, "##"))
  coeffDV <- unlist(strsplit(modelRuns2$coeffdv, "##"))
  

  # Data prep
  #### Protected Data ###
  datasetN <- names(timeSeriesDataToBeImputed)
  flagValidTableLoss <- as.data.table(flagValidTable)
  protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
    .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
  timeSeriesDataToBeImputed[,flagcombination :=  paste(flagobservationstatus, flagmethod, sep = ";")] 
  
  timeSeriesDataToBeImputed[flagcombination %in% protectedFlag$flagCombination,Protected := TRUE,]
  timeSeriesDataToBeImputed[Protected == TRUE,loss_per_clean:= loss_per_clean]
  
  timeSeriesDataToBeImputedGroups <- join(timeSeriesDataToBeImputed, fbsTree, by = c("measureditemcpc"),type= 'left', match='all')
  
  #Including only the years that need to be updated
  timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed[timepointyears %in% selectedYear,]
  
  DataPred <-  timeSeriesDataToBeImputed %>% filter(measureditemcpc %in% CPCs)
  DataPred <- VariablesAdd1(DataPred,keys_lower, coeffSig)
  names(DataPred) <- tolower(names(DataPred))
  names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 
  datapred <- DataPred
  ####################### Results #########################################
  OnlySigCoeff =T
  # COmbines the coefficients to create an estimate for every column in the group
  coeffN <- na.omit(coeffN) 
  
  coeffindex <-  grep(keys_lower[1],Inters, perl=TRUE, value=TRUE)
  coeffDV <-     grep(keys_lower[3],Inters, perl=TRUE, value=TRUE)
    
  datapred[,countydummy :=0]
  datapred[,cropdummy :=0]
  datapred[,intercept :=0]
    
  for(ind1 in 1:length(unique(gsub(keys_lower[1],"", coeffindex)))){
      datapred[geographicaream49 == gsub(keys_lower[1],"", coeffindex)[ind1],countydummy := as.numeric(modeEst[Inters %in% coeffindex[ind1],coeff]),]
    }
  for(ind2 in 1:length(unique(gsub(keys_lower[3],"", coeffDV)))){ 
      datapred[measureditemcpc == gsub(keys_lower[3],"", coeffDV)[ind2],cropdummy:= as.numeric(modeEst[Inters %in% coeffDV[ind2],coeff]),]
      
    }
  datapred[(cropdummy == 0) & (countydummy ==0),intercept:=as.numeric(modeEst[Inters %in% "(Intercept)",coeff])]
    
  if(length(coeffN) >0){
      # Applies the weights of the estimation across the entire cluster sets, using the demeaned coefficient as the intercept  (coefficients(mod2)[1]  
      datapred[,losstransf := 
                 rowSums(mapply(`*`,as.numeric(modeEst[Inters %in% coeffSig,coeff]),datapred[ ,coeffSig,with=F]), na.rm=TRUE)+
                 countydummy+cropdummy+intercept,] 
  }else{ datapred[,losstransf :=  countydummy+cropdummy+intercept,]}
  
  #Covert 
  names(datapred) <- tolower(names(datapred))
  names(production) <- tolower(names(production))
  production$geographicaream49 <-as.character(production$geographicaream49)
  datapred$geographicaream49 <-as.character(datapred$geographicaream49)
    
  datapred[datapred$losstransf !=0, loss_per_clean := exp(datapred$losstransf)/(1+exp(datapred$losstransf)),]
  #datapred[datapred$losstransf !=0, loss_per_clean := datapred$losstransf[datapred$losstransf !=0]]
  datapred[,value_measuredelement_5126 := loss_per_clean,]
  datapred[,value_measuredelement_5016 := 0,]
  datapred[,flagobservationstatus := 'I',] 
  datapred[,flagmethod:= 'e',]
  datapred[,flagcombination := 'I;e',]
  datapred[,protected := FALSE,]
  medianLoss <- median(datapred$value_measuredelement_5126, na.rm=TRUE)
  print(paste('average loss:',medianLoss*100, "%"))
 
  
  timeSeriesDataToBeImputed$geographicaream49 <- as.character(timeSeriesDataToBeImputed$geographicaream49)
  
  int1 <-datapred[,tolower(datasetN), with=F]
  nameadd <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
  names(int1)[!names(int1) %in% keys_lower] <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
  
  timeSeriesDataToBeImputed <-  merge(timeSeriesDataToBeImputed, int1, by=keys_lower, all.x= TRUE)
  timeSeriesDataToBeImputed %>% filter(is.na(Protected))
  
  timeSeriesDataToBeImputed[is.na(Protected) & value_measuredelement_5016a>=0,flagcombination:= flagcombinationa,]
  timeSeriesDataToBeImputed[is.na(Protected) & value_measuredelement_5016a>=0,loss_per_clean:= loss_per_cleana,]
  timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagobservationstatus := 'I',] 
  timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagmethod:= 'e',]
  timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagcombination := paste(flagobservationstatus,flagmethod, sep=";"),]
  timeSeriesDataToBeImputed[,(nameadd):= NULL,]
  
  # Multiplies loss percentages by production
  timeSeriesDataToBeImputed <- merge(timeSeriesDataToBeImputed,prod_imports, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
  timeSeriesDataToBeImputed[,value_measuredelement_5126 := loss_per_clean,]
  timeSeriesDataToBeImputed[,value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510,]
  timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed %>% filter(!is.na(value_measuredelement_5016))
  datasetN[datasetN=="loss_per_clean"] <- "value_measuredelement_5126"
  
  
  ### Splits the data tables for the SWS ####
  timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5016","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5016[, measuredElement := "5016"]
  setnames(timeSeriesDataToBeImputed_5016, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"  ,"Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5016, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed_5016 %>% filter(!is.na(flagMethod))
  
  ##---------------------
  timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5126","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5126[, measuredElement := "5126"]
  setnames(timeSeriesDataToBeImputed_5126, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5126, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed_5126 %>% filter(!is.na(flagMethod))
  
  DataSave <- rbind(timeSeriesDataToBeImputed_5016,timeSeriesDataToBeImputed_5126)

  # Save to the SWS
  stats = SaveData(domain = "lossWaste",
                   dataset="loss",
                   data = DataSave
  )
  
  
  sprintf(
    "Module completed in %1.2f minutes.
    Values inserted: %s
    appended: %s
    ignored: %s
    discarded: %s",
    difftime(Sys.time(), startTime, units = "min"),
    stats[["inserted"]],
    stats[["appended"]],
    stats[["ignored"]],
    stats[["discarded"]]
  )
  
  
}


