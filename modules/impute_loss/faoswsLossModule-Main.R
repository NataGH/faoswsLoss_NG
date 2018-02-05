#' Module for estimating Food Loss and Waste 
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English Marco Mingione 
#' 
#' 
######### Load all libraries ###########
#install.packages('faosws')
#install_github(repo = "SWS-Methodology/faoswsFlag")
#install_github(repo = "SWS-Methodology/faoswsUtil")
#install.packages("faoswsLoss", repo = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/") 
#install.packages("faoswsModules", repos = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/")




library(stats4)
library(plm)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(rpart)
library(scales)
library(reshape)
#library(devtools)
library(lmtest)
library(readr)
library(Hmisc)


library(magrittr) 
remove.packages(pkgs, lib, version)

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
    library(reshape2)
    library(plyr)
    library(dplyr)
    
  })


############# Computation Parameters #####################################
## Options for the user - See full documentation for the User Oriented Work Flow 
updateModel <- TRUE
LocalRun <- FALSE

#For the model - using more than the SWS loss % for the 
SubNationalEstimates <- 1

# selecting data collection methods for aggregating the subnational estimates 
DataCollectionTags_all <- c("SWS","APHLIS","Rapid Assessment","Expert Opinion",
  "Laboratory Trials","Field Trial","Survey","Declarative","Crop-Cutting","Case study")
DataCollectionTags_represent <- c("SWS","APHLIS","Expert Opinion","Survey","Declarative")
ExternalDataOpt <- DataCollectionTags_represent

# For aggregating the subnational using the markov function
MarkovOpt <- "aveatFSP"  # "model"

## Year should be a paramameter selected.
## selectedYear = as.character(1961:2015)
selectedYear = as.character(1991:2015)
selectedModelYear = as.character(1961:2015)

HierarchicalCluster <- "foodgroupname" # "isocode", "SDG.Regions"
VaribleSelection <- "RandomForest_geo"
graphLoss <- 0
##########################################################


if(CheckDebug()){
  message("Not on server, so setting up environment...")
  USER <- if_else(.Platform$OS.type == "unix",
                    Sys.getenv('USER'),
                    Sys.getenv('USERNAME'))


  library(faoswsModules)
  settings <- ReadSettings(file = file.path(paste(dirmain,"sws.yml", sep='/')))
  #SetClientFiles(settings[["certdir"]])

  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )

}




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
if(LocalRun ){
  CountryGroup <- as.data.table(read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep='')))
  FAOCrops <- as.data.table(read.csv(paste(githubsite, 'General/Cpc.csv', sep=''))) ## All Crops in the CPC system
  ConvFactor1 <- as.data.table(read.csv(paste(githubsite, 'General/FLW_LossPercFactors.csv', sep='')))
  names(CountryGroup) <- tolower(names(CountryGroup))
  names(FAOCrops) <- tolower(names(FAOCrops))
  names(ConvFactor1) <- tolower(names(ConvFactor1))
  ConvFactor1[,loss_per_clean := as.numeric(levels(loss_per_clean))[loss_per_clean]]
  }else{

CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
ConvFactor1 <- ReadDatatable('flw_lossperfactors')
  }

CountryGroup$country <- tolower(CountryGroup$countryname)
CountryGroup[,"geographicaream49":=CountryGroup$m49code]

FAOCrops[, "crop" := FAOCrops$description]
FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]


fbsTree <- ReadDatatable("fbs_tree")
names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals',]
fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",


#####  Runs the model and collects the needed data  #####
finalModelData = 
  {
    ## requiredItems <<- getRequiredItems()
    production <- getProductionData(areaVar,itemVar,yearVar,elementVar) # Value_measuredElement_5510
    
    #lossDataAll <-getLossData() 
    lossProtected <- getLossData(areaVar,itemVar,yearVar,elementVar,selectedYear,protected = TRUE)     # Value_measuredElement_5016
    names(lossProtected)[ names(lossProtected) == "Value"] <-  "value_measuredelement_5016"
    names(lossProtected)[ names(lossProtected) == "measuredItemSuaFbs"] <-  "measureditemcpc"
    names(lossProtected) <- tolower(names(lossProtected))
    names(production) <- tolower(names(production ))
    production$geographicaream49 <- as.character(production$geographicaream49)
    lossProtected$geographicaream49 <- as.character(lossProtected$geographicaream49)
    #Data for the model
    lossData <-  merge(production,lossProtected,  by.x = keys_lower,  by.y = keys_lower, all.y= TRUE)
    lossData[, loss_per_clean := 100*(value_measuredelement_5016/value_measuredelement_5510)]
    lossData[, fsc_location := "SWS"]
    lossData <- lossData %>% filter(!loss_per_clean > 100)
    names(lossData) <- tolower(names(lossData))
    lossData <- merge(lossData,CountryGroup[,c("isocode","geographicaream49", "country")],  by = c("geographicaream49"), all.x = TRUE, all.y = FALSE)
    #lossData <- merge(lossData,FAOCrops[,c("measureditemcpc","crop")],  by = c("measureditemcpc"), all.x = TRUE, all.y = FALSE)
    
    #write.table(highLosses,paste(githubsite, 'General/highLosses',date, '.csv', sep=''),sep=',' )
    
    # creating time series:
    timeSeriesData <- as.data.table(expand.grid(timepointyears = sort(unique(production$timepointyears)),
                                                geographicaream49 = as.character(unique(production$geographicaream49)),
                                                measureditemcpc = as.character(unique(production$measureditemcpc))))
    
    # Take the Data to be imputed
    timeSeriesDataToBeImputed <- merge(timeSeriesData, lossData,  by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
    timeSeriesDataToBeImputed[is.na(loss_per_clean), loss_per_clean := 0]
    timeSeriesDataToBeImputed$value_measuredelement_5016 = 0
    timeSeriesDataToBeImputed[,flagcombination:="0"]
    
    
    setnames(timeSeriesDataToBeImputed, old =  c("timepointyears","geographicaream49","measureditemcpc","isocode","country","loss_per_clean","fsc_location","flagobservationstatus.y","flagmethod.y","value_measuredelement_5016","flagcombination"),
             new =  c("timepointyears","geographicaream49","measureditemcpc","isocode","country","loss_per_clean","fsc_location","flagobservationstatus","flagmethod","value_measuredelement_5016","flagcombination") )
    
    timeSeriesDataToBeImputed <- subset(timeSeriesDataToBeImputed,
                                        select = c(keys_lower,"value_measuredelement_5016", "flagcombination","flagobservationstatus","flagmethod","loss_per_clean")
                                  )

    names(timeSeriesDataToBeImputed) <-tolower(names(timeSeriesDataToBeImputed))

    
    lossData <- subset(lossData, 
                       select = c(keys_lower,"isocode","country","loss_per_clean","fsc_location","flagobservationstatus.y", "flagmethod.y"))
    
    
} 
 
if(updateModel==1){  
  ########### Loss Factor Data and Aggregation ################### 
  ## This section imports the data of the loss factors and then merges it with the country designations for the SDG 
  if(SubNationalEstimates){
       # brings in the current file of converstion factors 

       ConvFactor1  <- join(ConvFactor1,CountryGroup[,c('isocode',"geographicaream49")],  by = c('isocode'),type= 'left', match='all')
       ConvFactor1  <- ConvFactor1 %>% filter(tag_datacollection %in%  ExternalDataOpt)
       ConvFactor1$measureditemcpc <- addHeadingsCPC(ConvFactor1$measureditemcpc)
       names(ConvFactor1)[names(ConvFactor1)=='year'] <-'timepointyears'
       
       ## Runs the Markov Model to standardize estimates 
       markov <- FSC_Markov(ConvFactor1,MarkovOpt)
       FullSet <- rbind(markov,lossData, fill=T)
       FullSet[,index :=rownames(FullSet)]
      
       duplicates <- FullSet %>%
         group_by(geographicaream49,timepointyears,measureditemcpc) %>%
         summarise(total.count=n())
       duplicates <- duplicates %>% filter(total.count>1)
       drop <- FullSet %>% filter(geographicaream49 %in% duplicates$geographicaream49 & timepointyears %in% duplicates$timepointyears
                          & measureditemcpc  %in% duplicates$measureditemcpc & fsc_location != "SWS")
       
       FullSet <- FullSet %>% filter(!index %in% drop$index)
       FullSet[,index :=NULL]
       }else{FullSet <- lossData}
  
 
  
  #write.table(FullSet,paste(githubsite, 'General/FullSet.csv', sep=''),sep=',' )
  ### Save the intermediate aggregation table  to the sws
  #<>
  names(FullSet) <- tolower(names(FullSet))
  ## Delete
  table = "aggregate_loss_table"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset,  FullSet[,c("geographicaream49","timepointyears","measureditemcpc","isocode","country","crop","loss_per_clean","fsc_location"),])
  Finalise(changeset)

  FullSet <- FullSet %>% filter(loss_per_clean <40)
  ########### Variables for the module  ###################   
  # Adds the explanatory Varaibles,
  Predvar <- c()
  Data_Use_train <- VariablesAdd1(FullSet,keys_lower,Predvar)

  ####### Model Estimation ############
  
  #KeepVar <- c(keys,'isocode','SDG_Regions',"measuredItemCPC",
  #             'FSC_Location',HierarchicalCluster)
  
  timeSeriesDataToBeImputed2 <- LossModel(Data= Data_Use_train,timeSeriesDataToBeImputed, production,HierarchicalCluster,keys_lower)
                                            
 
}  
if(updateModel ==0){
  ###Computation parameters##
  LastRun <- TRUE
  Cluster2Update <- na.omit(unique(fbsTree$GFLI_Basket))

  ## Read DataTables of the existing model runs 
  modelRuns <- ReadDatatable("lossmodelruns")
  
  Cluster2Update <- na.omit(unique(fbsTree$GFLI_Basket))[1]
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
  timeSeriesDataToBeImputed[Protected == TRUE,loss_per_clean:= loss_per_clean/100]
  
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
  timeSeriesDataToBeImputed <- merge(timeSeriesDataToBeImputed,production, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
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
  
  # Save to the SWS
  stats = SaveData(domain = "lossWaste",
                   dataset= "loss",
                   data   = timeSeriesDataToBeImputed_5016
  )
  
  stats = SaveData(domain= "lossWaste",
                   dataset="loss",
                   data=timeSeriesDataToBeImputed_5126
  )
  
}

if(graphLoss){

}
