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


library(XML)
library(httr)
library(stats4)
library(plm)
library(gtools)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(rpart)
library(gridExtra)
library(scales)
library(reshape)
library(devtools)
library(jsonlite)
library(readr)

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




##################### For deletion #####################################
## For Local 
## SWS Connection

githubsite <- '~/faoswsLoss/data-raw/'
dirmain <-  '~/faoswsLoss'
SetClientFiles(dir = "C:\\Users\\ENGLISHA\\Documents\\certificates\\qa")
files = dir("~/Github/faoswsLoss/R",
            full.names = TRUE) 


token4 = "4ebe38a7-495e-4c62-bcc4-b6193a394eec" # saved Loss % data

GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token4
)  

############# Computation Parameters #####################################
## Options for the user - See full documentation for the User Oriented Work Flow 
updateModel <- 1
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
graphLoss <- 1
##########################################################


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

#####  Runs the model and collects the needed data  #####

if(updateModel){
  finalModelData = 
  {
    ## requiredItems <<- getRequiredItems()
    production <- getProductionData() # Value_measuredElement_5510
    
    #lossDataAll <-getLossData() 
    lossProtected <- getLossData(protected = TRUE)     # Value_measuredElement_5016
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
    lossData <- lossData %>% filter(loss_per_clean < 50)
    #highLosses <- lossData %>% filter(loss_per_clean > 50)
    #date = "_18Dec17"
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
  }else{FullSet <- lossData}
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
    AddInsertions(changeset,  FullSet[,c("geographicaream49","timepointyears","measureditemcpc","isocode","country","crop","loss_per_clean","fsc_location"),])
    Finalise(changeset)
  
  
  ########### Variables for the module  ###################   
  # Adds the explanatory Varaibles,
  Predvar <- c()
  Data_Use_train <- VariablesAdd1(FullSet,keys_lower,Predvar)

  ####### Model Estimation ############
  
  #KeepVar <- c(keys,'isocode','SDG_Regions',"measuredItemCPC",
  #             'FSC_Location',HierarchicalCluster)
  
  timeSeriesDataToBeImputed2 <- LossModel(Data= Data_Use_train,timeSeriesDataToBeImputed, HierarchicalCluster,keys_lower)
                                            
 
}  
if(ExistModel){

  }

if(graphLoss){
  
dlpath <- file.path(dirmain,'plots')
pdffile <- file.path(dlpath, paste("Commodities_",as.character(Sys.Date()),".pdf", sep=""))
lossProtected <- getLossData(protected = TRUE)     # Value_measuredElement_5016
lossProtected <- DataPred %>% filter(loss_per_clean >0)
names(lossProtected) <- tolower(names(lossProtected))
pdf(file = pdffile, height = 11, width = 16)


for(j in 1:length(unique(lossProtected[,tolower(itemVar),with=F]))){
  for(i in 1:length(unique(lossProtected$areaVar))){
    ctry = unique(lossProtected$areaVar)[i]
    crp = unique(lossProtected$itemVar)[j]
    lossProtected[,flagplot:=0,with=T]
    lossProtected [itemVar ==  crp & areaVar == ctry,flagplot:=1,with=T]
    
    tmp <- lossProtected %>% filter(flagplot ==1)
    if(dim(tmp)[1] > 3){
      p = ggplot() + 
        geom_point(data= lossProtected[itemVar ==  crp,,], aes(x = timepointyears, y = value_measuredelement_5126, color = flagobservationstatus_measuredelement_5016 ))+
        geom_line(data= tmp[flagplot == 1  & areaVar ==ctry ,,], aes(x = timepointyears, y =value_measuredelement_5126, color = flagobservationstatus_measuredelement_5016 ), size =2)+
        xlab('timePointYears') + ylab('Loss (%)') +
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold")) +
        scale_y_continuous(labels = percent)+
        scale_x_continuous(limits = c(2000, 2014), breaks = seq(2000, 2014, 2)) +
        ggtitle(paste(unique(tmp[flagplot == 1 ,,]$foodgroupname),unique(tmp[flagplot == 1 ,,]$itemVar), sep = ", "))
      print(p)
    }else{next}    
    
  }}

dev.off()
}
