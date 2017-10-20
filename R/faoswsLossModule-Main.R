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

library(jsonlite)
library(XML)
library(httr)
library(stats4)
library(plm)
library(gtools)
library(ggplot2)
library(data.table)
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




##########################################################
## For Local 
## SWS Connection
githubsite <- '~/SWSLossModule/raw-data/'
dirmain <- 'T:\\Team_working_folder\\A\\GFLI\\2_Estimation-Methods\\SWS_LossModule'
SetClientFiles(dir = "C:\\Users\\ENGLISHA\\Documents\\certificates\\qa")
files = dir("~/Github/faoswsLoss/R",
            full.names = TRUE) 

token = "84fdac88-f975-4f81-95a1-7dd3cbfdedc5" #Production 2004-06
token2 = '72832c23-6650-4454-ac7e-a2d1d926353a' #Loss Data


GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token
)  




  
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

## Year should be a paramameter selected.
## selectedYear = as.character(1961:2015)
selectedYear = as.character(1991:2015)
selectedModelYear = as.character(1961:2015)

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"

##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
CountryGroup <- read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep=''))
CountryGroup$CountryName <- tolower(CountryGroup$CountryName)

FAOCrops <- read.csv(paste(githubsite, 'General/Cpc.csv', sep='')) ## All Crops in the CPC system
load(paste(githubsite, 'General/fbsTree.RData',sep=""))



#### SWS Data #####
## There are two datasets from the SWS,which is loading the protected data for the Loss Training data
## and the second is used for the Index 

load(paste(githubsite, 'General/finalModelData.RData',sep=""))# Protected data from the SWS 
lossData <- finalModelData
#lossData = nameData("agriculture", "aproduction", lossData)
lossData[, percent := Value_measuredElement_5016/(Value_measuredElement_5510)]
#lossData[geographicAreaM49 == 840 & measuredItemCPC_description == "Bananas"]

lossData[, timePointYears := as.numeric(timePointYears)]
lossData[order(-timePointYears, geographicAreaM49, measuredItemCPC), ]
FAOCrops$cpc <- addHeadingsCPC(FAOCrops$cpc) 

lossData$geographicAreaM49 <- as.factor(lossData$geographicAreaM49)
CountryGroup$M49Code <- as.factor(CountryGroup$M49Code)
lossData <- merge(lossData,CountryGroup[,c('ISOCode','M49Code','CountryName')],  by.x = c('geographicAreaM49'),  by.y = c('M49Code'), all.x= TRUE)
SWS__data <- lossData 
SWS__data$ISOCode <- lossData$ISOCode
SWS__data$Year <- lossData$timePointYears
SWS__data$Country <- lossData$CountryName
SWS__data$measuredItemCPC <- lossData$measuredItemCPC

SWS__data$Loss_Per_clean  <- (SWS__data$Value_measuredElement_5016/SWS__data$Value_measuredElement_5510)*100
SWS__data$FSC_Location  <- "SWS"
SWS__data$ID <-  paste(SWS__data$ISOCode,SWS__data$measuredItemCPC,SWS__data$Year,sep = ";")
SWS__data$M49Code <- lossData$geographicAreaM49

SWS__data <- merge(SWS__data, unique(FAOCrops[,c('cpc', 'description')]),  by.x = c('measuredItemCPC'),
                   by.y = c('cpc'), all.x = TRUE)
SWS__data$Crop <-SWS__data$description

SWS__data <- SWS__data[,c("ISOCode","Year","Country","measuredItemCPC","Crop","Loss_Per_clean","FSC_Location","ID","M49Code")]

### SWS - set 2 ###


ProdQtySWS <- GetData(swsContext.datasets[[1]])
ProdQtySWS$geographicAreaM49 <- as.integer(ProdQtySWS$geographicAreaM49)
CountryGroup$M49Code2 <- as.integer(CountryGroup$M49Code)


ProdQtySWS <- merge(ProdQtySWS, CountryGroup[, c('M49Code2', 'ISOCode', 'SDG.Regions', 'M49.Level.2.Region')], by.x = c('geographicAreaM49'),
                    by.y = c('M49Code2'), all.x = TRUE, all.y = FALSE)

ProdQtySWS  <- merge(ProdQtySWS, fbsTree, by.x = c('measuredItemCPC'),
                     by.y = c('measuredItemCPC'), all.x = TRUE, all.y = FALSE)

GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token2
)  

lossSWS <- GetData(swsContext.datasets[[1]])
lossSWS$geographicAreaM49 <- as.integer(lossSWS$geographicAreaM49)

lossSWS <- merge(lossSWS, CountryGroup[, c('M49Code2', 'ISOCode', 'SDG.Regions', 'M49.Level.2.Region')], by.x = c('geographicAreaM49'),
                 by.y = c('M49Code2'), all.x = TRUE, all.y = FALSE)

lossSWS  <- merge(lossSWS, fbsTree, by.x = c('measuredItemCPC'),
                  by.y = c('measuredItemCPC'), all.x = TRUE, all.y = FALSE)

### Creates the needed dataset
if(updateModel){
  finalModelData = 
  {
    ## requiredItems <<- getRequiredItems()
    production <<- getProductionData() # Value_measuredElement_5510
    lossProtected <<- getLossData(protected = TRUE)     # Value_measuredElement_5016
  
    # creating time series:
    timeSeriesData <- as.data.table(expand.grid(timePointYears = sort(unique(loss$timePointYears)),
                                                geographicAreaM49 = as.numeric(unique(loss$geographicAreaM49)),
                                                measuredItemCPC = as.character(unique(loss$measuredItemCPC))))
    
    # tirar dados que sao oficiais
    keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    timeSeriesDataToBeImputed <- merge(timeSeriesData, lossProtected, by = keys, all.x = T)
    timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed[is.na(Value_measuredElement_5016)]
    
    # openingStock <<- getOpeningStockData()     # Value_measuredElement_5113
    ## lossFoodGroup <<- getLossFoodGroup()
    assign("lossFoodGroup", getLossFoodGroup(), envir = .GlobalEnv)
  }  %>%
    mergeAllLossData(lossData = lossProtected, production, lossFoodGroup) %>%
    subset(x = .,
           ## only use observations where:
           ## a) production is zero and imports are positive
           ## b) production is positive and imports are zero or positive
           ## returns around 700 obs
           subset = ((Value_measuredElement_5510 == 0 &
                        Value_measuredElementTrade_5610 > 0) |
                       (Value_measuredElement_5510 > 0 &
                          Value_measuredElementTrade_5610 >= 0)),
           select = c("geographicAreaM49", 
                      "measuredItemCPC", 
                      "timePointYears",
                      "Value_measuredElement_5016", # loss
                      "Value_measuredElement_5510", # production
                      "foodGroupName",
                      )) %>%
    ## another filter: returns around 280 obs
    removeCarryLoss(data = ., lossVar = "Value_measuredElement_5016") %>%
    ## Convert variables to factor
    .[, `:=`(c("geographicAreaM49",
               "measuredItemCPC", 
               "foodGroupName", 
               "foodPerishableGroup"),
             lapply(c("geographicAreaM49",
                      "measuredItemCPC", 
                      "foodGroupName"),
                    FUN = function(x) as.factor(.SD[[x]])
             )
    )
    ]
  
  
  ########### Loss Factor Data and Country Designation   ################### 
  ## This section imports the data of the loss factors and then merges it with the country designations for the SDG 
  
  # brings in the current file of converstion factors collected, Makes the country lower case for eas of merging later.  
  ConvFactor1 <- read.csv(paste(githubsite, 'General/FLW_LossPercFactors.csv', sep=''))
  
  # Runs the Markov Model to standardize estimates 
  markov <- FSC_Markov(ConvFactor1[ConvFactor1$Reference != "SWS",],"aveatFSP")
  markov2 <- join(markov,CountryGroup[,c('ISOCode','M49Code')],  by = c('ISOCode'),type= 'left', match='all')
  markov2 <- markov2[na.omit(markov2$M49Code),]
  
  
  
  dim(markov)
  mean(na.omit(markov$Loss_Per_clean))
  names(markov2)
  
  FullSet <- rbind(markov2,  SWS__data)
  # Create the set that needs to have predictions
  LossFactorSet2 <- PredictiveSet(FullSet,"SWS")
  
  # Adds the variables to the dataset from the APIs
  print("If you need to update the World Bank data tables, uncomment the line below ")
  #VariablesAdd(FullSet)
  
  
  # since there was discussion on production or production +imports
  #LossFactor_train_SWSProdOnly # With production only 
  #LossFactor_train <- LossFactor_train_SWSProdOnly 
  
  Data_Use_train <- VariablesAdd2(FullSet)
  
  Data_Use_train[,lag1yr := NULL ]
  Data_Use_train[,lag2yr := NULL ]
  Data_Use_train[,lag3yr := NULL ]
  
  print("number of unique country and crop combinations: ")
  print(length(unique(interaction(Data_Use_train$ISOCode, Data_Use_train$measuredItemCPC,sep = ";"))))
  
  #LossFactor_Predict <-VariablesAdd(LossFactorSet2)
  Data_Use_Predict <- VariablesAdd2(LossFactorSet2)
  Data_Use_Predict[,lag1yr := NULL ]
  Data_Use_Predict[,lag2yr := NULL ]
  Data_Use_Predict[,lag3yr := NULL ]
  
  print("number of unique country and crop combinations: ")
  print(length(unique(interaction(Data_Use_Predict$ISOCode, Data_Use_Predict$measuredItemCPC,sep = ";"))))

  data_act <- as.data.frame(Data_Use_train[Data_Use_train$SDG.Regions.x == "Latin America and the Caribbean (MDG=M49)",])
  datacrop <-  as.data.frame(Data_Use_train)
  colnames(Data_Use_train) <- gsub("[[:punct:]]","_",colnames(Data_Use_train)) 

  
  ####### Model Estimation ############
  
  KeepVar <- c("ID",'Country','ISOCode','M49Code',"Crop",'Year','SDG_Regions',"measuredItemCPC",'Loss_Per_clean',
               'FSC_Location',flag)
  
  DataPred <- LossModel(Data= Data_Use_train,DataPred=finalModelData,flag = "foodGroupName")
  
  
  
}  