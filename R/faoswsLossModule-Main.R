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

############# Computation Parameters #####################################
## Options for the user - See full documentation for the User Oriented Work Flow 
updateModel <- 1
#For the model - using more than the SWS loss % for the 
SubNationalEstimates <- 1

# selecting data collection methods for aggregating the subnational estimates 
DataCollectionTags_all <- c("SWS","APHLIS","Rapid Assessment","Expert Opinion",
  "Laboratory Trials","Field Trial","Survey","Declarative","Crop-Cutting","Case study")
DataCollectionTags_represent <- c("Expert Opinion","Survey","Declarative")
ExternalDataOpt <- DataCollectionTags_all

# For aggregating the subnational using the markov function
MarkovOpt <- "aveatFSP"  # "model"

## Year should be a paramameter selected.
## selectedYear = as.character(1961:2015)
selectedYear = as.character(1991:2015)
selectedModelYear = as.character(1961:2015)

HierarchicalCluster <- "foodGroupName" # "isocode", "SDG.Regions"
  
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

##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
#CountryGroup <- as.data.table(read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep='')))
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
CountryGroup$Country <- tolower(CountryGroup$countryname)
CountryGroup[,"geographicAreaM49":=CountryGroup$m49code]


#FAOCrops <- as.data.table(read.csv(paste(githubsite, 'General/Cpc.csv', sep=''))) ## All Crops in the CPC system
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
FAOCrops[, "Crop" := FAOCrops$description]
FAOCrops[, "measuredItemCPC" := addHeadingsCPC(FAOCrops$cpc)]

#####FAO SWS Datasets#####
load(paste(githubsite, 'General/fbsTree.RData',sep=""))
#fbsTree <- ReadDatatable("fbsTree") &&
names(fbsTree) <- c("fbsID4","measuredItemCPC", "fbsID1","fbsID2","fbsID3")

#####  Runs the model and collects the needed data  #####

if(updateModel){
  finalModelData = 
  {
    ## requiredItems <<- getRequiredItems()
    production <- getProductionData() # Value_measuredElement_5510
    lossProtected <- getLossData(protected = TRUE)     # Value_measuredElement_5016
    #Data for the model
    lossData <-  merge(production,lossProtected,  by.x = keys,  by.y = keys, all.y= TRUE)
    lossData[, Loss_Per_clean := 100*(Value_measuredElement_5016/Value_measuredElement_5510)]
    lossData[, FSC_Location := "SWS"]
    lossData <- lossData %>% filter(!Loss_Per_clean > 100)
    
    # creating time series:
    timeSeriesData <- as.data.table(expand.grid(timePointYears = sort(unique(lossData$timePointYears)),
                                                geographicAreaM49 = as.numeric(unique(lossData$geographicAreaM49)),
                                                measuredItemCPC = as.character(unique(lossData$measuredItemCPC))))
    
    # Take the Data to be imputed
    timeSeriesDataToBeImputed <- merge(timeSeriesData, lossData, by = keys, all.x = T)
    timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed[is.na(Value_measuredElement_5016)]
    timeSeriesDataToBeImputed[, Loss_Per_clean := 0]
  
  }  %>%
   
    ## Convert variables to factor
    .[, `:=`(c("geographicAreaM49",
               "measuredItemCPC"),
             lapply(c("geographicAreaM49",
                      "measuredItemCPC"),
                    FUN = function(x) as.factor(.SD[[x]])
             )
    )
    ]
  
  lossData <- join(lossData,CountryGroup[,c("isocode","geographicAreaM49", "Country")],  by = c("geographicAreaM49"),type= 'left', match='all')
  lossData <- join(lossData,FAOCrops[,c("measuredItemCPC","Crop")],  by = c("measuredItemCPC"),type= 'left', match='all')
  lossData <- lossData[,c("geographicAreaM49","isocode","timePointYears","Country","measuredItemCPC","Crop","Loss_Per_clean","FSC_Location")]
  
  ########### Loss Factor Data and Aggregation ################### 
  ## This section imports the data of the loss factors and then merges it with the country designations for the SDG 
  if(SubNationalEstimates){
       # brings in the current file of converstion factors 
       #ConvFactor1 <- read.csv(paste(githubsite, 'General/FLW_LossPercFactors.csv', sep=''))
       ConvFactor1 <- ReadDatatable('flw_lossperfactors')
       ConvFactor1  <- join(ConvFactor1,CountryGroup[,c('isocode',"geographicAreaM49")],  by = c('isocode'),type= 'left', match='all')
       ConvFactor1  <- ConvFactor1 %>% filter(tag_datacollection %in%  ExternalDataOpt)
       
       ## Runs the Markov Model to standardize estimates 
       markov <- FSC_Markov(ConvFactor1,MarkovOpt)
       markov <- markov[na.omit(markov$loss_per_clean),]
       names(markov) <- c("geographicAreaM49","isocode","timePointYears","Country","measuredItemCPC","Crop","Loss_Per_clean","FSC_Location")
  
    FullSet <- rbind(markov,lossData)
  }else{FullSet <- lossData}
    
  
  ########### Variables for the module  ###################   
  # Adds the variables to the dataset from the APIs
  print("If you need to update the World Bank data tables, uncomment the line below ")
  #VariablesAdd(FullSet)
  
  # Adds the explanatory Varaibles 
  Data_Use_train <- VariablesAdd2(FullSet)
  lagyr <- c("lag1yr","lag2yr","lag3yr")
  Data_Use_train[,lag1yr := NULL ]
  Data_Use_train[,lag2yr := NULL ]
  Data_Use_train[,lag3yr := NULL ]
  
  print("number of unique country and crop combinations: ")
  print(length(unique(interaction(Data_Use_train$isocode, Data_Use_train$measuredItemCPC,sep = ";"))))
  
  #LossFactor_Predict <-VariablesAdd(LossFactorSet2)
  Data_Use_Predict <- VariablesAdd2(LossFactorSet2)
  Data_Use_Predict[,lag1yr := NULL ]
  Data_Use_Predict[,lag2yr := NULL ]
  Data_Use_Predict[,lag3yr := NULL ]
  
  print("number of unique country and crop combinations: ")
  print(length(unique(interaction(Data_Use_Predict$isocode, Data_Use_Predict$measuredItemCPC,sep = ";"))))

  data_act <- as.data.frame(Data_Use_train[Data_Use_train$SDG.Regions.x == "Latin America and the Caribbean (MDG=M49)",])
  datacrop <-  as.data.frame(Data_Use_train)
  colnames(Data_Use_train) <- gsub("[[:punct:]]","_",colnames(Data_Use_train)) 

  
  ####### Model Estimation ############
  
  KeepVar <- c("ID",'Country','isocode','M49Code',"Crop",'Year','SDG_Regions',"measuredItemCPC",'Loss_Per_clean',
               'FSC_Location',flag)
  
  DataPred <- LossModel(Data= Data_Use_train,DataPred=finalModelData,flag = "foodGroupName")
  
  
  
}  
