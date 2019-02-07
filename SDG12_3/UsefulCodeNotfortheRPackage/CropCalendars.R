
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
DataCollectionTags_represent <- c("Expert Opinion","-","SWS","NationalStatsYearbook" 
                                  ,"NonProtected","Survey","NationalAcctSys"              
                                  ,"WRI Protocol","FBS/APQ","LitReview"                  
                                  ,"APHLIS","NP","Laboratory Trials","Modelled", "Census" )

# DataCollectionTags_represent <- c("-","APHLIS","Case Study","Census","Declarative","Expert Opinion",
#                                   "FBS/APQ","LitReview","Modelled","NationalAcctSys",
#                                   "NationalStatsYearbook","NonProtected","NP","Survey","SWS")
UB<- 0.65
LB <- 0.02
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
CropCalendar <- ReadDatatable("crop_calendar_nov17")
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops     <- ReadDatatable("fcl2cpc_ver_2_1")

CropCalendar$measureditemcpc <- addHeadingsCPC(CropCalendar$measureditemcpc)
ctry <- "cuba"
#grep(ctry, CropCalendar, perl=TRUE, value=TRUE)

if(savesws){
  table =  "crop_calendar_nov17"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  keepN <- names(newdat)[!names(newdat) %in% c("__id","__ts")]
  AddInsertions(changeset, CropCalendar )
  Finalise(changeset)
}
