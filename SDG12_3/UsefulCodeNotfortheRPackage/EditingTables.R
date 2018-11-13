#' Module for estimating Food Loss and Waste 
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English
#' 
#' 
#  This function updates the tables 


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

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#####  Collects the data from the SWS #####
ConvFactor1  <- ReadDatatable('flw_lossperfactors_')
unique(ConvFactor1$tag_datacollection)

ConvFactor1[tag_datacollection == "LItReview","tag_datacollection",with=F]
ConvFactor1[tag_datacollection == "LItReview","tag_datacollection"] <- "LitReview"
ConvFactor1[tag_datacollection == "LItREview","tag_datacollection"] <- "LitReview"
ConvFactor1[tag_datacollection == "LitREview","tag_datacollection"] <- "LitReview"
ConvFactor1[tag_datacollection == "litReview","tag_datacollection"] <- "LitReview"
ConvFactor1[tag_datacollection == "Litreview","tag_datacollection"] <- "LitReview"
ConvFactor1[tag_datacollection == "field trial","tag_datacollection"] <- "Field Trial"
ConvFactor1[tag_datacollection == "expert opinion","tag_datacollection"] <- "Expert Opinion"


unique(ConvFactor1[,"fsc_location",with=F])
ConvFactor1[,"fsc_location"] <- lapply(ConvFactor1[,"fsc_location",with=F], function(x) gsub("^\\s+|\\s+$", "", x))
ConvFactor1[fsc_location %in% c("farm","On-farm") ,"fsc_location"] <-"Farm"
ConvFactor1[fsc_location %in% c( "Farm, Wholesale Retail") ,"fsc_location"] <-"Farm/Wholesale/Retail"
ConvFactor1[fsc_location %in% c( "marketing chain") ,"fsc_location"] <-"WholeSupplyChain"
ConvFactor1[fsc_location %in% c( "storage"),"fsc_location"] <-"Storage"


unique(ConvFactor1[,"activity",with=F])
ConvFactor1[,"activity"] <- lapply(ConvFactor1[,"activity",with=F], function(x) gsub("^\\s+|\\s+$", "", x))
ConvFactor1[,"activity"] <- lapply(ConvFactor1[,"activity",with=F], function(x) gsub("\\n", " " , x))
ConvFactor1[,"activity"] <- lapply(ConvFactor1[,"activity",with=F], function(x) gsub("\\s+and+\\s", " & " , x))

unique(ConvFactor1[,"causeofloss",with=F])
ConvFactor1[,"causeofloss"] <- lapply(ConvFactor1[,"causeofloss",with=F], function(x) gsub("^\\s+|\\s+$", "", x))


### finding CPC Numbers
ConvFactor1[,"crop"] <- lapply(ConvFactor1[,"crop",with=F], function(x) gsub("^\\s+|\\s+$", "", x))
ConvFactor2 <- merge(ConvFactor1,gfli_basket, by=c("measureditemcpc"), all.x=T)
r <-unique(ConvFactor2[is.na(gfli_basket),c("measureditemcpc","crop"),with=F])
FAOCrops2 <- merge(unique(FAOCrops[,c("measureditemcpc","crop"),with=F]),gfli_basket, by=c("measureditemcpc"), all.x=T)

#ConvFactor1$measureditemcpc <- addHeadingsCPC(ConvFactor1$measureditemcpc)

ConvFactor1 <- 
ConvFactor1[!is.na(geographicaream49),]
unique(ConvFactor1$crop)
ConvFactor1[crop %in% "Eggs", "measureditemcpc"]<-"0231"
ConvFactor1[crop %in% grep("Egg", unique(ConvFactor1$crop), value = T)[1:2], "measureditemcpc"] <-"0231"

ConvFactor1[crop %in% grep("Egg", unique(ConvFactor1$crop), value = T)[1:2], "measureditemcpc", with=F]

ConvFactor1[crop %in% r$Crop[i], "measureditemcpc", with=F] 
ConvFactor1[crop %in% ("Paddy Rice"), "measureditemcpc"] <- "0113"
ConvFactor1[crop %in% ("Other Cereals N.E."), "measureditemcpc"] <- "01199.90"
ConvFactor1[crop %in% "Beans, Green", "measureditemcpc"] <- "01243"
ConvFactor1[crop %in% "Garlic", "measureditemcpc"] <- "01252"
ConvFactor1[crop %in% "Onion", "measureditemcpc"] <- "01253.01"
ConvFactor1[crop %in% "Mushroom", "measureditemcpc"] <- "01270"
ConvFactor1[crop %in% "Cherries", "measureditemcpc"] <- "01344.02"
ConvFactor1[crop %in% "Fruit", "measureditemcpc"] <- "01359.90"
ConvFactor1[crop %in% "Cassava", "measureditemcpc"] <- "01520.01"
ConvFactor1[crop %in% "Coffee, Green", "measureditemcpc"] <- "01610"
ConvFactor1[crop %in% "Cocoa Beans", "measureditemcpc"] <- "01640"
ConvFactor1[crop %in% "cocoa beans", "measureditemcpc"] <- "01640"
ConvFactor1[crop %in% "Cattle", "measureditemcpc"] <- "02111"
ConvFactor1[crop %in% "Poultry Meat", "measureditemcpc"] <- "F1061"
ConvFactor1[crop %in% "Sheep", "measureditemcpc"] <- "02122"
ConvFactor1[crop %in% "Goats", "measureditemcpc"] <- "02123"
ConvFactor1[crop %in% "Teff", "measureditemcpc"] <- "01199.01"
ConvFactor1[crop %in% "Okra", "measureditemcpc"] <- "01239.01"
ConvFactor1[crop %in% "Vegetables", "measureditemcpc"] <- "01290.90"
ConvFactor1[crop %in% "Pears", "measureditemcpc"] <- "01342.01"
ConvFactor1[crop %in% "Quinces", "measureditemcpc"] <- "01342.02"
ConvFactor1[crop %in% "Currants", "measureditemcpc"] <- "01351.01"
ConvFactor1[crop %in% "Raspeberries", "measureditemcpc"] <- "01353.01"
ConvFactor1[crop %in% "Blackberry", "measureditemcpc"] <- "01355.90"
ConvFactor1[crop %in%  "blueberries", "measureditemcpc"] <- "01355.01"
ConvFactor1[crop %in% "Blueberries", "measureditemcpc",with=F] <- "01355.01"
ConvFactor1[crop %in% "Persimmon", "measureditemcpc"] <- "01359.01"
ConvFactor1[crop %in% "Oil Palm Fruit", "measureditemcpc"] <- "01491.01"
ConvFactor1[crop %in% "Cassava, Fresh", "measureditemcpc"] <- "01520.01"
ConvFactor1[crop %in% "Cassava, Dried Chips", "measureditemcpc"] <- "01520.02"
ConvFactor1[crop %in% "Lupins", "measureditemcpc"] <- "01709.02"
ConvFactor1[crop %in% "Peanuts/Groundnut", "measureditemcpc"] <- "0142"
ConvFactor1[crop %in% "Peanut", "measureditemcpc"] <- "0142"
ConvFactor1[crop %in% "Pulses N.E.", "measureditemcpc"] <- "01709.90"
ConvFactor1[crop %in% "Milk", "measureditemcpc"] <- "02211"
ConvFactor1[crop %in% "Sweet Cherry", "measureditemcpc"] <- "01344.02"
ConvFactor1[crop %in% "Onions And Shallots, Dry (Excluding Dehydrated)", "measureditemcpc"] <- "01253.02"
ConvFactor1[crop %in% "Green Corn (Maize)", "measureditemcpc"] <- "01290.01"
ConvFactor1[crop %in% "Other Vegetables, Fresh N.E.", "measureditemcpc"] <- "01290.90"
ConvFactor1[crop %in% "Cranberries And Other Fruits Of The Genus Vaccinium", "measureditemcpc"] <- "01355.90"
ConvFactor1[crop %in% "Other Berries And Fruits Of The Genus Vaccinium N.E.", "measureditemcpc"] <- "01355.90"
ConvFactor1[crop %in% "Other Nuts (Excluding Wild Edible Nuts And Groundnuts), In Shell, N.E.", "measureditemcpc"] <- "01379.90"
ConvFactor1[crop %in% "Pulses", "measureditemcpc"] <- "01709.90"
ConvFactor1[crop %in% "Milk, Cow", "measureditemcpc"] <- "02211" 
ConvFactor1[crop %in% "Other Beans, Green", "measureditemcpc"] <- "01241.90" 
ConvFactor1[crop %in% "Oriental Bunching Onion", "measureditemcpc"] <- "01253.01" 
ConvFactor1[crop %in% "Vegetables, Fresh", "measureditemcpc"] <- "01290.90" 
ConvFactor1[crop %in% "Apricot", "measureditemcpc"] <- "01343"
ConvFactor1[crop %in% "Common Beans", "measureditemcpc"] <- "01701"
  
i=22
grep(tolower(r$Crop[i]),FAOCrops2$crop, value=T)
FAOCrops2[crop %in% grep(tolower(r$Crop[i]),FAOCrops2$crop, value=T),]
r$Crop[i]



names(ConvFactor1) <- tolower(names(ConvFactor1))
## Delete
table = "flw_lossperfactors_"
changeset <- Changeset(table)
newdat <- ReadDatatable(table, readOnly = FALSE)
AddDeletions(changeset, newdat)
Finalise(changeset)
## Add
AddInsertions(changeset,ConvFactor1)
Finalise(changeset)


##########################################################
gfli_basket  <- ReadDatatable('gfli_basket')
fbsTree      <- ReadDatatable("fbs_tree")

names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"

fbsTree$GFLI_Basket <- 'NA'
fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals',]
fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Meat & Animals Products',] # |foodGroupName == "PRODUCTS FROM FISH",

fish<- as.data.table(c(2766,2765,2762,2761,2764,2767, 2763,2960,2769,2775,2768,2961))
names(fish)[names(fish) == 'V1'] <- "measureditemcpc" 
fish$GFLI_Basket <- 'Fish & Fish Products'
fbsTree <- rbind(fbsTree,fish, fill=T)
fbsTree[GFLI_Basket == "NA", 'GFLI_Basket'] <- NA

gfli_basket <- fbsTree[,c("foodgroupname","measureditemcpc", "GFLI_Basket"),with=F]

names(gfli_basket) <- tolower(names(gfli_basket))
## Delete
table = 'gfli_basket'
changeset <- Changeset(table)
newdat <- ReadDatatable(table, readOnly = FALSE)
AddDeletions(changeset, newdat)
Finalise(changeset)
## Add
AddInsertions(changeset,gfli_basket)
Finalise(changeset)



