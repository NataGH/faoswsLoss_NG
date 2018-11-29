#' Module for estimating Global Food Loss Index
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English 

######### Load all libraries ###########
#install.packages('faosws')
#install_github(repo = "SWS-Methodology/faoswsFlag")
#install_github(repo = "SWS-Methodology/faoswsUtil")
#install.packages("faoswsLoss", repo = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/") 

library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(openxlsx)
library(tidyr)
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
savesws <- TRUE
LocalRun <- TRUE # For if you are running the model on a local environment and loading data tables from local fiiles

if (!exists('selectedYear_start', inherits = FALSE)| !exists('selectedYear_end', inherits = FALSE)) {
  ## Year should be a paramameter selected.
  selectedYear_start <- swsContext.computationParams$selectedyear_start
  selectedYear_end <- swsContext.computationParams$selectedyear_end
  selectedYear = as.character(as.numeric(selectedYear_start):as.numeric(selectedYear_end))
}
if (!exists('aggregation', inherits = FALSE)) {
  # Options are: #  "iso2code","isocode","countryname","sdgregion_code","sdg_regions"           
  # "m49_level1_code","m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions"            
  # "ldcs_code","ldcs","lldcssids_code","lldcssids","Country","geographicaream49", "WORLD"   
  aggregation <- swsContext.computationParams$aggregation
}
if (!exists('weights', inherits = FALSE)) {
  # Options are: #"intl_prices"    
  weights <- swsContext.computationParams$weights
}
if (!exists('basketn', inherits = FALSE)) {
  # Options are: "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"  
  basketn<- swsContext.computationParams$basketn
}
if (!exists('ComparisonYear_start')| !exists('ComparisonYear_end')) {
  ## Year should be a paramameter selected.
  ComparisonYear_start <- swsContext.computationParams$ComparisonYear_start
  ComparisonYear_end <- swsContext.computationParams$ComparisonYear_end
  ComparisonYear = as.character(as.numeric(ComparisonYear_start):as.numeric(ComparisonYear_end))
  gfli_compare <- TRUE
}
if (!exists('ReportingYear', inherits = FALSE)) {
  # Options are: "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"  
  ReportingYear<- swsContext.computationParams$ReportingYear
  gfli_Reporting <- TRUE
}
if(LocalRun){
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

BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
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

#### Data In #####
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
#nutrient_table <- getNutrientFactors()

fbsTree <- ReadDatatable("fbs_tree")
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")

names(Losses) <- tolower(names(Losses))
names(production) <- tolower(names(production))
names(imports) <- tolower(names(imports))

production$geographicaream49 <- as.character(production$geographicaream49)
Losses$geographicaream49 <- as.character(Losses$geographicaream49)
production$timepointyears <- as.numeric(production$timepointyears)
imports$timepointyears<- as.numeric(imports$timepointyears)

prod_imports <- merge(production,imports, by= keys_lower, all.x = TRUE)
prod_imports[,prod_imports := rowSums(.SD, na.rm = TRUE), .SDcols=c("value.x","value.y")]

CountryGroup$country <- tolower(CountryGroup$m49_region)
CountryGroup[,"geographicaream49":=CountryGroup$m49_code]

FAOCrops[, "crop" := FAOCrops$description]
FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]

names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"

fbsTree$GFLI_Basket <- 'NA'
fbsTree[foodgroupname %in% c(2905,2911), GFLI_Basket :='Cereals & Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Meat & Animals Products',] 
fbsTree[GFLI_Basket == "NA", 'GFLI_Basket'] <- NA
names(fbsTree) <- tolower(names(fbsTree) )
## Fish Products ##
# This section will have to be added when Fisheries data is included in the SWS


names(Losses)[names(Losses) =="value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measureditemsuafbs"] <- "measureditemcpc"

# ProdQtySWS <- subset(prod_imports,
#                      select = c(keys_lower,"prod_imports")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])


ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- Base_Prod[timepointyears == as.numeric(BaseYear[2])-1,]
Base_Prod <- Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F]

#### Weights ####
if(weights == "intl_prices"){
  intPrice2005 <-  ReadDatatable("int_$_prices_2005")
  
  pvail <- unique(intPrice2005$itemcode)
  
  intPrice2005Selected <-
    intPrice2005 %>%
    select(itemcode,itemname, value) %>%
    filter(itemcode %in% as.numeric(unlist(na.omit(pvail))))
  
  intPrice2005Selected$intprice <- intPrice2005Selected$value
  #intPrice2005Selected$measureditemfclname <- intPrice2005Selected$ItemName
  
  intPrice2005Selected$measureditemfcl <- addHeadingsFCL(intPrice2005Selected$itemcode)
  intPrice2005Selected$measureditemcpc <- fcl2cpc(intPrice2005Selected$measureditemfcl,version = "2.1")
  intPrice2005Selected$itemname <- tolower(intPrice2005Selected$itemname)
  Weights <- intPrice2005Selected[, c("itemname","measureditemcpc","intprice"),with=F]
  
  #distinct(intPrice2005Selected,measuredItemCPC)
}else if(weights == "calories"){
  
  
}

## Production multiplied by the weighting scheme
FLIData <-  merge(Base_Prod,Weights, by = c('measureditemcpc'),all.x = F, all.y = F)
FLIData[, p0q0 := qty_avey1y2*intprice,]

## Loss percentages multiplied by the quantity weighted (for the numerator), Includes all commodities
DataForIndex <- merge(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , FLIData, by =c("measureditemcpc","geographicaream49"),type= 'left', match='all')
DataForIndex$l0ptqt =0
DataForIndex[,l0ptqt:=value_measuredelement_5126*p0q0,with=T]

DataForIndex <- join(DataForIndex ,fbsTree , by = c('measureditemcpc'),type= 'left', match='all')
DataForIndex <- join(DataForIndex,CountryGroup, by = c('geographicaream49'),type= 'left', match='all')

#### Basket ####
# Pulls in the existing table
BasketExist <- ReadDatatable("sdg123_commoditybasket")
BasketExist <- BasketExist[protected == T,]

if(basketn == "top2perhead_byCtry"){
  Top10perctry <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    arrange(geographicaream49, -p0q0) 
  
  Top10_VP <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(geographicaream49) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
  
  basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
  basket <- basket %>% filter(!is.na(gfli_basket))
  basketKeys <- c('geographicaream49', "measureditemcpc")
  ComBasketN  <- 'Production Value- Top 10 by country'
  basket[,basketname := ComBasketN]
  basket[,protected := FALSE] 
  
  basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
  basket[,Percent_prod := p0q0/All_p0q0]
}

if(basketn == "top2perhead_Globatop10"){
  Top10Global<-   DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(measureditemcpc,gfli_basket) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE)) %>%
    arrange(-All_p0q0)
  
  Top10_VP <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(geographicaream49) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
  
  ItemsBasket <- Top10Global[ ,head(.SD, 2), by= c('gfli_basket')]
  merge( ItemsBasket, FAOCrops[,c("measureditemcpc","crop")], by= c("measureditemcpc"), all.x = TRUE )
  basket <-   DataForIndex %>% filter(measureditemcpc %in%  unlist(ItemsBasket[!is.na(gfli_basket),measureditemcpc]))
  basketKeys <- ( "measureditemcpc")
  ComBasketN  <- 'Production Value- Top 10 by World'
  basket[,basketname := ComBasketN]
  basket[,protected := FALSE] 
  
  basket <- merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE)
  basket[,Percent_prod := p0q0/All_p0q0]
}
if(basketn == "top2_loss_byCtry"){
  Top10perctry <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    arrange(geographicaream49, -value_measuredelement_5126) 
  
  Top10_VP <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(geographicaream49) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
  
  basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
  basket[geographicaream49 == 100,]
  basket <- basket %>% filter(!is.na(gfli_basket))
  basketKeys <- c('geographicaream49', "measureditemcpc")
  ComBasketN  <- 'Loss Percents - Top 10 by country'
  basket[,basketname := ComBasketN]
  basket[,protected := FALSE] 
  
  basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
  basket[,Percent_prod := p0q0/All_p0q0]
}
if(basketn == "Global"){
  AllBasket <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    arrange(geographicaream49, -value_measuredelement_5126) 
  
  Top10_VP <- DataForIndex %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(geographicaream49) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
  
  basket <- AllBasket
  basket[geographicaream49 == 100,]
  basket <- basket %>% filter(!is.na(gfli_basket))
  basketKeys <- c('geographicaream49', "measureditemcpc")
  ComBasketN  <- 'Global Percents'
  basket[,basketname := ComBasketN]
  basket[,protected := FALSE] 
  
  basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
  basket[,Percent_prod := p0q0/All_p0q0]
}

# if(basketn == "calories"){
#   Globalkcal1 <- ReadDatatable("top10_foodsupplykcal")
#   #Globalkcal1 <- Globalkcal1  %>%
#   #  filter(timepointyears == as.numeric(BaseYear[2])-1)
#   
#   Globalkcal1$item_code <- as.character(Globalkcal1$item_code)
#   Globalkcal1 <- merge(Globalkcal1,fbsTree,by.x= c('item_code'), by.y = c("id4"), type ='left',match='all')
#   Globalkcal1 <- Globalkcal1[order(-Globalkcal1$value),]
#   
#   ItemsBasket <- unique(Globalkcal1[ ,head(.SD, 2), by= c('gfli_basket')]$measureditemcpc)
#   basket <-   DataForIndex %>% filter(measureditemcpc %in%  ItemsBasket)
#   basketKeys <- ("measureditemcpc")
#   ComBasketN  <- 'Caloric Value- Top 10 by World'
#   basket[,basketname := ComBasketN]
#   basket[,protected := FALSE] 
#   
# } 

basket <- subset(basket,
                 select = c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","Percent_prod", "protected"))
basket$geographicaream49 <- as.character(basket$geographicaream49)

### Save the Basket Selection to the sws ####
if(savesws){
  write.table(basket ,'Commodbasket.csv', sep=",", row.names=FALSE)
  names(basket) <- tolower(names(basket))
  ## Delete
  table = "sdg123_commoditybasket"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset,  basket[,c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","protected"),with=F])
  Finalise(changeset)
}



#### Calculations ####
# ## SDG Calc 2018 - only official reporting countries with loss data for more than 2 commodities over 3 years##
# DataForIndex$m49Cpc = "" 
# DataForIndex[,m49Cpc := paste(geographicaream49,measureditemcpc,sep=";")]
# SDG_NM$m49Cpc = "" 
# SDG_NM[,m49Cpc := paste(geographicaream49,measureditemcpc,sep=";")]
# tally <- as.data.table(SDG_NM %>%
#      group_by(geographicaream49, measureditemcpc) %>%
#      dplyr:: summarise(n = length(m49Cpc)))
# tally <- tally[!is.na(measureditemcpc) & n>3,]
# 
# tally2 <- as.data.table(tally %>%
#                          group_by(geographicaream49) %>%
#                          dplyr:: summarise(n2 = length(measureditemcpc)))
#   
# tally2 <- tally2[ n2>2,]
# 
# 
# DataForIndex2 <- DataForIndex[geographicaream49 %in% unique(tally2$geographicaream49), ]
# 
# GlobalfoodLoss2 <- GFLI_SDG_fun(BaseYear,keys_lower,"WORLD",basket,basketKeys,DataForIndex2)
# FoodLossIndex2 <- GFLI_SDG_fun(BaseYear,keys_lower,"geographicaream49",basket,basketKeys,DataForIndex2)
# write.xlsx(FoodLossIndex2[!is.na(FLP) & timepointyears>=2005,], "FLI_1oct2018.xlsx")

###

DataForIndex$geographicaream49 <- as.character(DataForIndex$geographicaream49)
GlobalfoodLoss <- GFLI_SDG_fun(BaseYear,keys_lower,"WORLD",basket,basketKeys,DataForIndex)
FoodLossIndex <- GFLI_SDG_fun(BaseYear,keys_lower,"geographicaream49",basket,basketKeys,DataForIndex)
FoodLossIndex[geographicaream49 ==104 & timepointyears>2005,]


SDGRepotingAreas <- c(grep("m49", grep("code", names(CountryGroup),value=TRUE) ,value=TRUE), grep("sdg", grep("code", names(CountryGroup),value=TRUE) ,value=TRUE))

RegionalLossIndex_L1 <- GFLI_SDG_fun(BaseYear,keys_lower,SDGRepotingAreas[2],basket,basketKeys,DataForIndex)
names(RegionalLossIndex_L1) <- names(FoodLossIndex)
RegionalLossIndex_L2 <- GFLI_SDG_fun(BaseYear,keys_lower,SDGRepotingAreas[3],basket,basketKeys,DataForIndex)
names(RegionalLossIndex_L2) <- names(FoodLossIndex)
RegionalLossIndex_sdg <- GFLI_SDG_fun(BaseYear,keys_lower,SDGRepotingAreas[4],basket,basketKeys,DataForIndex)
names(RegionalLossIndex_sdg) <- names(FoodLossIndex)

Index2Save <- rbind(GlobalfoodLoss,FoodLossIndex,RegionalLossIndex_L1,RegionalLossIndex_L2,RegionalLossIndex_sdg)
names(Index2Save) <- tolower(names(Index2Save))
names(Index2Save)[names(Index2Save) =="index"] <- "fli"

Index2Save <- Index2Save[!is.na(flp),]
Index2Save[flp >.1 & timepointyears ==as.numeric(BaseYear[2])-1 ,]
Index2Save[region_code== 1,]

setcolorder(Index2Save, 
            c("region_code", "region_name","timepointyears", "sum_p0qt","sum_p0q0","flp","fli"))

if(savesws){
  # Save to the SWS as data table until a dataset is created
  table = "sdg_12_3_1_fli"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset,Index2Save)
  Finalise(changeset)

}
## Scenario Tests
Index2Save[,combasketn := ComBasketN]
if(savesws){
  # Save to the SWS as data table until a dataset is created
  table = "sdg_12_3_1_fli_diff_baskets"
  changeset <- Changeset(table)
  ## Add
  AddInsertions(changeset,Index2Save)
  Finalise(changeset)
  
 
}

#### Comparison Between Years ####
if(gfli_compare){
  FLI<- GFLI_SDG_fun(BaseYear,keys_lower,aggregation,basket,basketKeys,DataForIndex) 
  FLI[,aggregation] <- sapply(FLI[,aggregation,with=F],as.character)
  FLI_y1 <- FLI[timepointyears== ComparisonYear[1],c(aggregation,"Index"),with=F] 
  FLI_y2 <- FLI[timepointyears== ComparisonYear[2],c(aggregation,"Index"),with=F]
  
  names(FLI_y1)<-  c(aggregation,ComparisonYear[1])
  names(FLI_y2)<-  c(aggregation,ComparisonYear[2])
  
  deltay1_y2 <- merge(FLI_y1,FLI_y2, by= c(aggregation),   all.x = TRUE)
  deltay1_y2[,deltay1_y2:= deltay1_y2[,as.character(ComparisonYear[2]),with=F]/deltay1_y2[,as.character(ComparisonYear[1]),with=F]]
  deltay1_y2[,deltay1_y2 := round(.SD,3), .SDcols="deltay1_y2"]
  
}  

#### Reporting Documents ####
if(gfli_Reporting){
  reporting(ReportingYear,ComparisonYear,BaseYear)


  }
