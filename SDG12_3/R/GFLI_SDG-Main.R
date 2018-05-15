#' Module for estimating Global Food Loss Index
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English 
#' 
#' 
######### Load all libraries ###########
#install.packages('faosws')
#install_github(repo = "SWS-Methodology/faoswsFlag")
#install_github(repo = "SWS-Methodology/faoswsUtil")
#install.packages("faoswsLoss", repo = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/") 




library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(devtools)
library(openxlsx)
library(tidyr)


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

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")


######### Options ############
savesws <- TRUE
selectedYear <- as.character(1991:2016)
LocalRun <- FALSE

gfli_calc <- TRUE
aggregation  <-  "geographicaream49" 
#  "iso2code","isocode","countryname","sdgregion_code","sdg_regions"           
# "m49_level1_code","m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions"            
# "ldcs_code","ldcs","lldcssids_code","lldcssids","Country","geographicaream49", "WORLD"   

weights <- "intl_prices" 
basketn <- "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"

gfli_compare <- TRUE
ComparisonYear <- as.character(c(2005,2016))

gfli_Reporting <- TRUE
ReportingYear <- as.character(c(2015))
#####################

BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)

####----  Data In ----------############
# This return FALSE if on the Statistical Working System
if(CheckDebug() & !LocalRun){
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
  
  Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
  production <- getProductionData(areaVar,itemVar,yearVar,elementVar) # Value_measuredElement_5510
  fbsTree <- ReadDatatable("fbs_tree")
  CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
  
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



CountryGroup$Country <- tolower(CountryGroup$countryname)
CountryGroup[,"geographicaream49":=CountryGroup$m49code] 
names(Losses)[names(Losses) =="Value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measuredItemSuaFbs"] <- "measureditemcpc"
names(Losses) <- tolower(names(Losses))
names(production) <- tolower(names(production))
names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
FAOCrops[, "crop" := FAOCrops$description]
names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"

#SDG Headings
fbsTree[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",


ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value_measuredelement_5510")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value_measuredelement_5510),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- unique(Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F])

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
  Weights <- intPrice2005Selected[, c("itemname","measureditemcpc","intprice")]
  
  #distinct(intPrice2005Selected,measuredItemCPC)
}

## Production multiplied by the weighting scheme
FLIData <-  merge(Base_Prod,Weights, by.x = c('measureditemcpc'), by.y = c('measureditemcpc'),all.x = F, all.y = F)
FLIData[, p0q0 := qty_avey1y2*intprice,]

## Loss percentages multiplied by the quantity weighted (for the numerator), Includes all commodities
DataForIndex <- merge(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , FLIData, by =c("measureditemcpc","geographicaream49"),type= 'left', match='all')
DataForIndex$l0ptqt =0
DataForIndex[,l0ptqt:=value_measuredelement_5126*p0q0,with=T]

DataForIndex <- join(DataForIndex ,fbsTree , by = c('measureditemcpc'),type= 'left', match='all')
DataForIndex <- join(DataForIndex,CountryGroup, by = c('geographicaream49'),type= 'left', match='all')

#### Basket ####
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
  
  basket <- merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE)
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

basket <- basket[,c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","Percent_prod", "protected")]
basket$geographicaream49<- as.character(basket$geographicaream49)
basket <- merge(basket, CountryGroup, by= c("geographicaream49"), all.x = TRUE)

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
  AddInsertions(changeset,  basket[,c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","protected")])
  Finalise(changeset)
}



#### Calculations ####
if(gfli_calc){
  DataForIndex$geographicaream49 <- as.character(DataForIndex$geographicaream49)
  GlobalfoodLoss <- GFLI_SDG_fun(BaseYear,keys_lower,"WORLD",basket,basketKeys,DataForIndex)
  FoodLossIndex <- GFLI_SDG_fun(BaseYear,keys_lower,aggregation,basket,basketKeys,DataForIndex)
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
  deltay1_y2[,deltay1_y2:= deltay1_y2[,ComparisonYear[2],with=F]/deltay1_y2[,ComparisonYear[1],with=F]]
  deltay1_y2[,deltay1_y2 := round(.SD,3), .SDcols="deltay1_y2"]
  
}  

#### Reporting Documents ####
if(gfli_Reporting){
  reporting(ReportingYear,ComparisonYear,BaseYear)


  }
