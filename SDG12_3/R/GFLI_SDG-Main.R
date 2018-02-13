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

######### Options ############
savesws <- TRUE
selectedYear <- as.character(1991:2015)


gfli_calc <- TRUE
aggregation  <-  "geographicaream49" 
#  "iso2code","isocode","countryname","sdgregion_code","sdg_regions"           
# "m49_level1_code","m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions"            
# "ldcs_code","ldcs","lldcssids_code","lldcssids","Country","geographicaream49", "WORLD"   

weights <- "intl_prices" 
basketn <- "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"

gfli_compare <- TRUE
ComparisonYear <- as.character(c(2013,2015))

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
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar) # Value_measuredElement_5510
fbsTree <- ReadDatatable("fbs_tree")
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")

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

######################################
############ Index #############
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
  
  #distinct(intPrice2005Selected,measuredItemCPC)
}


######################################################################################################################################
# Index calculation

ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value_measuredelement_5510")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

aveP <- ProdQtySWS[,qty_avey1y2 := mean(value_measuredelement_5510),by = c("geographicaream49",'measureditemcpc')]
aveP <- unique(aveP[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F])

aveP$measureditemcpc <- lapply(aveP$measureditemcpc, as.character)
aveP$measureditemcpc<- addHeadingsCPC(aveP$measureditemcpc)

FLIData <-  merge(aveP,intPrice2005Selected, by.x = c('measureditemcpc'), by.y = c('measureditemcpc'),all.x = F, all.y = F)
FLIData[, p0q0 := qty_avey1y2*intprice,]
FLIData <- join(FLIData ,fbsTree , by = c('measureditemcpc'),type= 'left', match='all')

FLIData <- join(FLIData,CountryGroup, by = c('geographicaream49'),type= 'left', match='all')


if(gfli_calc){
  GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"WORLD",weights,basketn,FLIData)
  FoodLossIndex <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,aggregation,weights,basketn,FLIData)
}

if(gfli_compare){
  FLI<- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,aggregation,weights,basketn,FLIData) 
  FLI[,aggregation] <- sapply(FLI[,aggregation,with=F],as.character)
  FLI_y1 <- FLI[timepointyears== ComparisonYear[1],c(aggregation,"Index"),with=F] 
  FLI_y2 <- FLI[timepointyears== ComparisonYear[2],c(aggregation,"Index"),with=F]
  
  names(FLI_y1)<-  c(aggregation,ComparisonYear[1])
  names(FLI_y2)<-  c(aggregation,ComparisonYear[2])
  
  deltay1_y2 <- merge(FLI_y1,FLI_y2, by= c(aggregation),   all.x = TRUE)
  deltay1_y2[,deltay1_y2:= deltay1_y2[,ComparisonYear[2],with=F]/deltay1_y2[,ComparisonYear[1],with=F]]
  deltay1_y2[,deltay1_y2 := round(.SD,3), .SDcols="deltay1_y2"]
  
}  

if(gfli_Reporting){
  reporting(ReportingYear,ComparisonYear)

}
