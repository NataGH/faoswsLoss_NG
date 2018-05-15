#' Part of the FAO Loss Module
#' 
#' @author Alicia English 
#' 
#' 
#' 
# ---
#   runtime: shiny
# output: html_document
# ---
# 
#library(faosws)
#library(faoswsUtil)
#library(faoswsLoss)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(rmarkdown)
library(gtools)
library(ggplot2)
library(grid)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)



suppressMessages({
  library(dplyr)
  
})


BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
LocalRun <- FALSE



# ###----  Data In ----------############
if(!LocalRun){
  library(faosws)
  library(faoswsUtil)
  library(faoswsLoss)
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
  
  LossFactorRaw <- ReadDatatable('flw_lossperfactors_')
  AggregateLoss <- ReadDatatable('aggregate_loss_table')
  fbsTree <- ReadDatatable("fbs_tree")
  CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
  LossFactorRaw$measureditemcpc <- addHeadingsCPC(LossFactorRaw$measureditemcpc)
  
  setnames(CountryGroup, old = c("m49code","iso2code","isocode","countryname","sdgregion_code","sdg_regions","m49_level1_code",        
                                 "m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions","ldcs_code","ldcs",                   
                                 "lldcssids_code","lldcssids","fao_region","fao_operationalcoverage"),
           new = c("geographicaream49","ISO2code","isocode","Country","sdgregion_code","SDG Regions","m49_level1_code",        
                   "Geographic Regions(m49) Level1","m49_level2_region_code","Geographic Regions(m49) Level2","mdgregions_code","MDG Regions","ldcs_code","Least Developed Countries (LDC)",                   
                   "lldcssids_code","Land Locked Developing Countries (LLDC)","FAO Operational Region","FAO Operational Coverage"))
  
  
}else{
  setwd(paste(getwd(),"/shiny/Input_Data",sep=""))
  load("Inputs.RData")
  
}

CountryGroup$fao_operationalcoverage<-as.character(CountryGroup$fao_operationalcoverage)
CountryGroup[fao_operationalcoverage %in% c("1"),fao_operationalcoverage := "Yes"]
CountryGroup[fao_operationalcoverage %in% c("0"),fao_operationalcoverage := "No"]


names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
FAOCrops[, "crop" := FAOCrops$description]
names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"
FAOCrops <- FAOCrops[order(FAOCrops$measureditemcpc),]

#SDG Headings
fbsTree[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",

LossFactorRaw[fsc_location =="SWS","fsc_location" ] <- "Official/Semi-Official - National"
LossFactorRaw[fsc_location =="sws_total","fsc_location" ] <- "Official/Semi-Official - National"
LossFactorRaw[fsc_location =="Calc","fsc_location" ] <- "Aggregated from multiple sources"


AggregateLoss[fsc_location =="SWS","fsc_location" ] <- "Official/Semi-Official - National"
AggregateLoss[fsc_location =="sws_total","fsc_location" ] <- "Official/Semi-Official - National"
AggregateLoss[fsc_location =="Calc","fsc_location" ] <- "Aggregated from multiple sources"

unlist(names(AggregateLoss))

LossFactorRaw[,"analyst" := NULL]
LossFactorRaw[,"notes" := NULL]
setnames(LossFactorRaw, old = c("geographicaream49","isocode","country","region","measureditemcpc","crop","timepointyears","loss_per_clean","percentage_loss_of_quantity",
                                "loss_quantity","loss_qualitiative","loss_monetary",
                                "activity","fsc_location","periodofstorage","treatment","causeofloss","samplesize",
                                "units","method_datacollection","tag_datacollection","reference","url"),
         new = c("geographicaream49","isocode","Country","Region","measureditemcpc","Crop","Year","loss_per_clean","Range of Quantity Loss (%)",
                 "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)",
                 "Activity","Stage","period of storage","treatment","Causes of loss","Sample Size",
                 "Sampling Units","Method of Data Collection","Data Collection Tag","Reference","Url"))

setnames(AggregateLoss, old = c("geographicaream49","isocode","timepointyears","country","measureditemcpc","crop",
                                "loss_per_clean","fsc_location"),
         new = c("geographicaream49","isocode","Year","Country","measureditemcpc","Crop",
                 "loss_per_clean","Stage"))



LossFactorRaw[,"Average Quantity Loss (%)":=loss_per_clean]
AggregateLoss[,"Average Quantity Loss (%)":=loss_per_clean]

LossFactorRaw$fsc_location1 = sapply(strsplit(LossFactorRaw$Stage,"/"), '[', 1)
LossFactorRaw <- merge(LossFactorRaw,CountryGroup, by=c("isocode", "geographicaream49"))
AggregateLoss <- merge(AggregateLoss,CountryGroup, by=c("isocode", "geographicaream49"))
LossFactorRaw[, "Country.y" := NULL]
names(LossFactorRaw)[names(LossFactorRaw) =="Country.x"] <- "Country"
names(AggregateLoss)[names(AggregateLoss) =="Country.x"] <- "Country"
datatags <- sort(unlist(unique(LossFactorRaw$"Data Collection Tag")),decreasing=F)
dataStages <- sort(unlist(unique(LossFactorRaw$fsc_location1)))
LossFactorRaw[Stage =="SWS_Total","Stage"] = "Official - Whole chain Estimate"

LossFactorRaw2 <- merge(LossFactorRaw,fbsTree, by=c("measureditemcpc"))
LossFactorRaw_descriptivestat <- LossFactorRaw2 %>%
  filter(loss_per_clean >0 & Reference != "SWS") %>%
  group_by(gfli_basket) %>%
  do(data.frame(t(quantile(.$loss_per_clean, probs = c(0.25, 0.50, 0.75)))))

LossFactorRaw_descriptivestat2 <- LossFactorRaw2 %>%
  filter(loss_per_clean >0 & Reference != "SWS") %>%
  group_by(gfli_basket) %>%
  dplyr::summarise(n=n())


