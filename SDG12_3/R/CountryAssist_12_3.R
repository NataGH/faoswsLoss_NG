#' Part of the FAO Loss Module and the SDG 12.3 Reporting to get countries to fillout the appropriate spreadsheets
#' for data collection on 12.3
#' 
#' @author Alicia English 
#' @export CountryAssist_12_3
#' 
library(faosws)
library(faoswsUtil)
library(faoswsLoss)
library(faoswsFlag)

library(openxlsx)
library(data.table)
library(tidyr)
library(dplyr)
library(plyr)
library(reshape)

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
 
})
proper=function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)
  

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)

##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
LocalRun <-TRUE
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

############# Computation Parameters #####################################
maxYear <- format(Sys.Date(), "%Y")
if (!exists('selectedYear_start', inherits = FALSE)| !exists('selectedYear_end', inherits = FALSE)) {
  ## Year should be a paramameter selected.
  selectedYear_start <- swsContext.computationParams$selectedyear_start
  selectedYear_end <- swsContext.computationParams$selectedyear_end
  selectedYear = as.character(as.numeric(selectedYear_start):as.numeric(selectedYear_end))
}
if (!exists('ctry_modelvar')) {
  ## IF just one country is modeled
  ctry_modelvar <- swsContext.computationParams$ctry_modelvar
}
if(LocalRun){
  ctry_modelvar <- '484'
  selectedYear <- as.character(2014:maxYear)
}  
BaseYear = as.character(c(2014,2016)) ## This is not an option to choose after the movement to the SDG base yr
reportYear = as.character(2015:2025) ## This is not an option to choose after the movement to the SDG base yr


fileTemplates <- "C:\\Users\\Englisha.FAODOMAIN\\Documents\\faoswsLossa\\SDG12_3\\Excel\\"
fileSave <- fileTemplates
#readline("Where would you like to save the file: ")  

#### Data In #####
production <- getProductionData(areaVar,itemVar,yearVar,elementVar, selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')

names(imports) <- tolower(names(imports))
names(production) <- tolower(names(production))
names(Losses) <- tolower(names(Losses))
names(Losses)[names(Losses) == "measureditemsuafbs"] <- "measureditemcpc"

production$geographicaream49 <- as.character(production$geographicaream49)
imports$geographicaream49 <- as.character(imports$geographicaream49)
production$timepointyears <- as.numeric(production$timepointyears)
imports$timepointyears<- as.numeric(imports$timepointyears)

Losses$flagcombination <- " "
Losses[,flagcombination :=  paste(flagobservationstatus, flagmethod, sep = ";")] 

CommodityBasket <- ReadDatatable("sdg123_commoditybasket")  
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
ConvFactor1  <- ReadDatatable('flw_lossperfactors_')
ConvFactor1$measureditemcpc <- addHeadingsCPC(ConvFactor1$measureditemcpc)

ctryFLIReport <- function(selectedYear,ctry_modelvar){
  #####  SDG 12.3 Assistance Doc ##############
  filesIn <- paste(fileTemplates,"FLP-FLI Calculation_original_12Sept18.xlsx", sep="")
  
  wb <- loadWorkbook(filesIn)
  
  sheets <- getSheetNames(filesIn)
  
  
  #### Step 1: Select Commodity Basket #####
  Step1_SelectCommodityBasket <- read.xlsx(wb, sheet = sheets[3])
  SCB_Headings <- unname(unlist(c(Step1_SelectCommodityBasket[1,])))
    
  
  CommodityBasket <- CommodityBasket %>% filter(geographicaream49 %in% ctry_modelvar) 
  CB <- subset(CommodityBasket,  select = c("gfli_basket", "measureditemcpc", "itemname","intprice"))
  setnames(CB, old= c("gfli_basket", "measureditemcpc", "itemname","intprice"), new = c("Heading", "measureditemcpc", "Item Name","Price"))
  
  totproduction <- production %>%  filter((geographicaream49 %in% ctry_modelvar)&(timepointyears == as.numeric(BaseYear[2])-1))
  totimport  <- imports  %>%  filter((geographicaream49 %in% ctry_modelvar)&(timepointyears == as.numeric(BaseYear[2])-1))
  tot <- as.data.table(merge(totproduction, totimport, by= keys_lower))
  tot[,pi_tot:= rowSums(.SD, na.rm = TRUE), .SDcols=c("value.x" ,"value.y")]
  pi_tot2 <- sum(tot$pi_tot)
  
  Prod <- production %>% filter((geographicaream49 %in% ctry_modelvar) & ( measureditemcpc %in% CommodityBasket$measureditemcpc))
  BaseProd <- as.data.table(Prod %>% filter(timepointyears %in% as.numeric(BaseYear[1]):as.numeric(BaseYear[2])))
  BaseProd[,Prodqty_avey1y2 := mean(value),by = c('measureditemcpc')]
  BaseProd <- BaseProd%>% filter(timepointyears == as.numeric(BaseYear[2])-1)
  
  imp <- as.data.table(imports %>% filter((geographicaream49 %in% ctry_modelvar) & ( measureditemcpc %in% CommodityBasket$measureditemcpc)))
  imp[,Prodqty_avey1y2 := mean(value),by = c('measureditemcpc')]
  imp <- imp%>% filter(timepointyears == as.numeric(BaseYear[2])-1)
  
  
  table <- merge(CB, BaseProd[,c("measureditemcpc","Prodqty_avey1y2"),with=F], by= "measureditemcpc")
  names(table)[names(table)=="Prodqty_avey1y2"] <- "Production" 
  table <- merge(table,imp[,c("measureditemcpc","Prodqty_avey1y2"),with=F], by= "measureditemcpc",all.x=T) 
  names(table)[names(table)=="Prodqty_avey1y2"] <- "Imports"  
  table <- as.data.table(table)
  table$pi = 0
  table[,pi:= rowSums(.SD, na.rm = TRUE), .SDcols=c("Production" ,"Imports"),]
  
  table[,percent:= round(pi/pi_tot2,2)]
  table$Production <- format(round(table$Production,0), big.mark=",")
  table$Imports <- format(round(table$Imports,0), big.mark=",")
  table$pi <- format(round(table$pi,0), big.mark=",")
  table$`Item Name` <- proper(table$`Item Name`)
  
  
  table <- subset(table,select = c("Heading", "measureditemcpc", "Item Name","Production" ,"Imports","pi","Price", 'percent'))
  names(table) <- SCB_Headings
  
  newrow <- c("Fish & Fish Products", "0", " ",0 ,0,0,0,0)
  m <- as.data.table(rbind(newrow,newrow))
  names(m) <- names(table)
  table <- rbind(table,m)
  
  n <- table[Heading == "Other",]
  table <- table[!Heading == "Other",]
  setorder(table,"Heading")
  table <- rbind(table,n)
  
    
  writeDataTable(wb, sheets[3], table, startRow = 3, startCol = 1 ,withFilter = FALSE)  
  
  #### Add Production and Imports #####
  Step1a <- read.xlsx(wb, sheet = sheets[4])
  years <- na.omit(unname(unlist(Step1a[1,])))
  production2 <- getProductionData(areaVar,itemVar,yearVar,elementVar, selectedYear) # Value_measuredElement_5510
  names(production2)[names(production2)=="Value"] <- "Production"
  imports2 <- getImportData(areaVar,itemVar,yearVar,selectedYear)
  names(imports2)[names(imports2)=="Value"] <- "Imports"
  
  names(imports2) <- tolower(names(imports2))
  names(production2) <- tolower(names(production2))
  production2$geographicaream49 <- as.character(production2$geographicaream49)
  imports2$geographicaream49 <- as.character(imports2$geographicaream49)
  production2$timepointyears <- as.numeric(production2$timepointyears)
  imports2$timepointyears<- as.numeric(imports2$timepointyears)
  
  table2 <- merge(production2,imports2, by= keys_lower, all.x=T)
  t2 <- table2 %>% filter((geographicaream49 %in% ctry_modelvar)  &( measureditemcpc %in% CommodityBasket$measureditemcpc))
  t2 <- as.data.table(t2)
  tb <-t2[timepointyears == 2015,"measureditemcpc",with=F]
  named <-"measureditemcpc"
  for( ii in 1:length(unique(t2$timepointyears))){
    t2a <- t2 [timepointyears == unique(t2$timepointyears)[ii],c("measureditemcpc","production","imports"), with=F]
    tb <-merge(tb,t2a, by= c("measureditemcpc") )
    named <- cbind(named,"Production", "Imports")
  }
  tb3 <- table[,"CPC",with=F]
  names(tb3) <- "measureditemcpc"
  tb3 <- join(tb3,tb, by= c("measureditemcpc"))
  tb3[,measureditemcpc:=NULL]
  named <- as.data.frame(named)[2:length(named)]
  
  writeDataTable(wb, sheets[4], tb3, startRow = 4, startCol = 4 , firstColumn = FALSE,colNames = TRUE,withFilter = FALSE)  
  
  
  #### Step2 - FLPs along the national and subnational stages #####
  selectedYear <- selectedYear[selectedYear>=2015]
  Sources <- read.xlsx(wb, sheet = sheets[length(sheets)])
  conv <- ConvFactor1 %>% filter((geographicaream49 == ctry_modelvar)& (measureditemcpc %in% CommodityBasket$measureditemcpc) )
  SourceConv <-subset(conv, select = c("geographicaream49", "measureditemcpc","crop","timepointyears","loss_per_clean","fsc_location","tag_datacollection",
                                       "reference","url"))
  
  for(yr in 1:length(selectedYear)){
    Step2 <- read.xlsx(wb, sheet = paste("Step2_FLP_SubNat_",selectedYear[yr],sep=""),startRow = 2)
    #First table - Identification
    Step2_1 <- Step2[1:3,1:2]
    Step2_1[Step2_1$Item == "Country","Data"] <- CountryGroup[m49_code == ctry_modelvar,"m49_region",with=F]
    Step2_1[Step2_1$Item == "Year ","Data"] <- selectedYear[yr]
    writeDataTable(wb,  paste("Step2_FLP_SubNat_",selectedYear[yr],sep=""), Step2_1, startRow = 2, startCol = 1,withFilter = FALSE )  
    
    #second table _ loss percetnages by stage
    Step2_2 <- as.data.table(Step2[7:19,4:9])
    names(Step2_2) <-unname(unlist(Step2_2[1,]))
    Step2_2 <- Step2_2[!1,]
    
    CB <- table[,c("CPC","Item Name"),with=F]
    names( CB)[names( CB)=="CPC"] <- "measureditemcpc"
    nums <- as.data.table(matrix(0.0, nrow = dim(Step2_2)[1], ncol = dim(Step2_2)[2]))
    names(nums) <- names(Step2_2 )
    nums <- cbind(CB,nums)
    tb2_2 <- cbind(CB,Step2_2)
    
    sourceyr <- SourceConv %>% filter(timepointyears == selectedYear[yr])
    if(dim(sourceyr)[1]>0){
      #averages the ranges of available data points
      for( i in nums$measureditemcpc){
        for(ii in names(Step2_2)){
          if(!is.na(sourceyr[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean"]) &
             dim(sourceyr[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F])[1]>0 ){
            if(dim(sourceyr[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F])[1]>=2){
              nums[(measureditemcpc==i), ii] <- colMeans(sourceyr[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F],na.rm=T)
            }else{
              nums[(measureditemcpc==i), ii] <- sourceyr[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F]
            }
          }}
      }
      nums[,c("measureditemcpc","Item Name") := NULL]
      writeDataTable(wb,  paste("Step2_FLP_SubNat_",selectedYear[yr],sep=""),nums, startRow = 11, startCol = 4,withFilter = FALSE )  
      
    }
    
    
    #third table _ loss percetnages by whole supply chain - FBS 
    Step2_3 <- as.data.table(Step2[7:19,11])
    names(Step2_3) <-unname(unlist(Step2_3[1,]))
    Step2_3 <- Step2_3[!1,]
    
    tb2_3 <- cbind(CB,Step2_3)
    
    FBS_Losses <- Losses %>% filter((geographicaream49 == ctry_modelvar) & (measureditemcpc %in% CommodityBasket$measureditemcpc) & 
                                        timepointyears == (selectedYear[yr]))
    FBS_Losses1 <- subset(FBS_Losses,select = c("measureditemcpc","value","flagcombination"))
      
    FBS_Losses2 <-join(tb2_3, FBS_Losses1, by= c("measureditemcpc"))
    FBS_Losses2$value <- round(FBS_Losses2$value,3)
    FBS_Losses2[,"Whole Supply Chain" := NULL]
    names(FBS_Losses2)[names(FBS_Losses2)=="value"] <- names(Step2_3)
      
    writeDataTable(wb,  paste("Step2_FLP_SubNat_",selectedYear[yr],sep=""), FBS_Losses2[,c("Whole Supply Chain", "flagcombination"),with=F], startRow = 11, startCol = 12,withFilter = FALSE )  
    
  }
    
  ### Pre-2015 estimates along the supply chain ####
  a <- SourceConv[SourceConv$measureditemcpc %in%  CB$measureditemcpc,]
  if(nrow(a)>0){
    Step4 <- read.xlsx(wb, sheet =  sheets[5],startRow = 2)
    
    Step4_1 <- Step4[1:3,1:2]
    Step4_1[Step2_1$Item == "Country","Data"] <- CountryGroup[m49_code == ctry_modelvar,"m49_region",with=F]
    Step4_1[Step2_1$Item == "Year ","Data"] <-  paste(min(a$timepointyears),max(a$timepointyears), sep=" - ")
    writeDataTable(wb, sheets[5], Step4_1, startRow = 2, startCol = 1,withFilter = FALSE )  
    
    #second table _ loss percetnages by stage
    Step4_2 <- as.data.table(Step4[7:19,4:9])
    names(Step4_2) <-unname(unlist(Step4_2[1,]))
    Step4_2 <- Step4_2[!1,]
    
    CB <- table[,c("CPC","Item Name"),with=F]
    names( CB)[names( CB)=="CPC"] <- "measureditemcpc"
    tb2_2 <- cbind(CB,Step4_2)
    
  
    a <- as.data.table(a)
    for( i in CB$measureditemcpc){
      for(ii in names(Step4_2)){
        if(!is.na(a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean"]) &
            dim(a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F])[1]>0 ){
          if(dim(a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F])[1]>=2){
             tb2_2[(measureditemcpc==i), ii] <- paste(min( a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F]),
                  max(a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F]),sep= " - ")
          }else{
            tb2_2[(measureditemcpc==i), ii] <- as.character(a[(measureditemcpc==i) & (fsc_location ==ii),"loss_per_clean",with=F])
          }
      }}
    }
    
    tb2_2[,c("measureditemcpc","Item Name") := NULL]
    writeDataTable(wb,  sheets[5],tb2_2, startRow = 11, startCol = 4,withFilter = FALSE )  
  } 
  ### Write Sources Table ####
  
  writeDataTable(wb, sheets[length(sheets)],SourceConv , startRow = 1, startCol = 1,withFilter = T) 
  ### Clean up sheets ####
  Step6 <- read.xlsx(wb, sheet ="Step3_CompareFLI",startRow = 2)
  Step6_1 <-  as.data.table(Step6[17:29,4:14])
  names(Step6_1) <-unname(unlist(Step6_1[1,]))
  Step6_1[,c(selectedYear) :=NULL]
  Step6_1 <- Step6_1[!1,]
  writeDataTable(wb,  "Step3_CompareFLI",Step6_1, startRow = 21, startCol = 4+length(selectedYear),withFilter = FALSE ) 
  
  for(iii in reportYear[!reportYear %in%selectedYear]){
  #  openxlsx::removeWorksheet(wb,paste("Step2_FLP_SubNat_",iii,sep=""))
    which(sheets == paste("Step2_FLP_SubNat_",iii,sep=""))
    sheetVisibility(wb)[which(sheets == paste("Step2_FLP_SubNat_",iii,sep=""))] <- FALSE
    }
    
  saveWorkbook(wb,paste(fileSave,paste("FLP-FLI Calculation_original_", Sys.Date(),"_",CountryGroup[m49_code == ctry_modelvar,"m49_region",with=F] ,".xlsx", sep=""), sep=""),overwrite = T)
}

ctryFLIReport(selectedYear,ctry_modelvar)
