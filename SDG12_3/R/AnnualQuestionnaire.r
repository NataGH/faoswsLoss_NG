#' Part of the FAO Loss Module. This is the reporting script for the SDG committee and for the 
#' ESS reporting for the department indicator 402B.
#' 
#' The function reports for a specific year, and can include a comparison year.
#' 
#' @author Alicia English 
#' @export reporting 



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
ConvFactor1  <- as.data.table(ReadDatatable('flw_lossperfactors'))
BasketExist <- ReadDatatable("sdg123_commoditybasket")
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")

basketItems <- c("<Fruits & Vegetables>", "<Meat & Animals Products>","<Roots, Tubers & Oil-Bearing Crops>",
                 "<Cereals & Pulses>","<Fish & Fish Products>")
basketItems <- c(basketItems, basketItems)


reporting <- function(ReportingYear, ctry_modelvar){
  library(data.table)
  library(openxlsx)
  library(tidyr)
  library(dplyr)
  library(stringr)
  # 
  fileTemplates <- "T:\\Team_working_folder\\A\\GFLI\\9_TechnicalAssistance\\Questionnaires\\ESS_Annual\\"
  fileSave <-  paste0(fileTemplates,ReportingYear, "\\")
 
  ReportingYr <- as.numeric(max(ReportingYear))
  maxYear <- 2000
  
  ##### Official SDG Reporting ##############
  wb <- loadWorkbook(paste(fileTemplates,"FAO_000_ESS_SDGLoss_QUEST_2019_en.xlsx", sep=""))
  
  ctryN <- CountryGroup[m49_code %in% ctry_modelvar, "m49_region", with=F] 
  
  writeData(wb, "Cover",   unlist(ctryN)  , withFilter = FALSE, startRow = 14, startCol = 2)
  
  ### Prefilled Section 1 #############
  Sub <- read.xlsx(wb, sheet = "Sec1 Losses")
  HeadingsKeep1 <- Sub[3,]
  colnames(Sub) <- gsub(" ", ".",unlist(HeadingsKeep1))
  HeadingsKeep <- colnames(Sub)
 

  table <- BasketExist  %>% filter(geographicaream49 %in% ctry_modelvar  & gfli_basket != "Other") 
  
  newrow <- c(ctry_modelvar,"Fish & Fish Products", "0", "<Fish & Fish Products>",0 ,0,0," ",FALSE)
  m <- as.data.table(rbind(newrow,newrow))
  names(m) <- names(table)
  table <- rbind(table,m)
  
  n <- table[gfli_basket == "Other",]
  table <- table[!gfli_basket == "Other",]
  table <- rbind(table,n)
  
  table$itemname <- str_to_title(table$itemname)
  lossSec1 <- as.data.frame(table$itemname)
  names(lossSec1) <- "COMMODITY"
  if(nrow(table) != 10){
    if(all(basketItems %in% unlist(table$gfli_basket) )){
      v <- as.data.frame(table(unlist(table$gfli_basket))[table(unlist(table$gfli_basket))-2 != 0])
      names(v) <- c( "COMMODITY")
      va <- as.data.frame(character())
    }else{
      v <- as.data.frame(table(unlist(table$gfli_basket))[table(unlist(table$gfli_basket))-2 != 0])
      names(v) <- c( "COMMODITY")
      va <- as.data.frame(basketItems[! basketItems %in% unlist(table$gfli_basket)])
      va <- as.data.frame(apply(va ,1, function(X) paste0("<", X, ">")))
      names(va) <- "COMMODITY"
      
    }
    
    v$COMMODITY <-v$Var1
   # v <- as.data.table(v)
    
    if(length(v[v$Freq == 2,"COMMODITY"])>0){
      vv <- v[v$Freq == 2,"COMMODITY"]
      vv0 <- as.data.frame(apply(vv0,1, function(X) paste0("<", X, ">")))
      names(vv2) <- "COMMODITY"
    }else{
      vv0 <- v[v$Freq == 2,"COMMODITY"]}
    if(length(v[v$Freq == 1,"COMMODITY"])>0){
      vv1 <- v[v$Freq == 1,"COMMODITY"]
      vv2 <- as.data.frame(apply(vv1,1, function(X) paste0("<", X, ">")))
      names(vv2) <- "COMMODITY"
    }else{
      vv2<- v[v$Freq == 1,"COMMODITY"]}
    if(nrow(table)== 2){
      lossSec1 <- as.data.frame(basketItems)
      lossSec1 <- as.data.frame(apply(lossSec1 ,1, function(X) paste0("<", X, ">")))
      names(lossSec1) <- "COMMODITY"
      va <- as.data.frame(character())
      names(va) <- "COMMODITY"
      warn <- "err"
    }
    lossSec1 <- rbind(lossSec1, vv2,vv0, va)
    if(nrow(lossSec1) == 10){
     warn <- ""
    }else{warn <- "err"}
  }else{warn <- ""}


  writeDataTable(wb, "Sec1 Losses",  lossSec1 , withFilter = FALSE, startRow = 4, startCol = 1)

  ### Prefilled Section 2  #############
  Sub <- read.xlsx(wb, sheet = "Sec2 Historic Loss Data")
  HeadingsKeep1 <- Sub[3,]

  colnames(Sub) <- gsub(" ", ".",unlist(HeadingsKeep1))
  HeadingsKeep <- colnames(Sub)
  
  

  
  
  table2 <-as.data.table(ConvFactor1 %>% filter(geographicaream49 %in% ctry_modelvar & timepointyears > maxYear) )
  table2$Official <- ""
  table2[tag_datacollection  %in% c("SWS","FBS/APQ", "NationalStatsYearbook", "NationalAcctSys"),Official := "Y" ]
  table2[tag_datacollection %in% c("SWS","FBS/APQ"),tag_datacollection := "ESS Collected - official or Semi Official, Percentages calculated by ESS" ]
  table2[tag_datacollection %in% c("LitReview"),tag_datacollection := "Reported within the document as a previous review" ]
  table2[method_datacollection %in% c(""),method_datacollection := "No method specified" ]
  table2[samplesize %in% c(""),samplesize := "No sample sized specified" ]
  
  table2[fsc_location  %in% c( "sws_total", "wholesupplychain"),fsc_location := "Whole Supply Chain (post-harvest to retail)" ]
  table2[reference == "SWS",reference  := "" ]
  table2[, "Sources" := paste(table2$reference ,table2$url, sep="; ")]
  table2[, "Metadata" := paste(table2$tag_datacollection, table2$method_datacollection,table2$samplesize,table2$notes, sep="; ")]
  
  table2$causeofloss <- gsub("[\r\n]", " ", table2$causeofloss)
  table2$Metadata <- gsub("[\r\n]", " ", table2$Metadata)
  
  table2a <- subset(table2, select =c("crop", "region", "timepointyears", "fsc_location" , "activity","loss_quantity", "Official",
            "loss_quantity_ref","percentage_loss_of_quantity", "causeofloss","Sources","Metadata"))
  
  
  table2a$crop <- str_to_title(table2a$crop)
  table2a$fsc_location <- str_to_title(table2a$fsc_location)
  
  
  setnames(  table2a, old =  c("crop", "region", "timepointyears", "fsc_location" , "activity","loss_quantity","Official",
                                                    "loss_quantity_ref","percentage_loss_of_quantity", "causeofloss","Sources","Metadata"),
           new =  c("COMMODITY", "Geographic scope", "Year", "Stage(s) of the supply chain" , "Activity(ies)","Losses (tons)","Official Y/N", 
                    "Reference quantities (tons)","Loss %","Cause(s) of Loss","Sources","Data collection method, sample size, Notes") )

  
  writeDataTable(wb, "Sec2 Historic Loss Data",  table2a , withFilter = FALSE,  bandedRows = TRUE, startRow = 4, startCol = 1)
  ########## Save Workbook ###############
    if(ctry_modelvar< 100 & ctry_modelvar>= 10 ){
    ctry_modelvarN <- paste0("0", ctry_modelvar)
  }
  if(ctry_modelvar< 10 ){
    ctry_modelvarN <- paste0("00", ctry_modelvar)
  }
  if(ctry_modelvar>= 100 ){
    ctry_modelvarN <- ctry_modelvar
  }  
  saveWorkbook(wb,paste(fileSave,paste0(warn,"FAO_", ctry_modelvarN, "_ESS_SDGLoss_QUEST_2019_en.xlsx"), sep=""),overwrite = T)
  

  
}

for(i in CountryGroup$m49_code){
 reporting(2019,as.numeric(i))
}
## Fix China with the different m49 Code...
reporting(2019,as.numeric(1248))
