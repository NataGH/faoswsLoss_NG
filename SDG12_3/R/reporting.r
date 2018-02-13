#' Part of the FAO Loss Module
#' 
#' @author Alicia English 
#' 
#' 

library(openxlsx)
library(tidyr)
library(dplyr)

reporting <- function(ReportingYear,ComparisonYear){
  # 
  fileTemplates <- "C:\\Users\\ENGLISHA\\Documents\\faoswsLossa\\SDG12_3\\Excel\\"
  fileSave <- readline("Where would you like to save the file: ")  
  
  ReportingYr <- as.numeric(max(ReportingYear))
  
  ##### Official SDG Reporting ##############
  wb <- loadWorkbook(paste(fileTemplates,"SDGDataRequest_02Feb2017.xlsx", sep=""))
  Sub <- read.xlsx(wb, sheet = "Submission")
  HeadingsKeep1 <- Sub[5,]
  Sub <- as.data.table(Sub[6:dim(Sub)[1],])
  colnames(Sub) <- gsub(" ", ".",unlist(HeadingsKeep))
  HeadingsKeep <- colnames(Sub)
  Sub$"Observation.value" <- as.numeric(Sub$"Observation.value")
  Sub$"Time.Period.(Year)" <- as.numeric(Sub$"Time.Period.(Year)")
  Sub[,'Time.Period.(Year)' := ReportingYr]
  Sub[,'Disaggregation.classification' :=  ComBasketN ]
  Sub$`Time.Period.(Year)` <- ReportingYr
  RowOrder <-Sub$Reference.area.code
  
  # GFLI
  GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"WORLD",weights,basketn,FLIData) 
  Sub[Reference.area.code == 1, "Observation.value"] = GlobalfoodLoss[timepointyears == ReportingYear,GFLI] # GFLI
  Sub$Reference.area.code <- as.character(Sub$Reference.area.code)
  Ref <- colnames(CountryGroup)
  
  for(ii in 1:(length(Ref)-1)){
  
      foodLossIndex <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,Ref[ii],weights,basketn,FLIData)
      Index_RY <- foodLossIndex[timepointyears == ReportingYear, ]
      Sub <- merge(Sub,Index_RY,by.x = c("Reference.area.code"), by.y = c(Ref[ii]),all.x = TRUE)
      Sub[Reference.area.code %in% unlist(Index_RY[,Ref[ii],with=F]),Observation.value := Index]
      Sub <- Sub[,HeadingsKeep,with=F]
  }
  
  Sub <- Sub %>% slice(match(RowOrder,Reference.area.code))
  writeData(wb, "Submission", Sys.Date()   , startRow = 1, startCol = 2)
  writeData(wb, "Submission", "UNFAO",       startRow = 2, startCol = 2)
  writeData(wb, "Submission", "Target 12.3", startRow = 3, startCol = 2)
  writeData(wb, "Submission", "Indicator 12.3.1", startRow = 4, startCol = 2)
  writeData(wb, "Submission", "Target 12",   startRow = 5, startCol = 2)
  
  writeDataTable(wb, "Submission",  Sub, startRow = 6, startCol = 1)
  
  saveWorkbook(wb,paste(fileSave,paste("SDGDataSubmission_", Sys.Date(), ".xlsx", sep=""), sep=""),overwrite = T)

  ##### ESS SDG Reporting ##############
  Ind402B <- loadWorkbook(paste(fileTemplates,"Indicator402B.xlsx", sep=""))
  Ind402B_Sub <- read.xlsx( Ind402B, sheet = "Sheet1")
  Ind402B_Sub <- as.data.table(Ind402B_Sub [2:dim(Ind402B_Sub)[1],1:3])
  names(Ind402B_Sub) <- c("geographicaream49","M49 Country Name", "Delta (Δ)")
  Ind402B_Sub <-Ind402B_Sub %>%filter(!is.na(geographicaream49))
  
  FLI<- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"geographicaream49",weights,basketn,FLIData) 
  FLI$geographicaream49 <- as.character(FLI$geographicaream49)
  FLI_y1 <- FLI[timepointyears== ComparisonYear[1],c("geographicaream49","Index")] 
  FLI_y2 <- FLI[timepointyears== ComparisonYear[2],c("geographicaream49","Index")]
  
  setnames(FLI_y1, old=c("geographicaream49","Index"), new= c("geographicaream49",ComparisonYear[1]))
  setnames(FLI_y2, old=c("geographicaream49","Index"), new= c("geographicaream49",ComparisonYear[2]))
  
  deltay1_y2 <- merge(FLI_y1,FLI_y2, by= c("geographicaream49"),   all.x = TRUE)
  deltay1_y2[,deltay1_y2:= deltay1_y2[,ComparisonYear[2],with=F]/deltay1_y2[,ComparisonYear[1],with=F]]
  deltay1_y2[,deltay1_y2 := round(.SD,3), .SDcols="deltay1_y2"]
  
  DeltaTab <- merge(Ind402B_Sub,deltay1_y2, by= c("geographicaream49"),   all.x = TRUE)
  DeltaTab[,"Delta (Δ)":=deltay1_y2]
  DeltaTab$geographicaream49 <- as.numeric(DeltaTab$geographicaream49)
  DeltaTab <- DeltaTab[order(geographicaream49),]

  
  writeDataTable(Ind402B, "Sheet1", as.data.table(ComparisonYear), startRow = 211, startCol = 1, withFilter = FALSE, tableStyle = "none")
  writeDataTable(Ind402B, "Sheet1",  DeltaTab[,c("geographicaream49", "M49 Country Name","Delta (Δ)")], startRow = 2, startCol = 1)
  
  saveWorkbook(Ind402B,paste(fileSave,paste("Indicator402B_", Sys.Date(), ".xlsx", sep=""), sep=""),overwrite = T)
  
}
