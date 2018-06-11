#' Part of the FAO Loss Module
#' 
#' @author Alicia English 
#' @export reporting 




reporting <- function(ReportingYear,ComparisonYear,BaseYear){
  
  library(openxlsx)
  library(tidyr)
  library(dplyr)
  # 
  fileTemplates <- "C:\\Users\\ENGLISHA\\Documents\\faoswsLossa\\SDG12_3\\Excel\\"
  fileSave <- readline("Where would you like to save the file: ")  
  
  ReportingYr <- as.numeric(max(ReportingYear))
  
  ##### Official SDG Reporting ##############
  wb <- loadWorkbook(paste(fileTemplates,"SDGDataRequest_02Feb2017_original.xlsx", sep=""))
  Sub <- read.xlsx(wb, sheet = "Submission")
  HeadingsKeep1 <- Sub[5,]
  Sub <- as.data.table(Sub[6:dim(Sub)[1],])
  colnames(Sub) <- gsub(" ", ".",unlist(HeadingsKeep1))
  HeadingsKeep <- colnames(Sub)
  Sub$"Observation.value" <- as.numeric(Sub$"Observation.value")
  Sub$"Time.Period.(Year)" <- as.numeric(Sub$"Time.Period.(Year)")
  Sub[,'Time.Period.(Year)' := ReportingYr]
  Sub[,'Disaggregation.classification' :=  ComBasketN ]
  Sub$`Time.Period.(Year)` <- ReportingYr
  RowOrder <-Sub$Reference.area.code
  
  # GFLI
  GlobalfoodLoss <- GFLI_SDG_fun(BaseYear,keys_lower,"WORLD",basket,basketKeys,DataForIndex) 
  Sub[Reference.area.code == 1, "Observation.value"] = GlobalfoodLoss[timepointyears == ReportingYear,Index] # GFLI
  Sub$Reference.area.code <- as.character(Sub$Reference.area.code)
  Ref <- colnames(CountryGroup)
  
  for(ii in 1:(length(Ref)-1)){
    if(Ref[ii] != "fao_operationalcoverage"){
      foodLossIndex <- GFLI_SDG_fun(BaseYear,keys_lower,Ref[ii],basket,basketKeys,DataForIndex)
      Index_RY <- foodLossIndex[timepointyears == ReportingYear, ]
      Sub <- merge(Sub,Index_RY,by.x = c("Reference.area.code"), by.y = c(Ref[ii]),all.x = TRUE)
      Sub[Reference.area.code %in% unlist(Index_RY[,Ref[ii],with=F]),Observation.value := Index]
      Sub <- Sub[,HeadingsKeep,with=F]
  }}
  
  Sub <- Sub %>% slice(match(RowOrder,Reference.area.code))
  writeData(wb, "Submission", Sys.Date()   , startRow = 1, startCol = 2)
  writeData(wb, "Submission", "UNFAO",       startRow = 2, startCol = 2)
  writeData(wb, "Submission", "Target 12.3", startRow = 3, startCol = 2)
  writeData(wb, "Submission", "Indicator 12.3.1", startRow = 4, startCol = 2)
  writeData(wb, "Submission", "Target 12",   startRow = 5, startCol = 2)
  
  writeDataTable(wb, "Submission",  Sub, startRow = 6, startCol = 1)
  
  saveWorkbook(wb,paste(fileSave,paste("SDGDataSubmission_", Sys.Date(), ".xlsx", sep=""), sep=""),overwrite = T)

  ##### ESS SDG Reporting ##############
  Ind402B <- loadWorkbook(paste(fileTemplates,"Indicator402B_original.xlsx", sep=""))
  Ind402B_Sub <- read.xlsx( Ind402B, sheet = "IndexChange")
  Ind402B_Sub <- as.data.table(Ind402B_Sub[10:dim(Ind402B_Sub)[1],1:5])
  names(Ind402B_Sub) <- c("geographicaream49","M49 Country Name",ComparisonYear[1],ComparisonYear[2], "Delta (Δ)")
  Ind402B_Sub <-Ind402B_Sub %>%filter(!is.na(geographicaream49))
  
  FLI<- GFLI_SDG_fun( BaseYear,keys_lower,"geographicaream49",basket,basketKeys,DataForIndex) 
  FLI$geographicaream49 <- as.character(FLI$geographicaream49)
  FLI_y1 <- FLI[timepointyears== ComparisonYear[1],c("geographicaream49","Index")] 
  FLI_y2 <- FLI[timepointyears== ComparisonYear[2],c("geographicaream49","Index")]
  
  setnames(FLI_y1, old=c("geographicaream49","Index"), new= c("geographicaream49",paste(ComparisonYear[1],'a',sep="")))
  setnames(FLI_y2, old=c("geographicaream49","Index"), new= c("geographicaream49",paste(ComparisonYear[2],'a',sep="")))
  
  deltay1_y2 <- merge(FLI_y1,FLI_y2, by= c("geographicaream49"),   all.x = TRUE)
  deltay1_y2[,deltay1_y2:= deltay1_y2[,paste(ComparisonYear[2],'a',sep=""),with=F]/deltay1_y2[,paste(ComparisonYear[1],'a',sep=""),with=F]]
  deltay1_y2[,deltay1_y2 := round(.SD,3), .SDcols="deltay1_y2"]
  
  DeltaTab <- merge(Ind402B_Sub,deltay1_y2, by= c("geographicaream49"),   all.x = TRUE)
  DeltaTab[,"Delta (Δ)":=deltay1_y2]
  DeltaTab[,ComparisonYear[1]:=  DeltaTab[[paste(ComparisonYear[1],'a',sep="")]]]
  DeltaTab[,ComparisonYear[2]:=  DeltaTab[[paste(ComparisonYear[2],'a',sep="")]]]
  DeltaTab$geographicaream49 <- as.numeric(DeltaTab$geographicaream49)
  DeltaTab <- DeltaTab[order(geographicaream49),]
  DeltaTab[,(ComparisonYear) := round(.SD,3), .SDcols=ComparisonYear]
  Report_Ctrys <- DeltaTab[,c("geographicaream49", "M49 Country Name",ComparisonYear[1],ComparisonYear[2],"Delta (Δ)"),with=F]

  BasketLosses <- merge(basket,DataForIndex, by=c(keys_lower[1],keys_lower[3]))
  BasketLosses <- BasketLosses[,c("measureditemcpc","geographicaream49","timepointyears","value_measuredelement_5126"),with=F]
  BasketLosses <- dcast(BasketLosses, measureditemcpc+geographicaream49 ~ timepointyears, value.var= "value_measuredelement_5126")
  BasketLossesC  <-BasketLosses[,names(BasketLosses)[names(BasketLosses) %in% c(keys_lower,ComparisonYear[1]:ComparisonYear[2])]]
  Basket_Losses_yr <- merge(basket,BasketLossesC, by=c(keys_lower[1],keys_lower[3]))
  names(Basket_Losses_yr)[names(Basket_Losses_yr) %in% c(ComparisonYear[1]:ComparisonYear[2])] <- paste("value_measuredelement_5126_",ComparisonYear[1]:ComparisonYear[2], sep="")
  Basket_Losses_yr $geographicaream49 <- as.character(Basket_Losses_yr$geographicaream49)
  #Basket_Losses_yr <- merge(Basket_Losses_yr ,CountryGroup[,c("geographicaream49","Country")], by=c("geographicaream49"), all.x =TRUE)
  
  ### FAO member Countries 
  FAO_regions_FLI  <- GFLI_SDG_fun( BaseYear,keys_lower,"fao_region",basket,basketKeys,DataForIndex) 
  FAO_regions_FLI2 <- FAO_regions_FLI %>% filter( timepointyears %in% c(ComparisonYear[1]:ComparisonYear[2]))
  FAO_regions_FLI2a <- as.data.table(dcast(FAO_regions_FLI2, fao_region ~timepointyears, value.var="Index"))
  FAO_regions_FLI2a[,"Delta (Δ)":= FAO_regions_FLI2a[,ComparisonYear[2],with=F]/FAO_regions_FLI2a[,ComparisonYear[1], with=F]]
  FAO_regions_FLI2a[,"geographicaream49":= ""]
  Report_Regions <- FAO_regions_FLI2a[,c("geographicaream49", "fao_region",ComparisonYear[1],ComparisonYear[2],"Delta (Δ)"),with=F]
  setnames(Report_Regions,old= "fao_region", new = "Index Coverage")
  
  FAO_globalCov_FLI <- GFLI_SDG_fun( BaseYear,keys_lower,"fao_operationalcoverage",basket,basketKeys,DataForIndex) 
  FAO_globalCov_FLI2 <- FAO_globalCov_FLI %>% filter( fao_operationalcoverage == 1 & timepointyears %in% c(ComparisonYear[1]:ComparisonYear[2]))
  FAO_globalCov_FLI2a <- as.data.table(dcast(FAO_globalCov_FLI2 ,fao_operationalcoverage ~ timepointyears, value.var="Index"))
  FAO_globalCov_FLI2a[,"Delta (Δ)":= FAO_globalCov_FLI2a[,ComparisonYear[2],with=F]/FAO_globalCov_FLI2a[,ComparisonYear[1], with=F]]
  FAO_globalCov_FLI2a[,"geographicaream49":= ""]
  FAO_globalCov_FLI2a[,"fao_operationalcoverage":= "FAO Operational Countries (149)"]
  Report_FAO_G <- FAO_globalCov_FLI2a[,c("geographicaream49", "fao_operationalcoverage",ComparisonYear[1],ComparisonYear[2],"Delta (Δ)"),with=F]
  setnames(Report_FAO_G,old= "fao_operationalcoverage", new = "Index Coverage")
  
  GFLI <- GFLI_SDG_fun( BaseYear,keys_lower,"WORLD",basket,basketKeys,DataForIndex) 
  GFLI2 <- GFLI %>% filter(timepointyears %in% c(ComparisonYear[1]:ComparisonYear[2]))
  GFLI2a <- as.data.table(dcast(GFLI2 , WORLD ~ timepointyears, value.var="Index"))
  GFLI2a[,"Delta (Δ)":= GFLI2a[,ComparisonYear[2],with=F]/GFLI2a[,ComparisonYear[1], with=F]]
  GFLI2a[,"geographicaream49":= ""]
  Report_G <-  GFLI2a[,c("geographicaream49", "WORLD",ComparisonYear[1],ComparisonYear[2],"Delta (Δ)"),with=F]
  setnames(Report_G,old= "WORLD", new = "Index Coverage")
  
  ReportR <- rbind(Report_G, Report_FAO_G, Report_Regions)
  ReportR[,(ComparisonYear) := round(.SD,3), .SDcols=ComparisonYear]
  
  ## Metadata
  ModelData<- ReadDatatable("lossmodelruns")
  Ind402B_Meta <- read.xlsx( Ind402B, sheet = "Metadata")
  Ind402B_Meta <- as.data.table(Ind402B_Meta[1:dim(Ind402B_Meta)[1],1:2])
  Ind402B_Meta[Ind402B_Meta$Value =="Base Year","X2"] <- as.character(as.numeric(BaseYear[2])-1)
  Ind402B_Meta[Ind402B_Meta$Value =="SWS Model Version","X2"] <- ModelData$modelversion[ModelData$daterun==max(ModelData$daterun)][1]
  Ind402B_Meta[Ind402B_Meta$Value =="Date of Model Run","X2"] <- max(ModelData$daterun)
  
  #writeDataTable(Ind402B, "IndexChange", as.data.table(ComparisonYear), startRow = 211, startCol = 1, withFilter = FALSE, tableStyle = "none")
  writeDataTable(Ind402B, "IndexChange", ReportR, startRow = 2, startCol = 1 ,withFilter = FALSE)
  
  writeDataTable(Ind402B, "IndexChange",Report_Ctrys , startRow = 10, startCol = 1)
  writeDataTable(Ind402B, "BasketCommodities",  Basket_Losses_yr, startRow = 1, startCol = 1)
  writeDataTable(Ind402B, "Metadata",   Ind402B_Meta , startRow = 1, startCol = 1)
  
  saveWorkbook(Ind402B,paste(fileSave,paste("Indicator402B_", Sys.Date(), ".xlsx", sep=""), sep=""),overwrite = T)
  
}
