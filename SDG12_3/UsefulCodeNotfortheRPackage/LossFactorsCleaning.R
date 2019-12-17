if(savesws){
  # names(FullSet) <- tolower(names(FullSet))
  # ## Delete
  table ='flw_lossperfactors'
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset, ConvFactor3)
                
  Finalise(changeset)
} 

ConvFactor1  <- ReadDatatable('flw_lossperfactors')





InputData_Out$isocode <- InputData_Out$isocode.x
InputData_Out$cropvariety <- ""
InputData_Out$loss_quantity_ref <- 0
InputData_Out$represents <- ""
InputData_Out$supplymap <- FALSE
InputData_Out$surveyinstrument<- FALSE
InputData_Out$entry_date <- 43344.0


ConvFactor1[ConvFactor1$country == "Democratic Republic Of Congo" ,"country" ] <- "Democratic Republic of the Congo"
ConvFactor1[ConvFactor1$country == "Tanzania" ,"country" ] <- "United Republic of Tanzania"
ConvFactor1[ConvFactor1$country == "Austrailia" ,"country" ] <- "Australia"
ConvFactor1[ConvFactor1$country == "Bolivia" ,"country" ] <- "Bolivia (Plurinational State of)"
ConvFactor1[ConvFactor1$country == "Laos" ,"country" ] <- "Lao People’s Democratic Republic"
ConvFactor1[ConvFactor1$country == "Lao People's Democratic Republic" ,"country" ] <- "Lao People’s Democratic Republic"
ConvFactor1[ConvFactor1$country == "None" ,"country" ] <- "   "
ConvFactor1[ConvFactor1$country == "Russia" ,"country" ] <- "Russian Federation"
ConvFactor1[ConvFactor1$country == "South Korea" ,"country" ] <- "Republic Of Korea"
ConvFactor1[ConvFactor1$country == "United Kingdom"  ,"country" ] <- "United Kingdom of Great Britain and Northern Ireland"
ConvFactor1[ConvFactor1$country == "Timor Leste"  ,"country" ] <- "Timor-Leste"
ConvFactor1[ConvFactor1$country ==  "United States Of America"   ,"country" ] <- "United States of America"
ConvFactor1[ConvFactor1$country ==  "Trinidad And Tobago"   ,"country" ] <- "Trinidad and Tobago"
ConvFactor1[ConvFactor1$country ==  "The former Yugoslav Republic of Macedonia"   ,"country" ] <- "North Macedonia"
ConvFactor1[ConvFactor1$country ==  "China"   ,"country" ] <- "China, Main"
ConvFactor1[ConvFactor1$country ==  "Cote d'Ivoire"   ,"country" ] <- "Cote D'Ivoire"
ConvFactor1[ConvFactor1$country ==  "Syria"  ,"country" ] <- "Syrian Arab Republic" 
ConvFactor1[ConvFactor1$country ==  "Swaziland" ,"country" ] <- "Eswatini"
ConvFactor1[ConvFactor1$country ==  "Venezuela (Bolivarian Republic Of)"  ,"country" ] <- "Venezuela (Bolivarian Republic of)"
ConvFactor1[ConvFactor1$country ==  "Republic Of Korea"  ,"country" ] <- "Republic of Korea"



InputData_Out[tag_datacollection ==  "Food Balance Sheet/Ag. Production Questionnaire", "tag_datacollection" ] <-"FBS/APQ"
InputData_Out[tag_datacollection == "Secondary Sources cited in Documents", "tag_datacollection" ] <- "LitReview"
InputData_Out[tag_datacollection == "FAO Sources", "tag_datacollection" ] <- "SWS"
InputData_Out[tag_datacollection == "National Statistics Yearbook", "tag_datacollection" ] <-  "NationalStatsYearbook"
InputData_Out[tag_datacollection == "FAO Sources", "tag_datacollection" ] <- 
InputData_Out[tag_datacollection == "Secondary Sources cited in Documents", "tag_datacollection" ] <- "NonProtected"
InputData_Out[tag_datacollection == "", "tag_datacollection" ] <- "-"
InputData_Out[tag_datacollection ==  "National Acctounts", "tag_datacollection" ] <-"NationalAcctSys"
InputData_Out[tag_datacollection ==  "Crop Cutting Field Experiment", "tag_datacollection" ] <- "Crop-Cutting"


ConvFactor2 <- InputData_Out[,names(ConvFactor1), with = F]
ConvFactor3 <- rbind(ConvFactor2, ConvFactor1)


ConvFactor3[duplicated(ConvFactor3)==TRUE | duplicated(ConvFactor3, fromLast = TRUE) , supplymap :=TRUE ]
ConvFactor3 <- unique(ConvFactor3)
ConvFactor3$crop <- tolower(ConvFactor3$crop)
ConvFactor3$fsc_location <- tolower(ConvFactor3$fsc_location)
ConvFactor3 %>% filter(supplymap ==TRUE)
ConvFactor3 <- ConvFactor3[order(geographicaream49, measureditemcpc,timepointyears)]
