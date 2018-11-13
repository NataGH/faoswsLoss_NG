
#Production plus imports
lossData <-  merge(prod_imports,lossProtected,  by= keys_lower, all.y= TRUE)
lossData[, loss_per_clean := (value_measuredelement_5016/value_measuredelement_5510)]
lossData[, loss_per_clean_pi := (value_measuredelement_5016/prod_imports)]
lossData[, per_diff :=loss_per_clean-loss_per_clean_pi]

BigPos <- unique(lossData[!is.na(per_diff) & per_diff>1,"geographicaream49",with=F])

Pos <- unique(lossData[!is.infinite(per_diff) & !is.na(per_diff) & per_diff>0,"geographicaream49",with=F])
BigNeg <- unique(lossData[!is.infinite(per_diff) & !is.na(per_diff) & per_diff< 0,"geographicaream49",with=F])


lossData$loss_per_clean <-round(lossData$loss_per_clean,3)
lossData$loss_per_clean_pi <-round(lossData$loss_per_clean_pi,3)
lossData$per_diff <-round(lossData$per_diff,3)

comodities = lossData[loss_per_clean > 1 &  per_diff>.01,c("geographicaream49","measureditemcpc"),with=F]
comodities[, combp := paste(geographicaream49,measureditemcpc, sep=";")]
comb <- unique(comodities$combp)
unique(comodities$geographicaream49)

CountryGroup[geographicaream49 %in% unlist(BigPos) , ]
library(openxlsx)

for(t in unique(comodities$geographicaream49)){
  wb <- createWorkbook()
  for(i in unlist(unique(comodities[geographicaream49== t,"measureditemcpc",with=F]))){
    addWorksheet(wb, as.character(i))
    check1 <- lossData[measuredelement == 5016 &geographicaream49 == t & measureditemcpc == unname(unlist(i)) ,]
    check2 <- timeSeriesDataToBeImputed2[  geographicaream49 == t & measureditemcpc == unname(unlist(i)),]
    check3 <- merge(check1,check2[,c(keys_lower,"loss_per_clean"),with=F], by= keys_lower,all.y=T)
    writeDataTable(wb, as.character(i), check3, startRow = 1, startCol = 1 ,withFilter = FALSE)  
    }

  saveWorkbook(wb, file =  paste("comparison_", t,".xlsx",sep=""), overwrite = TRUE)
 
}

ConvFactor1  <- ReadDatatable('flw_lossperfactors_')
#############################################

CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops     <- ReadDatatable("fcl2cpc_ver_2_1")
Crops_dict <- setDT(unique(FAOCrops[,c("crop", "measureditemcpc"),with=F]))
setkey(Crops_dict,measureditemcpc)
key(Crops_dict)

comodities2 <- comodities  
comodities2 <- merge(comodities2,Crops_dict, by=("measureditemcpc"))
comodities2 <- merge(comodities2,CountryGroup[,c("geographicaream49","country"),with=F], by=("geographicaream49"))
comodities2[, combp := paste(country,crop, sep=";")]
comb <- unique(comodities2$combp)
  
test <- markov[geographicaream49 == 104 &  measureditemcpc=="0142",]
test2 <-test[1,]
test2$fsc_location <- "SWS" 
test2$loss_per_clean <- .10


###
r <-as.list(keys_lower)
test2 <- as.data.table(test2)
r <- paste( r,sep=";")
rowSums(.SD, na.rm = TRUE), .SDcols=c("value_measuredelement_5510","value_measuredelement_5610")]
r <- function(...){
  paste(...,sep=";")
}
r(keys_lower)


