mean(timeSeriesDataToBeImputed_5126$Value)
r <- timeSeriesDataToBeImputed_5126
r$measureditemcpc <-  r$measuredItemSuaFbs
rr <- r[ timePointYears==2016,"Value",with=F]
mean(rr$Value,na.rm=TRUE)
tt <- merge(r,fbsTree, by=c("measureditemcpc"))
tt%>%
  group_by_(.dots = list("gfli_basket")) %>%
  dplyr:: summarise(Sum_p0q0 = mean(Value, na.rm = TRUE))
tt %>% filter(Value >.6)  
#tt2 <- DataForIndex
Losses <-timeSeriesDataToBeImputed_5126

SOFA_data <- as.data.table(DataForIndex %>%
  group_by_(.dots = list("gfli_basket","sdg_regions")) %>%
  dplyr:: summarise(mean_p0q0 = mean(value_measuredelement_5126, na.rm = TRUE)) )

SOFA_data2 <- as.data.table(DataForIndex %>%
  group_by_(.dots = list("gfli_basket","sdg_regions")) %>%
  dplyr:: summarise(min_p0q0 = min(value_measuredelement_5126, na.rm = TRUE)))

SOFA_data3 <- as.data.table(DataForIndex %>%
  group_by_(.dots = list("gfli_basket","sdg_regions")) %>%
  dplyr:: summarise(min_p0q0 = max(value_measuredelement_5126, na.rm = TRUE)))

SOFA_data <- as.data.table(rbind(SOFA_data,SOFA_data3,fill=TRUE))
write.csv(SOFA_data,"SOFA_data_regionagg.csv")
write.csv(GlobalfoodLoss,"SOFA_GlobalfoodLoss.csv")
write.csv(RegionalLossIndex_sdg ,"SOFA_FLP_SDG_region_m49_l2.csv")
write.table(rr,"CommodityGroups.csv",sep = "\t ','")

convf <-merge(ConvFactor1, CountryGroup, by= c("geographicaream 49"))
names(NutrientQuery) <- tolower(names(NutrientQuery) )
NutrientQuery1 <- merge(NutrientQuery, CountryGroup[,c("geographicaream49","country"),with=F], by= c("geographicaream49"))
NutrientQuery1 <- merge(NutrientQuery1,FAOCrops[,c( "crop", "measureditemcpc"),with=F], by= c("measureditemcpc"))

write.csv(convf ,"SOFA_ConvFactor1.csv")
write.csv(NutrientQuery1 ,"SOFA_Nutrients.csv")
## check on oceania and west africa...

write.csv(GlobalfoodLoss ,"SOFA_FLP_global.csv")
write.csv(FoodLossIndex_inc ,"SOFA_FLP_WorldBankInc.csv")
write.csv(FoodLossIndex_sofa  ,"SOFA_FLP_SoFA_agg.csv")
write.csv(FoodLossIndex_gfli ,"SOFA_FLP_ComBasket.csv")
write.csv(RegionalLossIndex_sdg ,"SOFA_FLP_SdgRegion.csv")


