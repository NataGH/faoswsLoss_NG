
ConvFactor1[, combo := paste(geographicaream49,measureditemcpc,timepointyears, loss_per_clean,sep=";")]
ConvFactor1 %>% filter(fsc_location %in% c( "sws_total", "wholesupplychain") & reference == "SWS" ) 

oldLoss <- as.data.table(read.xlsx("C:\\Users\\Englisha\\Documents\\faoswsLossa\\data-raw\\Loss_factors\\flw_lossperfactors_Dec2018.xlsx"))
oldLoss <- as.data.table(oldLoss)
oldLoss <- oldLoss[ ! is.na(FAO_CommodityNum), ]

oldLoss$FAO_CommodityNum <- addHeadingsCPC(oldLoss$FAO_CommodityNum) 
oldLoss[,combo := paste(FAO_countryNum,FAO_CommodityNum,Year,sep=";")]


baskets <- ReadDatatable("sdg123_commoditybasket")
baskets[geographicaream49 == '1248',geographicaream49 := 156]
baskets[geographicaream49 %in% c(886,720),geographicaream49 := 887]
baskets[geographicaream49 %in% c(530),geographicaream49 := 531]


CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
CountryGroup$geographicaream49 <- CountryGroup$m49_code
baskets2 <- join(baskets , CountryGroup[,c("geographicaream49","m49_region","sdg_regions","worldbank_income2018_agg")], by = "geographicaream49", type = "left")

baskets2[, combo := paste(itemname, sdg_regions, sep=";")]


bb <- baskets2[, c("gfli_basket","itemname","sdg_regions")] %>%
  filter(! is.na(sdg_regions)) %>%
  group_by(gfli_basket,itemname) %>%
  dplyr:: summarise(n= n())

bc <- baskets2[, c("gfli_basket","itemname","sdg_regions","worldbank_income2018_agg")] %>%
  filter(! is.na(sdg_regions)) %>%
  group_by(worldbank_income2018_agg, gfli_basket,itemname) %>%
  dplyr:: summarise(n= n())

write.table(bc, "baskets_Regions2.csv", sep= ",")




####################################
unique(baskets2$sdg_regions)
bb <- baskets2 %>%
  filter(sdg_regions %in% c( "Central Asia (M49) and Southern Asia (MDG=M49)","Sub-Saharan Africa (M49)") ) %>%
  group_by(gfli_basket,sdg_regions) %>%
  dplyr:: summarise(n= sum(qty_avey1y2, na.rm=TRUE))


bc <- as.data.table(baskets2 %>%
  filter(sdg_regions %in% c( "Central Asia (M49) and Southern Asia (MDG=M49)","Sub-Saharan Africa (M49)")& gfli_basket != "Other") %>%
  group_by(sdg_regions,gfli_basket,measureditemcpc) %>%
  dplyr:: summarise(n=  n()))

write.table(bc, "baskets_Regions2_.csv", sep= ",")
bc[order(sdg_regions,gfli_basket,-n),] 
