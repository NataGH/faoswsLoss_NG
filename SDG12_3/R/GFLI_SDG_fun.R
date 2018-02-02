#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 

GFLI_SDG_fun <- function(selectedYear,keys_lower,aggregation,weights,basketN,production){
    # Description:
    #
    
    # inputs:
    #	Aggregation
    # weights 
    # basket
    
  #production <- getProductionData() 
  #----  Data In ------------------------------------------
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
  fbsTree[foodgroupname %in% c(2905,2911), GFLI_Basket :='Cereals & Pulses',]
  fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
  fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
  fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
  fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",
  

  
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
                       select = c(keys_lower,"value_measuredelement_5510")) %>% filter(timepointyears >= 2004 & timepointyears <= 2006)

  aveP <- ProdQtySWS[,Qty_ave0406 := mean(value_measuredelement_5510),by = c("geographicaream49",'measureditemcpc')]
  aveP <- unique(aveP[,c("geographicaream49",'measureditemcpc','Qty_ave0406'),with=F])
  
  aveP$measureditemcpc <- lapply(aveP$measureditemcpc, as.character)
  aveP$measureditemcpc<- addHeadingsCPC(aveP$measureditemcpc)
  
  FLIData <-  merge(aveP,intPrice2005Selected, by.x = c('measureditemcpc'), by.y = c('measureditemcpc'),all.x = F, all.y = F)
  FLIData[, P0Q0 := Qty_ave0406 *intprice,]
  FLIData <- join(FLIData ,fbsTree , by = c('measureditemcpc'),type= 'left', match='all')
  
  FLIData <- join(FLIData,CountryGroup[,c('Country','isocode', 'geographicaream49','sdg_regions')] , by = c('geographicaream49'),type= 'left', match='all')
  
  ##### Commodity Groupings - 2013 ####
  
  if(basketN == "top2perhead_byCtry"){
    Top10perctry <- FLIData %>%
    arrange(geographicaream49, -P0Q0) 
  
    basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','GFLI_Basket')]
    basket <- basket %>% filter(!is.na(GFLI_Basket))
    basketKeys <- c('geographicaream49', "measureditemcpc")
    ComBasketN  <- 'Production Value- Top 10 by country'
    #cast(Top10perctry,geographicaream49 ~measureditemcpc)
    #write.csv(Top10perctry, file='LossBasket_top10byctryB.csv')
  }
  
  if(basketN == "top2perhead_Globatop10"){
    Top10Global<- FLIData %>%
      filter(!is.na(foodGroupName))%>%
      arrange( -P0Q0)
  
      ItemsBasket <- unique(Top10Global[ ,head(.SD, 2), by= c('GFLI_Basket')]$measureditemcpc)
      basket <- FLIData %>% filter(measureditemcpc %in%  ItemsBasket)
      basketKeys <- ( "measureditemcpc")
      ComBasketN  <- 'Production Value- Top 10 by World'
  }
  if(basketN == "calories"){
    Globalkcal1 <- ReadDatatable("top10_foodsupplykcal")
    Globalkcal1$item_code <- as.character(Globalkcal1$item_code)
    Globalkcal1 <- merge(Globalkcal1,fbsTree,by.x= c('item_code'), by.y = c("fbsID4"), type ='left',match='all')
    Globalkcal1 <- Globalkcal1[order(-Globalkcal1$value),]
    
    ItemsBasket <- unique(Globalkcal1[ ,head(.SD, 2), by= c('GFLI_Basket')]$measureditemcpc)
    basket <- FLIData %>% filter(measureditemcpc %in%  ItemsBasket)
    basketKeys <- ("measureditemcpc")
    ComBasketN  <- 'Caloric Value- Top 10 by World'
    
  }  
  
  DataForIndex <- join(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , FLIData, by =c("measureditemcpc","geographicaream49"),type= 'left', match='all')
  DataForIndex$l0ptqt =0
  DataForIndex[,l0ptqt:=value_measuredelement_5126*P0Q0,with=T]
   
  
  ComBasket  <- merge(DataForIndex,unique(basket[,basketKeys,with=FALSE]), by=basketKeys, all.y= TRUE )
  
  basket2 <-  merge(basket,FAOCrops[,c("measureditemcpc","crop")], by=("measureditemcpc"), all.x =T)
  S_Ctry <- basket2 %>% filter(geographicaream49 == 643)
  write.csv(S_Ctry, file=paste('LossBasket_top10byctry',unique(S_Ctry$geographicaream49), '.csv', sep=""))
  
  ###### Weights and GFLI Calculation ######### 
  
  SumP0Qt = ComBasket  %>%
    group_by(geographicaream49,timepointyears) %>%
    dplyr:: summarise(Sum_p0qt = sum(l0ptqt, na.rm = TRUE))
  
  
  SumP0Q0 = SumP0Qt %>%
    dplyr::group_by(geographicaream49,timepointyears) %>%
    mutate(Sum_p0q0 = Sum_p0qt) %>%
    filter(timepointyears == 2005)
  
  foodLossIndex = data.table( 
    merge(SumP0Qt,SumP0Q0[,c(1,4)],
          by = c("geographicaream49"),
          all.x = TRUE))
  
  
  foodLossIndex[, Index := (Sum_p0qt/Sum_p0q0)*100] 
  foodLossIndex <- foodLossIndex %>%filter(!is.na(geographicaream49))
  
  
  GFLI_WeightD <- ComBasket  %>%
    group_by(timepointyears) %>%
    dplyr:: summarise(Sum_P0Q0 = sum(P0Q0, na.rm = TRUE))%>%
    filter(timepointyears == 2005)
  
  GFLI_Weight <- data.table( SumP0Q0[,c(1,4)])
  GFLI_Weight[, GFLI_W:= Sum_p0q0/GFLI_WeightD$Sum_P0Q0]
  names(GFLI_Weight)[names(GFLI_Weight)== "geographicaream49"] <- names(foodLossIndex)[names(foodLossIndex)== "geographicaream49"] 
  
  foodLossIndex = data.table( 
    merge(foodLossIndex,GFLI_Weight,
          by = c("geographicaream49"),
          all.x = TRUE))
  
  foodLossIndex[, GFLI_N := Index*GFLI_W]
  
  GFLI2 <- foodLossIndex %>%
    group_by(timepointyears) %>%
    dplyr:: summarise(Sum_GFLI_N = sum(GFLI_N, na.rm = TRUE))
  
  GFLI2W <- foodLossIndex %>%
    group_by(timepointyears) %>%
    dplyr:: summarise(Sum_GFLI_D = sum(GFLI_W, na.rm = TRUE))
  
  GlobalfoodLossIndex= data.table( 
    merge(GFLI2,GFLI2W,
          by = c("timepointyears"),
          all.x = TRUE))
  
  GlobalfoodLossIndex[,GFLI :=(Sum_GFLI_N/Sum_GFLI_D)]

return(list(GlobalfoodLossIndex=GlobalfoodLossIndex,foodLossIndex=foodLossIndex))
}

