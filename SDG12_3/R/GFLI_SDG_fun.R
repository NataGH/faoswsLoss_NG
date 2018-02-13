#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 

GFLI_SDG_fun <- function(selectedYear,BaseYear,keys_lower,aggregation,weights,basketn,FLIData){
    # Description:
    #
    
    # inputs:
    #	Aggregation
    # weights 
    # basket
    
  #production <- getProductionData() 
  agg <- FALSE
  ##### Commodity Groupings - 2013 ####
  
  if(basketn == "top2perhead_byCtry"){
    Top10perctry <- FLIData %>%
    arrange(geographicaream49, -p0q0) 
  
    basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
    basket <- basket %>% filter(!is.na(gfli_basket))
    basketKeys <- c('geographicaream49', "measureditemcpc")
    ComBasketN  <- 'Production Value- Top 10 by country'
    basket[,basketname := ComBasketN]
    basket[,protected := FALSE] 
  }
  
  if(basketn == "top2perhead_Globatop10"){
    Top10Global<- FLIData %>%
      filter(!is.na(foodGroupName))%>%
      arrange( -p0q0)
  
      ItemsBasket <- unique(Top10Global[ ,head(.SD, 2), by= c('gfli_basket')]$measureditemcpc)
      basket <- FLIData %>% filter(measureditemcpc %in%  ItemsBasket)
      basketKeys <- ( "measureditemcpc")
      ComBasketN  <- 'Production Value- Top 10 by World'
      basket[,basketname := ComBasketN]
      basket[,protected := FALSE] 
  }
  if(basketn == "calories"){
    Globalkcal1 <- ReadDatatable("top10_foodsupplykcal")
    Globalkcal1$item_code <- as.character(Globalkcal1$item_code)
    Globalkcal1 <- merge(Globalkcal1,fbsTree,by.x= c('item_code'), by.y = c("fbsID4"), type ='left',match='all')
    Globalkcal1 <- Globalkcal1[order(-Globalkcal1$value),]
    
    ItemsBasket <- unique(Globalkcal1[ ,head(.SD, 2), by= c('gfli_basket')]$measureditemcpc)
    basket <- FLIData %>% filter(measureditemcpc %in%  ItemsBasket)
    basketKeys <- ("measureditemcpc")
    ComBasketN  <- 'Caloric Value- Top 10 by World'
    basket[,basketname := ComBasketN]
    basket[,protected := FALSE] 
    
  }  
  
  basket <- basket[,c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","protected")]
   
  ### Save the Basket Selection to the sws ####
  if(savesws){
    #write.table(basket ,'Commodbasket.csv', sep=",", row.names=FALSE)
    names(basket) <- tolower(names(basket))
    ## Delete
    table = "sdg123_commoditybasket"
    changeset <- Changeset(table)
    newdat <- ReadDatatable(table, readOnly = FALSE)
    AddDeletions(changeset, newdat)
    Finalise(changeset)
    ## Add
    AddInsertions(changeset,  basket[,c("geographicaream49", "gfli_basket","measureditemcpc","itemname","qty_avey1y2","intprice","p0q0","basketname","protected")])
    Finalise(changeset)
  }
  
  DataForIndex <- join(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , FLIData, by =c("measureditemcpc","geographicaream49"),type= 'left', match='all')
  DataForIndex$l0ptqt =0
  DataForIndex[,l0ptqt:=value_measuredelement_5126*p0q0,with=T]
   
  
  ComBasket  <- merge(DataForIndex,unique(basket[,basketKeys,with=FALSE]), by=basketKeys, all.y= TRUE )
  
 
  ###### Weights and GFLI Calculation ######### 
  if(aggregation == "WORLD"){
    aggregation <- "geographicaream49"
    agg <-  TRUE
    }
  ### By Country #####
  SumP0Qt = ComBasket  %>%
    group_by_(.dots = list(aggregation,"timepointyears")) %>%
    dplyr:: summarise(Sum_p0qt = sum(l0ptqt, na.rm = TRUE))
  
  
  Sump0q0 = SumP0Qt %>%
    group_by_(.dots = list(aggregation,"timepointyears"))  %>%
    mutate(Sum_p0q0 = Sum_p0qt) %>%
    filter(timepointyears == as.numeric(BaseYear[2]) - 1)
  
  foodLossIndex = data.table( 
    merge(SumP0Qt,Sump0q0[,c(1,4)],
          by = c(aggregation),
          all.x = TRUE))
  
  
  foodLossIndex[, Index := (Sum_p0qt/Sum_p0q0)*100] 
  foodLossIndex <- foodLossIndex %>% filter(!is.na(noquote(aggregation)))

  # index of aggregation column which is na
  where.na <- which(!is.na(foodLossIndex[ ,aggregation,with=F ]))
  foodLossIndex <- foodLossIndex[where.na]
  
  if(agg){
  ### Global #####
  GFLI_WeightD <- ComBasket  %>%
    group_by(timepointyears) %>%
    dplyr:: summarise(Sum_p0q0 = sum(p0q0, na.rm = TRUE))%>%
    filter(timepointyears == 2005)

  GFLI_Weight <- data.table( Sump0q0[,c(1,4)])
  GFLI_Weight[, GFLI_W:= Sum_p0q0/GFLI_WeightD$Sum_p0q0]
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
  foodLossIndex=GlobalfoodLossIndex
  aggregation = "WORLD"
  }
#GlobalfoodLossIndex=GlobalfoodLossIndex,RegionalfoodLossIndex=regionalLossIndex,
return(foodLossIndex)
}

