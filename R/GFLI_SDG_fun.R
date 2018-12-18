#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' @export GFLI_SDG_fun

GFLI_SDG_fun <- function(BaseYear,keys_lower,aggregation,basket,basketKeys,DataForIndex){
    # Description:
    #
    
    # inputs:
    #	Aggregation
    # weights 
    # basket
    

  agg <- FALSE
  ############ Index Calculations #############
  ## The decision to calculate and set the weights and basket outside of the function was done to ensure the same choices across all calculations
  # of the FLI and the different aggregations
  
  
  ##### Commodity Groupings - 2013 ####
  ComBasket  <- merge(DataForIndex,basket[,basketKeys,with=FALSE], by=basketKeys, all.y= TRUE )
  if( aggregation == "geographicaream49"){
    aggregationName = "m49_region"
  }
  if(length(grep("code" ,aggregation))>0){
    aggregationName = sub("code", "region", aggregation)
    if(length(grep("sdg" ,aggregation))>0){
      aggregationName = "sdg_regions"
    }
  }
  if(length(grep("agg" ,aggregation))>0){
    aggregationName =aggregation
  }
  if(length(grep("gfli" ,aggregation))>0){
    aggregationName =aggregation
  }

 
  ###### Weights and GFLI Calculation ######### 
  if(aggregation == "WORLD"){
    aggregation <- "geographicaream49"
    aggregationName = "m49_region"
    agg <-  TRUE
  }
  NameConv <-  na.omit(unique(ComBasket[,c(aggregation,aggregationName),with=F]))
  
  ### By Country #####
  SumP0Qt = ComBasket  %>%
    group_by_(.dots = list(aggregation,"timepointyears")) %>%
    dplyr:: summarise(Sum_p0qt = sum(l0ptqt, na.rm = TRUE))
  
  Sump0q0 = ComBasket  %>%
    group_by_(.dots = list(aggregation,"timepointyears")) %>%
    dplyr:: summarise(Sum_p0q0 = sum(p0q0, na.rm = TRUE)) %>%
    filter(timepointyears == as.numeric(BaseYear[2]) - 1)
  
  foodLossIndex = data.table( 
    merge(SumP0Qt,Sump0q0[,c(aggregation,"Sum_p0q0"),with=F],
          by = c(aggregation),
          all.x = TRUE))
  
  
  foodLossIndex[, FLP := (Sum_p0qt/Sum_p0q0)] 
  foodLossIndex <- foodLossIndex %>% filter(!is.na(noquote(aggregation)))
  
  foodLossIndexB <- foodLossIndex%>%
    group_by_(.dots = list(aggregation,"timepointyears")) %>%
    filter(timepointyears == as.numeric(BaseYear[2]) - 1)
  
  foodLossIndexB[, FLP_t0 := FLP]
  
  foodLossIndex <-data.table(merge(foodLossIndex, foodLossIndexB[,c(aggregation,"FLP_t0"),with=F], 
        by = c(aggregation),
        all.x = TRUE))

  foodLossIndex[, Index := (FLP/FLP_t0)*100] 
  foodLossIndex[, FLP_t0 := NULL]
  
  # index of aggregation column which is na
  where.na <- which(!is.na(foodLossIndex[ ,aggregation,with=F ]))
  foodLossIndex <-  data.table(foodLossIndex[where.na])
  if(length(grep("region" ,aggregationName))>0){ 
    foodLossIndex <- join(foodLossIndex,NameConv, by=aggregation)
  }  
  names(foodLossIndex)[names(foodLossIndex) == aggregationName] <- "region_name"
  names(foodLossIndex)[names(foodLossIndex) == aggregation] <- "region_code"
  
  if(agg){
    ### Global #####
    GFLI_WeightD <- ComBasket  %>%
      group_by(timepointyears) %>%
      dplyr:: summarise(Sum_p0q0 = sum(p0q0, na.rm = TRUE))%>%
      filter(timepointyears == as.numeric(BaseYear[2]) - 1)
  
    GFLI_Weight <- data.table( ComBasket  %>%
                                 group_by_(.dots = list( "geographicaream49","timepointyears")) %>%
                                 dplyr:: summarise(Sum_p0q0 = sum(p0q0, na.rm = TRUE))%>%
                                 filter(timepointyears == as.numeric(BaseYear[2]) - 1)
                               )
    GFLI_Weight[, GFLI_W:= Sum_p0q0/GFLI_WeightD$Sum_p0q0]
    GFLI_Weight[, timepointyears:= NULL]
    names(GFLI_Weight)[names(GFLI_Weight) == aggregation] <- "region_code"
  
    foodLossIndex = data.table(
      merge(foodLossIndex,GFLI_Weight,
            by = c("region_code"),
            all.x = TRUE))
  
    foodLossIndex[, GFLI_N := Index*GFLI_W]
    foodLossIndex[, GFLP_N :=  FLP*GFLI_W]

  
    # GFLI2 <- foodLossIndex %>%
    #   group_by(timepointyears) %>%
    #   dplyr:: summarise(Sum_GFLI_N = sum(GFLI_N, na.rm = TRUE))
    # 
    # GFLI2W <- foodLossIndex %>%
    #   group_by(timepointyears) %>%
    #   dplyr:: summarise(Sum_GFLI_D = sum(GFLI_W, na.rm = TRUE))
    # 
    # GlobalfoodLossIndex= data.table(
    #   merge(GFLI2,GFLI2W,
    #         by = c("timepointyears"),
    #         all.x = TRUE))
    # GlobalfoodLossIndex[,Index :=(Sum_GFLI_N/Sum_GFLI_D)]
    
    ###
    GFLP2 <-  data.table(foodLossIndex %>%
      group_by(timepointyears) %>%
      dplyr:: summarise(Sum_GFLP_N = sum(GFLP_N, na.rm = TRUE)))
    
    GFLP2W <-  data.table(foodLossIndex %>%
      group_by(timepointyears) %>%
      dplyr:: summarise(Sum_GFLP_D = sum(GFLI_W, na.rm = TRUE)))
    
    GlobalfoodLossPercent= data.table(
      merge(GFLP2,GFLP2W,
            by = c("timepointyears"),
            all.x = TRUE))
    ##
  
    
    GlobalfoodLossPercent[,GFLP :=(Sum_GFLP_N/Sum_GFLP_D)]
    GLPB <- GlobalfoodLossPercent[(timepointyears == as.numeric(BaseYear[2]) - 1),]
    GlobalfoodLossPercent[,Index :=(GFLP/GLPB$GFLP)*100]
    GlobalfoodLossPercent[,geographicaream49:=1]
    
    GlobalfoodLossPercent <- subset(GlobalfoodLossPercent,
           select = c("geographicaream49", "timepointyears","Sum_GFLP_N", "Sum_GFLP_D", "GFLP", "Index"))
             
    setnames(GlobalfoodLossPercent, old=c("geographicaream49", "timepointyears","Sum_GFLP_N", "Sum_GFLP_D", "GFLP", "Index"), 
                                    new=c("geographicaream49", "timepointyears", "Sum_p0qt", "Sum_p0q0", "FLP","Index"))
    foodLossIndex=GlobalfoodLossPercent
    foodLossIndex[ ,region_name := "World"]
    names(foodLossIndex)[names(foodLossIndex) == aggregation] <- "region_code"

  }
#GlobalfoodLossIndex=GlobalfoodLossIndex,RegionalfoodLossIndex=regionalLossIndex,
return(foodLossIndex)
}

