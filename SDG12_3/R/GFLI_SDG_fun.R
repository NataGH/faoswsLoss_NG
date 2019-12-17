#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 

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
    filter(timepointyears == as.numeric(BaseYear[2]) - 1)

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

  GlobalfoodLossIndex[,Index :=(Sum_GFLI_N/Sum_GFLI_D)]
  GlobalfoodLossIndex[,WORLD :="WORLD"]
  setnames(GlobalfoodLossIndex, old=c("timepointyears", "Sum_GFLI_N", "Sum_GFLI_D", "Index"), 
                                new=c("timepointyears", "Sum_p0qt", "Sum_p0q0", "Index"))
  foodLossIndex=GlobalfoodLossIndex
  aggregation = "WORLD"
  }
#GlobalfoodLossIndex=GlobalfoodLossIndex,RegionalfoodLossIndex=regionalLossIndex,
return(foodLossIndex)
}

