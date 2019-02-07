#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' @export FSC_Markov

#options(warn=-1)
FSC_Markov <- function(RawData,opt,modelEst,selectedYear,CountryGroup,fbsTree){
  # Collapses the supply chain into singular observations at the country level
  # Args:
  #   RawData: Matrix of Conversion Factors
  #   opt: Options for aggregating up to the national level for each country/crop/year
  ##      opt = aveatFSP
  ##      This option averages at each supply chain point for each country,crop,year -  delineated by
  ##      ("farm","storage", "processing", "retail","trader", "transport", "wholesale","whole supply chain", "sws_total")
  ##
  ##      From the FLW_LossPercFactors.xls the column FSC_Location is the column that is to delineate the stages. 
  ##      If there are datapoints that cover multiple stages, the function splits it at the ‘/’ and attributes it to the first stage provided. 
  
  # Returns: 
  #   FullSet
  
  # Adjustments to the raw data
  locations <- c("farm", "wholesupplychain", "sws_total") ## transport","storage", "trader","wholesale", "processing", "retail" <>
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  RawData$country <- tolower(RawData$country)
  RawData$crop <- tolower(RawData$crop)
  RawData$fsc_location<- tolower(RawData$fsc_location)

  names(RawData) <- tolower(names(RawData))

  FullSet <- subset(RawData,
         select = c(keys_lower, "isocode",  "country", "loss_per_clean", "fsc_location")
         )
  
  nro = length(unique(paste(RawData$measureditemcpc,RawData$timepointyears,RawData$geographicaream49, sep=";")))
  FullSet <-   FullSet[1,,] 
  FullSet [,country:='antarctica']
  FullSet <-   FullSet[,loss_per_clean :=0,]
  FullSeta <-   FullSet[1,,] 
  TransitionMatrix = matrix(0,1,length(locations))
  
  #Splits the location to the first observation 
  RawData$fsc_location1 = sapply(strsplit(RawData$fsc_location,"/"), '[', 1)
      
   
  
  
  RawData <- RawData %>% filter(fsc_location1 %in% locations)
  
  # Sets the stage sequence for the markov chain
  colnames(TransitionMatrix) = locations
  count = 1
  #############Filling in the blanks ####################
  if(modelEst){
    SubNational <- RawData %>% filter((!fsc_location1 %in% c("sws_total", "wholesupplychain")) & (timepointyears %in% selectedYear))
    SubNational <- SubNational[loss_per_clean < 0.65,]
    SubNational2 <- merge(SubNational,CountryGroup, by = c("geographicaream49"), all.x = TRUE)
    SubNational2 <- merge(SubNational2,fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)
    ## US & CAN are modeled with Europe and Mexico to LatAM
    SubNational2$est_region <- SubNational2$m49_level2_code
    SubNational2[geographicaream49 %in% c("840", "124"),"est_region"] <- "155"
    SubNational2[geographicaream49 == "484","est_region"] <- "13"
      
    ## data to be estimated ####
    loc2 <- c("farm") #,"transport","storage", "trader","wholesale", "processing", "retail" <>
    maxYear <- as.numeric(format(Sys.Date(), "%Y"))
    SubNat_VCD <- as.data.table(expand.grid(timepointyears = seq(1990,  maxYear, by = 1),
                                            geographicaream49 = as.character(unique(SubNational$geographicaream49)),
                                            measureditemcpc = as.character(unique(SubNational$measureditemcpc)),
                                            locations = as.character(loc2)))
    
    SubNat_VCD$value <- 0 
    SubNat_VCD$locations <- as.character(SubNat_VCD $locations)
    SubNat_VCD2 <- merge(SubNat_VCD,CountryGroup, by = c("geographicaream49"), all.x = TRUE)
    SubNat_VCD2 <- merge(SubNat_VCD2,fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)
    SubNat_VCD2 <- SubNat_VCD2 %>% filter(!is.na(gfli_basket))
    SubNat_VCD$measureditemcpc<- as.character(SubNat_VCD$measureditemcpc)
    SubNat_VCD2$est_region <- SubNat_VCD2$m49_level2_code
    SubNat_VCD2[geographicaream49 %in% c("840", "124"),"est_region"] <- "155"
    SubNat_VCD2[geographicaream49 == "484","est_region"] <- "13"
    
    
    depVar<- "loss_per_clean"
    cluster <- "fsc_location"
    groups <- na.omit(unique(fbsTree[,c("gfli_basket"),with=F]))
    OnlySigCoeff =F
    ## By country estimates, then by regions
   
    
    for(gfliB in 1:dim(groups)[1]){
      print(na.omit(unique(fbsTree[,c("gfli_basket"),with=F]))[gfliB])
      SubNational3_gfli <- SubNational2 %>% filter(gfli_basket %in% na.omit(unique(fbsTree[,c("gfli_basket"),with=F]))[gfliB])
      
      if( dim(SubNational3_gfli)[1]==0){
        next
      }
      # exp_var_ctry <- c("measureditemcpc","timepointyears","geographicaream49","fsc_location1")  
      # formula_expOp <- paste(paste(depVar," ~",sep=""),paste(exp_var_ctry,sep="+", collapse= " + "))
      # mod2_exOp <- lm(as.formula(formula_expOp), data =  SubNational3_gfli )
      # print(summary(mod2_exOp))
      # 
      # ####################### Results #########################################
      # 
      # if(OnlySigCoeff){
      #   coeffSig <- summary(mod2_exOp)$coeff[,4][summary( mod2_exOp)$coeff[,4] <.1]
      #   Inters <- names(summary( mod2_exOp)$coeff[,4][summary( mod2_exOp)$coeff[,4] <.1])
      # }else{
      #   coeffSig <- summary(mod2_exOp)$coeff[,4]
      #   Inters <- names(summary( mod2_exOp)$coeff[,4])
      # }
      # 
      # results <- SubNat_VCD2 %>%filter(gfli_basket %in% na.omit(unique(fbsTree[,c("gfli_basket"),with=F]))[gfliB])
      #   #filter(measureditemcpc %in% sub("measureditemcpc", "", grep("cpc",names(coeffSig), value=TRUE)) &
      # #                                    geographicaream49 %in% sub("geographicaream49", "", grep("geographicaream49",names(coeffSig), value=TRUE)) )
      # 
      # results$intercept <-as.numeric(coefficients(mod2_exOp)[names(coefficients(mod2_exOp)) %in% "(Intercept)"])
      # results$countydummy =0
      # results$cropdummy  =0
      # results$timetrend  =0
      # results$LocFactor =0
      # 
      # for(ind1 in 1:length(unique(grep("geographicaream49",names(coeffSig), value=TRUE)))){
      #   results[geographicaream49 == gsub("geographicaream49","", grep("geographicaream49",names(coeffSig), value=TRUE))[ind1],
      #           countydummy := as.numeric(coefficients(mod2_exOp)[names(coefficients(mod2_exOp)) %in% grep("geographicaream49",names(coeffSig), value=TRUE)[ind1]]),]
      # }
      # for(ind2 in 1:length(unique(grep("measureditemcpc",names(coeffSig), value=TRUE)))){
      #   results[measureditemcpc == gsub("measureditemcpc","", grep("measureditemcpc",names(coeffSig), value=TRUE))[ind2],
      #           cropdummy := as.numeric(coefficients(mod2_exOp)[names(coefficients(mod2_exOp)) %in% grep("measureditemcpc",names(coeffSig), value=TRUE)[ind2]]),]
      # }
      # if(length(grep("timepointyears",names(coeffSig), value=TRUE)) >0){
      #   results[, timetrend :=  mapply(`*`,coefficients(mod2_exOp)[names(coefficients(mod2_exOp)) %in% "timepointyears"], results[ ,"timepointyears",with=F])]
      #   
      # }
      # for(ind3 in 1:length(unique(grep("fsc_location1",names(coeffSig), value=TRUE)))){
      #   results[locations == gsub("fsc_location1","", grep("fsc_location1",names(coeffSig), value=TRUE))[ind3],
      #           LocFactor := as.numeric(coefficients(mod2_exOp)[names(coefficients(mod2_exOp)) %in% grep("fsc_location1",names(coeffSig), value=TRUE)[ind3]]),]
      # }
      # results[,VC_ModelEst := countydummy+cropdummy+timetrend +LocFactor +intercept,]
      # results[VC_ModelEst <0,"VC_ModelEst"] <- 0 
      # tt1 <- results %>%
      #   group_by(locations) %>%
      #   summarise(mean = mean(VC_ModelEst), n = n())
      # print(tt1)
      # 
      ## By country estimates, then by regions
      SubNational3_gfli$geographicaream49 <- as.factor(SubNational3_gfli$geographicaream49)
      SubNational3_gfli$est_region <- as.factor(SubNational3_gfli$est_region)
      exp_var_region <- c("measureditemcpc","est_region","timepointyears","geographicaream49:est_region") #"fsc_location1"
      formula_expOp <- paste(paste(depVar," ~",sep=""),paste(exp_var_region,sep="+", collapse= " + "))
      mod2_exOp_reg <- lm(as.formula(formula_expOp), data =  SubNational3_gfli )
      print(summary(mod2_exOp_reg))

      if(OnlySigCoeff){
        coeffSig_reg <- summary(mod2_exOp_reg)$coeff[,4][summary(mod2_exOp_reg)$coeff[,4] <.1]
        Inters_reg <- names(summary(mod2_exOp_reg)$coeff[,4][summary(mod2_exOp_reg)$coeff[,4] <.1])
      }else{
        coeffSig_reg <- summary(mod2_exOp_reg)$coeff[,4]
        Inters_reg <- names(summary(mod2_exOp_reg)$coeff[,4])
      }
     
      results_reg <- SubNat_VCD2 %>% filter(gfli_basket %in% na.omit(unique(fbsTree[,c("gfli_basket"),with=F]))[gfliB])
      
      results_reg$intercept <-as.numeric(coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in% "(Intercept)"])
      results_reg$countydummy =0
      results_reg$regiondummy =0
      results_reg$cropdummy  =0
      results_reg$timetrend  =0
      results_reg$LocFactor =0

     
      ctrys  <-  grep("est_region",names(coeffSig_reg), value=TRUE)[grep("est_region",names(coeffSig_reg), value=TRUE) %in% grep(":",names(coeffSig_reg), value=TRUE)]
      ctrys_num <- gsub("geographicaream49","",grep("geographicaream49", unlist(strsplit(grep(":",names(coeffSig_reg), value=TRUE), ":")), value=TRUE))
      regs  <- grep("est_region",names(coeffSig_reg), value=TRUE)[! grep("est_region",names(coeffSig_reg), value=TRUE) %in% grep(":",names(coeffSig_reg), value=TRUE)]
      reg_num <-   gsub("est_region","", regs)
      loc <- grep("fsc_location1",names(coeffSig_reg), value=TRUE)
      loc_num <-   gsub("fsc_location1","",loc )
      for(ind1 in 1:length(unique(regs))){
        results_reg[est_region == reg_num[ind1],
                    regiondummy := as.numeric(coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in%  regs[ind1]]),]

      }
      for(ind1 in 1:length(unique(ctrys))){
        results_reg[est_region == ctrys_num[ind1],
                    countydummy := as.numeric(coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in%  ctrys[ind1]]),]
        
      }
      for(ind2 in 1:length(unique(grep("measureditemcpc",names(coeffSig_reg), value=TRUE)))){
        results_reg[measureditemcpc == gsub("measureditemcpc","", grep("measureditemcpc",names(coeffSig_reg), value=TRUE))[ind2],
                    cropdummy := as.numeric(coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in% grep("measureditemcpc",names(coeffSig_reg), value=TRUE)[ind2]]),]
      }
      if(length(grep("timepointyears",names(coeffSig_reg), value=TRUE)) >0){
        results_reg[, timetrend :=  mapply(`*`,coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in% "timepointyears"], results_reg[ ,"timepointyears",with=F])]

      }
      for(ind3 in 1:length(unique(grep("fsc_location1",names(coeffSig_reg), value=TRUE)))){
        results_reg[locations == gsub("fsc_location1","", grep("fsc_location1",names(coeffSig_reg), value=TRUE))[ind3],
                    LocFactor := as.numeric(coefficients(mod2_exOp_reg)[names(coefficients(mod2_exOp_reg)) %in% grep("fsc_location1",names(coeffSig_reg), value=TRUE)[ind3]]),]
      }
      
      
      results_reg[,VC_ModelEst := regiondummy+countydummy+cropdummy+timetrend +LocFactor +intercept,]
      results_reg[VC_ModelEst <0,"VC_ModelEst"] <- 0
      tt <- results_reg %>%
        group_by(est_region,locations) %>%
        summarise(mean = max(VC_ModelEst), n = n())
      print(tt)
     
      gfli_results <- rbind(results_reg)
      gfli_results$timepointyears <- as.numeric(gfli_results$timepointyears)

     
      gfli_results$locations <- as.character(gfli_results$locations)
      gfli_results <- gfli_results[, c( "geographicaream49", "measureditemcpc",  "timepointyears" , "locations","VC_ModelEst"), with=F ]
      gfli_results[VC_ModelEst>mean(SubNational3_gfli $loss_per_clean, na.rm = T) + 3*sd( SubNational3_gfli $loss_per_clean, na.rm = T),VC_ModelEst := mean(SubNational3_gfli $loss_per_clean, na.rm = T) + 3*sd(SubNational3_gfli$loss_per_clean, na.rm = T)] 
      

      SubNat_VCD <- merge(SubNat_VCD, gfli_results, by =c( "geographicaream49", "measureditemcpc","timepointyears", "locations"), all.x =T)
    
      SubNat_VCD[VC_ModelEst>0,value:= VC_ModelEst]
      SubNat_VCD[,VC_ModelEst := NULL]
      
      SubNat_VCD[value>0,] 
      
    }
    names( SubNat_VCD)[ names( SubNat_VCD) =="locations"] = "fsc_location1" 
    names( SubNat_VCD)[ names( SubNat_VCD) =="value"] = "loss_per_clean" 
    SubNat_VCD <- SubNat_VCD[measureditemcpc != "",]
    SubNat_VCD <- SubNat_VCD[geographicaream49 != "",]
    SubNat_VCD <- SubNat_VCD[fsc_location1 != "",]
    if(savesws){
      SubNat_VCD2 <- SubNat_VCD
      setnames(SubNat_VCD2, old= c("geographicaream49", "measureditemcpc", "timepointyears", "fsc_location1", "loss_per_clean"),
               new= c("geographicaream49", "measureditemcpc", "timepointyears", "fsc_location", "value")
                 )
       SubNat_VCD2$timepointyears <- as.integer(SubNat_VCD2$timepointyears)
       SubNat_VCD2 <- SubNat_VCD2[value>0,]
       ## Delete
       table = "sn_vc_est"
       changeset <- Changeset(table)
       newdat <- ReadDatatable(table, readOnly = FALSE)
       AddDeletions(changeset, newdat)
       Finalise(changeset)
       ## Add
       AddInsertions(changeset, SubNat_VCD2 )
       Finalise(changeset)
    }
    
  }else{SubNat_VCD =0}  
  #######################################
  
  for(i in unique(RawData$geographicaream49)){
    # this first level selects the data for a specific country
    
    data2 <- RawData %>% filter(geographicaream49 == i)
    if(modelEst){
    SubNat_VCD_m2 <- SubNat_VCD  %>% filter(geographicaream49 == i)
    }
    for( ii in unique(data2$measureditemcpc)){
      # this first level selects the data for a specific crop
      data3 <- data2 %>% filter(measureditemcpc == ii)
      if(modelEst){
        SubNat_VCD_m3 <-SubNat_VCD_m2   %>% filter(measureditemcpc == ii)
      }
      
      for(iii in unique(data3$timepointyears)){
        # this first level selects the data for a specific Year
        data4 <- data3 %>% filter(timepointyears == iii)
        if(modelEst){
         SubNat_VCD_m4 <-SubNat_VCD_m3   %>% filter(timepointyears == iii)
        }
        data4 <- data4[!(is.na(data4$loss_per_clean)),] 
        if(nrow(data4)==0){next}
        if(modelEst){
          data4 <- rbind( data4,SubNat_VCD_m4,fill =T)
        }
        if(dim(data4)[1]> 1){
          if(opt == "aveatFSP"){
            # Averages at each location in the FSC, excluding the whole chain and the SWS 
            for(iiii in 1:length(locations)){
              TransitionMatrix[iiii] = sum(data4$loss_per_clean*(data4$fsc_location1 == colnames(TransitionMatrix)[iiii]))/(sum(data4$fsc_location1 == colnames(TransitionMatrix)[iiii]))
              TransitionMatrix[is.nan(TransitionMatrix)] = 0
            }
            
            ref <- 1000 # reference amount
            ref0 <- ref 
            amt <- 0 
            WS <- unlist(TransitionMatrix[,c("wholesupplychain")]) # Whole supply chain
            SWS <-unlist(TransitionMatrix[,c("sws_total")])  # Estimate from the SWS data 
            FSC <- TransitionMatrix[,1:(length(TransitionMatrix)-2)][TransitionMatrix[,1:(length(TransitionMatrix)-2)] > 0] #aggregates of the food supply chain
            for(ni in 1:length(FSC)){
              # Uses the referent quantity to aggregate losses alog the supply chain
              ref <- ref- ref*(FSC[ni])
              amt <- ref*(FSC[ni]) + amt
            }
            # Takes the average for the compounded losses over the FSC and averages with the Whole supply chain and the SWS
            est = c(amt/ref0, WS, SWS)
            lossPer = mean(est[!est==0])

            FullSeta[1, geographicaream49 := unique(data4$geographicaream49),]
            FullSeta[1, timepointyears := unique(data4$timepointyears),]
            FullSeta[1, measureditemcpc := unique(data4$measureditemcpc),]
            FullSeta[1, isocode := na.omit(unique(data4$isocode)),]
            FullSeta[1, country := na.omit(unique(data4$country)),]
            FullSeta[1, 'loss_per_clean':=lossPer]
            FullSeta[1, 'fsc_location'] <- "Calc"
          }} else{
            # If there is only one estimate for the country/commodity/year the estimate just gets added to the dataset   
            FullSeta[1, geographicaream49 := unique(data4$geographicaream49),]
            FullSeta[1, timepointyears := unique(data4$timepointyears),]
            FullSeta[1, measureditemcpc := unique(data4$measureditemcpc),]
            FullSeta[1, isocode := unique(data4$isocode),]
            FullSeta[1, country := unique(data4$country),]
            FullSeta[1, loss_per_clean:=unique(data4$loss_per_clean),]
            FullSeta[1, fsc_location := unique(data4$fsc_location),]

          }
        
        #  if(opt == "model"){
        #Model here
        #  }
        count = count +1
        FullSet <- rbind(FullSet,FullSeta)
      }
      }}
  FullSet <- FullSet %>% filter(loss_per_clean != 0)
  FullSet <- FullSet %>% filter(country!='antarctica')
  FullSet$measureditemcpc <- addHeadingsCPC(FullSet$measureditemcpc) 
  names(FullSet) <- tolower(names(FullSet))
  

  return(FullSet)
}  
