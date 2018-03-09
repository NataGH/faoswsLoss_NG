#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' @export
#' 
impVar <- FALSE# "ctry" #"var" "RF"
  
LossModel <- function(Data,timeSeriesDataToBeImputed,production,HierarchicalCluster,keys_lower){
  # Description:
  # The model operates in 3 parts, 
  #  1) sets the clusters for estimating for countries without data. 
  #      And it log transforming the data and setting the bounds just above 0 and below 1. 
  #  2) Random Forest variable selection. Runs the model across countries within the cluster to find the top preforming variables 
  #  3) Heirarchical model. Models the loss percentages on the results of the random forest 
  #     variable selection and then applies them to the country/commodities/years needed    
  
  # inputs:
  #	Data: is the data used for training the indicator. This should be the final data set, 
  #     of the loss percentages by country, with data aggregated in the Markov model and with explanatory variables added. 
  #	DataPred: is the data that needs estimates predicted (finalPredictData)
  #	HierarchicalCluster: is for the group/cluster ("foodgroupname" was the best preformer)
  CB <- function(dataIn){
    r = exp(dataIn)/(1+exp(dataIn)) 
    return(r)}
  
  modelversion <- "0.1.0"
  
  minobs <- 4
  minctry <- 2
  NumImportVarUse <- 8
  names(Data) <- tolower(names(Data))

  datasetN <- names(timeSeriesDataToBeImputed)

  ######## Protected Data ########
  flagValidTableLoss <- as.data.table(flagValidTable)
  protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
    .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
  timeSeriesDataToBeImputed[,flagcombination :=  paste(flagobservationstatus, flagmethod, sep = ";")] 
  
  timeSeriesDataToBeImputed[flagcombination %in% protectedFlag$flagCombination,Protected := TRUE,]

  
  ##### PART 1 - Data trasnformations and Clusters ####
  # The HierarchicalClusters set up the clusters for the analysis - as several HierarchicalClusters were tested
  if(HierarchicalCluster == "foodgroupname" | HierarchicalCluster == "foodPerishableGroup"){
    index1 <- c("geographicaream49")
    DV <- c("measureditemcpc")
    modelstr <- 'random'
  }
  if(HierarchicalCluster == "SDG.Regions"){
    index1 <- c("measureditemcpc")
    DV <-  c("geographicaream49")
    modelstr <- 'random'
  }
  if(HierarchicalCluster == "ISOCode"){
    index1 <- c("measureditemcpc")
    DV <-  c("geographicaream49")
    modelstr <- 'within'
  }
  
  # Prepares the data for the analysis
  Data <- Data %>% filter(loss_per_clean >= 0.01)
  
  Data[loss_per_clean == 0,loss_per_clean := 0.0001,]
  Data[loss_per_clean == 1,loss_per_clean :=  0.9999,] 
  Data[,losstransf := log(loss_per_clean/(1-loss_per_clean))]
  names(Data) <- gsub("[[:punct:]]","_",names(Data)) 
  depVar <- "losstransf" 
 
  Data$geographicaream49 <- as.character(Data$geographicaream49)
  
  IdentVar <- c(keys_lower,"isocode","country","sdg_region")
  newcol  <- names(sapply(Data, is.numeric))[!(names(sapply(Data, is.numeric)) %in% IdentVar)]

  drops2 <- c("lag1yr","lag2yr", "lag3yr", "month","month_x","month_y","harvesting_month_onset","area_code_y")
  drops1  <- c("loss_per_clean", IdentVar)
  Data <- Data[,names(Data)[!names(Data) %in% drops2],with=FALSE]

  fbsTree <- ReadDatatable("fbs_tree")
  names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
  names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
  
  timeSeriesDataToBeImputedGroups <- join(timeSeriesDataToBeImputed, fbsTree, by = c("measureditemcpc"),type= 'left', match='all')
  
  fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals ',]
  fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
  fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
  fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
  fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
  fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",
  #unique(timeSeriesDataToBeImputedGroups $foodgroupname)[!is.na(unique(timeSeriesDataToBeImputedGroups $foodgroupname))])
  ##### PART 2 - Random Forest ####
  for (vi in 1:length(na.omit(unique(fbsTree$GFLI_Basket)))){
    #length(unique(timeSeriesDataToBeImputedGroups $foodgroupname)[!is.na(unique(timeSeriesDataToBeImputedGroups $foodgroupname))])){
    print(vi)
    
    # for each subgroup in the cluster the model is created - with new varaibles selected
    #name = unique(timeSeriesDataToBeImputedGroups$foodgroupname)[!is.na(unique(timeSeriesDataToBeImputedGroups$foodgroupname))][vi]
    name = unique(fbsTree[GFLI_Basket ==na.omit(unique(fbsTree$GFLI_Basket))[vi],foodgroupname])
    print(name)
    modrun <- 0
    # filter the dataset to the
    data1 <- Data %>% filter(foodgroupname %in% name)
    
    # Truncates the data between 2 standard deviations of the mean 
    data1 <- data1 %>% filter(loss_per_clean <= mean(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T))
    data1 <- data1 %>% filter(loss_per_clean >= mean(data1$loss_per_clean, na.rm = T) - 3*sd(data1$loss_per_clean, na.rm = T))
 
    CPCs <- unique(data1$measureditemcpc)
    
    
    
    ###### Variable Selection ####
    ## CLuster wide Variable selection ##
    fit <- rpart(losstransf ~ ., data =  data1[,!names(data1) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
    ImportVar <- names(fit$variable.importance)[1:NumImportVarUse]
    
    UseVari <- unique(c(keys_lower,depVar,ImportVar))
    datamod <-     data1 [,UseVari,with=F]
    
    # # Add a time lag
    # library(tseries)
    # lag = 3
    # s <- adf.test(datamod$losstransf, k=lag)
    # datamod <- dataLag(datamod,indexVar= keys_lower,var="losstransf",timeVar="timepointyears",lag,LType='fullset')

    ###################
    Predvar<- unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",UseVari ,"Protected")))
    DataPred <-  timeSeriesDataToBeImputed %>% filter(measureditemcpc %in% CPCs)
    DataPred <- VariablesAdd1(DataPred,keys_lower,Predvar,impVar)
    names(DataPred) <- tolower(names(DataPred))
    names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 

    datapred <- DataPred
    
    VarNames <- names(DataPred)
    VarNames <- VarNames[!VarNames %in% c("geographicaream49","timepointyears","measureditemcpc","country","crop","loss_per_clean","fsc_location" ,"id1" ,                 
                                          "id2","foodgroupname","id4","month_x", "temperature_c" , "month_y" ,  "rainfall_mm")]
    
    
    for(ir in 1:length(VarNames)){
        for( j in unique(  datapred$geographicaream49)){
          i = ii /(length(unique(  datapred$geographicaream49))*length(VarNames))
          setTxtProgressBar(pb, i)
          datapred[geographicaream49 == j,VarNames[ir]] <- with(  datapred[geographicaream49 ==j,], impute(  datapred[[VarNames[ir]]], mean))
          datapred[,VarNames[ir]] <- na.approx(datapred[,VarNames[ir],with=FALSE], na.rm = T)
          ii =ii+1
        }}
    
    
    ##### PART 3 - Full Specified Heirarchical model ####
    # The choice of model is based on the assumption that countries are inherehently different in their loss structure.
    # Given that the panel data is unbalanced, the model has been specified and tested  for different specifications
    # With the challenge that losses may have a linear trend, but be increasing (decreasing) at decreasing rates

    ## Model 
    formula <- paste(paste(depVar," ~",sep=""),paste(keys_lower,sep="+", collapse= " + "),'+',paste(unique(UseVari[!UseVari %in% c(depVar,keys_lower)]), collapse= " + ")) #
    formula4 <- paste(paste(depVar," ~",sep=""), paste(keys_lower,sep="+", collapse= " + "), collapse= " + ")
    
    #paste("factor(", keys_lower[3], ")",sep="", collapse= " + "),'+',
    mod2_rlm <- lm(as.formula(formula), data = datamod)
    mod2_rand <- plm(as.formula(formula), data = datamod , index=c("measureditemcpc"), model ="random")
    mod2_red  <- plm(as.formula(formula4), data = datamod , index=c("measureditemcpc"), model ="random")
    #mod2_beta <- betareg(as.formula(formula), data = datamod)
   
    CB(mod2_rlm$coefficients[1] + mod2_rlm$coefficients[names(mod2_rlm$coefficients)=="timepointyears"]*2008)
    CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008)
    CB(mod2_red$coefficients[1]+ mod2_red$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008)
    
    modelspec = 'random'
    # GIven the unbalanced aspects of the panels, for some cases it creates heteroskedastic errors which skew the data beyond the max/min of reasonable estimates
    # In these cases the data is averaged over cpc (which provided better explanatory power than over country) by year and then re-selected the variables and 
    # re-estimate the series
    coeffSig <- summary(mod2_rand)$coeff[,4][summary(mod2_rand)$coeff[,4] <.1]
    coeffSig <- names(coeffSig)[names(coeffSig) %in%  c("timepointyears")]
    ##any(CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008)>
    ##max(Data[measureditemcpc %in% CPCs, loss_per_clean]) | CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008)< .01 )
    
    
    if( CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008) > median(unlist(Data[measureditemcpc %in% CPCs,"loss_per_clean"])) ){
      tma2 <- data1[,!names(data1) %in% unique(c("loss_per_clean")),with=F]
      tma2 <- tma2[,!names(tma2) %in% c('geographicaream49',"m49CPC","foodgroupname" ),with=F]
      tma2 <- tma2[, lapply(.SD, mean), by = c("timepointyears", "measureditemcpc")]
      fitTMA <- rpart(losstransf ~ ., data = tma2[,!names(  tma2) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
      ImportVarTMA <- names(fitTMA$variable.importance)[1:NumImportVarUse]
      
      UseVariTMA <- na.omit(unique(c(keys_lower[2:3],depVar,ImportVarTMA)))
      
      #Predvar<- unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",UseVariTMA ,"Protected")))
      #DataPred <-  timeSeriesDataToBeImputed %>% filter(measureditemcpc %in% CPCs)
      #DataPred <- VariablesAdd1(DataPred,keys_lower,Predvar,impVar)
      #names(DataPred) <- tolower(names(DataPred))
      #names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 
    
      datamodTMA <- tma2[,UseVariTMA,with=F]
      #### Exclude NaN
      datamodTMA <-datamodTMA[complete.cases(datamodTMA), ]
      formulaTMA <- paste(paste(depVar," ~",sep=""), paste(keys_lower[2:3], collapse= " + "),'+',paste(unique(UseVariTMA[!UseVariTMA %in% c(depVar,keys_lower)]), collapse= " + ")) #
      mod2_rand <- plm(as.formula(formulaTMA), data = datamodTMA%>% filter(timepointyears>1990) , index=c("measureditemcpc"), model ="random")
      modelspec = 'randomAveraged'
      
      datapred <- DataPred
      UseVari <- UseVariTMA
    }
    
    
    mod2 <- mod2_rand
    summary(mod2)
    mod2res = resid(mod2)
    mod2res <-  as.data.table(mod2res)
    coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
    modrun =1
    
    # ############ Alternative models & tests ######################
    # #### Part 3 and 3/4 - Specification tests
    # ## Alternative models
    # 
    # formula <- paste("losstransf ~ ", paste(unique(UseVari), collapse= "+")) 
    # mod2_fixed <- plm(as.formula(formula), data = datamod, index=  c(index1), model ="within")
    # mod2_pooling <- plm(as.formula(formula), data = datamod, index=  c(index1), model ="pooling")
    # 
    # mod2_rand <- plm(as.formula(formula), data = datamod, index= c('measureditemcpc'), model ="random")
    # #mod2_lin = lm(as.formula(formula), data = datamod)
    # ## Model
    # formula_res  <- paste("losstransf ~ ", paste(unique(keys_lower), collapse= "+")) #
    # mod2_resR <- plm(as.formula(formula_res), data = datamod, index= c(index1), model ="random")
    # mod2_resF <- plm(as.formula(formula_res), data = datamod, index= c(index1), model ="pooling")
    # mod2_lnres  = lm(as.formula(formula_res), data = datamod)
    # 
    # ## Testing the impact of the groupings
    # n <- dim(summary(mod2_rand)$coeff)[1]
    # K <- dim(summary(mod2_fixed)$coeff)[1] - n
    # N <- dim(datamod)[1]
    # F_Test <- ((summary(mod2_rand)$r.squared - summary(mod2_fixed)$r.squared)/(n-1))/((1-summary(mod2_rand)$r.squared)/(n*N-n-K))
    # print(pf(F_Test,(n-1),(n*N-n-K)))
    # 
    # #Hausman (if p is less than 0.05 use the fixed)
    # phtest(mod2_fixed,mod2_rand)
    # Tests for random or fixed
    # plmtest(mod2_pooling, type=c("bp")) # Tests for random or ols
    # 
    # pcdtest(mod2_fixed, test = c("lm")) # Cross sectional
    # pcdtest(mod2_fixed, test = c("cd")) # Cross sectional
    # 

    # ################################################################
    
    ####################### Results #########################################
    OnlySigCoeff =T

    
    #DV <- names(fixef(mod2))
    if(index1 == "measureditemcpc"){PD_V2 <- unique(unlist(c(datapred[,index1,with=FALSE])))} 
    if(index1 == "geographicaream49"){PD_V2 <- levels(unlist(c(datapred[,index1,with=FALSE])))}
    if(modelstr == "within"){  
      # For each of the items in the index
      coeffN <- unique(c( UseVari[!UseVari %in% c(keys_lower,depVar)]))
      for(ind1 in 1:length(unique(DV))){
            datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]) ,]$losstransf =
              fixef(mod2)[ind1] +coefficients(mod2)[names(coefficients(mod2))[ind2]]+
              rowSums(data.frame(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],  datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]),coeffN,with=FALSE]))) 
          }
      }else{
   
        # COmbines the coefficients to create an estimate for every column in the group
        coeffN <- c(UseVari[!UseVari %in% c(index1,DV,depVar)])
        coeffN <- na.omit(coeffN) 
        
        if(OnlySigCoeff){
          coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
          coeffSig <- names(coeffSig)[names(coeffSig) %in%  UseVari]
          Inters <- names(summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1])
        }else{
          coeffSig <- coeffN
          Inters <- names(coefficients(mod2))
        }
        
        coeffindex <-  grep(index1,Inters, perl=TRUE, value=TRUE)
        coeffDV <-     grep(DV,Inters, perl=TRUE, value=TRUE)
        
        datapred[,countydummy :=0]
        datapred[,cropdummy :=0]
        datapred[,intercept :=0]
        
        for(ind1 in 1:length(unique(gsub(index1,"", coeffindex)))){
          datapred[geographicaream49 == gsub(index1,"", coeffindex)[ind1],countydummy := as.numeric(coefficients(mod2)[coeffindex[ind1]]),]
        }
        for(ind2 in 1:length(unique(gsub(DV,"", coeffDV)))){ 
          datapred[measureditemcpc == gsub(DV,"", coeffDV)[ind2],cropdummy:= as.numeric(coefficients(mod2)[coeffDV[ind2]]),]
          
        }
        datapred[,intercept:=coefficients(mod2)[1]]
        #(cropdummy == 0) & (countydummy ==0)
        if(length(coeffSig) >0){
          # Applies the weights of the estimation across the entire cluster sets, using the demeaned coefficient as the intercept  (coefficients(mod2)[1]  
          datapred[,losstransf := 
                     rowSums(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffSig],datapred[ ,coeffSig,with=F]), na.rm=TRUE)+
                     countydummy+cropdummy+intercept,] 
        }else{ datapred[,losstransf := countydummy+cropdummy+intercept,]}
      }
 
    if(modrun ==1){
      #Transform the losses back to % and not logged numbers 

      names(datapred) <- tolower(names(datapred))
      names(production) <- tolower(names(production))
      production$geographicaream49 <-as.character(production$geographicaream49)
      datapred$geographicaream49 <-as.character(datapred$geographicaream49)
      
      datapred[datapred$losstransf !=0, loss_per_clean := exp(datapred$losstransf)/(1+exp(datapred$losstransf)),]
      #datapred[datapred$losstransf !=0, loss_per_clean := datapred$losstransf[datapred$losstransf !=0]]
      datapred[,value_measuredelement_5126 := loss_per_clean,]
      datapred[,value_measuredelement_5016 := 0,]
      datapred[,flagobservationstatus := 'I',] 
      datapred[,flagmethod:= 'e',]
      datapred[,flagcombination := 'I;e',]
      datapred[,protected := FALSE,]
      medianLoss <- median(datapred$value_measuredelement_5126, na.rm=TRUE)
      medianLossRaw <- median(unlist(Data[measureditemcpc %in% CPCs,"loss_per_clean"]))
      print(paste('average loss:',medianLoss*100, "%"))
      print(paste('average loss Raw:',medianLossRaw*100, "%"))
      print(paste('Number of countries:',length(unique(datamod$geographicaream49))))
      print(paste('Number of comodities:',length(unique(datamod$measureditemcpc))))
    }
    
    timeSeriesDataToBeImputed$geographicaream49 <- as.character(timeSeriesDataToBeImputed$geographicaream49)
    
    int1 <-datapred[,tolower(datasetN), with=F]
    nameadd <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
    names(int1)[!names(int1) %in% keys_lower] <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
    
    timeSeriesDataToBeImputed <-  merge(timeSeriesDataToBeImputed, int1, by=keys_lower, all.x= TRUE)
    timeSeriesDataToBeImputed %>% filter(is.na(Protected))
    
    timeSeriesDataToBeImputed[is.na(Protected) & value_measuredelement_5016a>=0,flagcombination:= flagcombinationa,]
    timeSeriesDataToBeImputed[is.na(Protected) & value_measuredelement_5016a>=0,loss_per_clean:= loss_per_cleana,]
    timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagobservationstatus := 'I',] 
    timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagmethod:= 'e',]
    timeSeriesDataToBeImputed[is.na(Protected) & loss_per_cleana >0,flagcombination := paste(flagobservationstatus,flagmethod, sep=";"),]
    timeSeriesDataToBeImputed[,(nameadd):= NULL,]
   
    ##### Save model parameters

    SavResult <- list(cluster=name,formula=formula,coeffnames = paste(unlist(names(coefficients(mod2))), collapse = "##"),mean_intercept=mean(ercomp(mod2)$theta),coeff =paste(unlist(coefficients(mod2)), collapse = "##"),
                     coeffsig=paste(unlist(coeffSig), collapse = "##"), coeffindex=paste(unlist(coeffindex), collapse = "##"), coeffdv= paste(unlist(coeffDV), collapse = "##") )

    lossmodelruns = as.data.table(SavResult)

    lossmodelruns[,daterun := date() ]
    lossmodelruns[,modelversion := modelversion ]
    setcolorder(lossmodelruns, c("daterun","modelversion",names(SavResult)))
    names(lossmodelruns) <- tolower(names(lossmodelruns) )
    
    table = "lossmodelruns"
    changeset <- Changeset(table)
    newdat <- ReadDatatable(table, readOnly = FALSE)
    newdat2 <- newdat[0,]
    Finalise(changeset)
    AddInsertions(changeset,  lossmodelruns)
    Finalise(changeset)
  }
  
  # Multiplies loss percentages by production
  timeSeriesDataToBeImputed <- merge(timeSeriesDataToBeImputed,production, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
  timeSeriesDataToBeImputed[,value_measuredelement_5126 := loss_per_clean,]
  timeSeriesDataToBeImputed[,value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510,]
  timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed %>% filter(!is.na(value_measuredelement_5016))
  datasetN[datasetN=="loss_per_clean"] <- "value_measuredelement_5126"
  

  ### Splits the data tables for the SWS ####
  timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5016","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5016[, measuredElement := "5016"]
  setnames(timeSeriesDataToBeImputed_5016, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"  ,"Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5016, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed_5016 %>% filter(!is.na(flagMethod))
  
  ##---------------------
  timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5126","flagobservationstatus", "flagmethod") ,with=F] 
  
  timeSeriesDataToBeImputed_5126[, measuredElement := "5126"]
  setnames(timeSeriesDataToBeImputed_5126, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ),
           new =  c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  
  
  setcolorder(timeSeriesDataToBeImputed_5126, 
              c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  
  timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed_5126 %>% filter(!is.na(flagMethod))

  # Save to the SWS
  stats = SaveData(domain = "lossWaste",
                dataset="loss",
                data = timeSeriesDataToBeImputed_5016
               )

  stats = SaveData(domain = "lossWaste",
                      dataset="loss",
                       data=timeSeriesDataToBeImputed_5126
      )

    
      
    
    
  #write_json(list(DataIN = Data), paste(dirmain,'\\ModelResults\\',HierarchicalCluster,'_ModelResults.json',sep="")) 
  return(list(timeSeriesDataToBeImputed_5016=timeSeriesDataToBeImputed_5016,timeSeriesDataToBeImputed_5126=timeSeriesDataToBeImputed_5126))
  
}


