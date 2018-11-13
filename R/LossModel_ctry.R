#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' @export LossModel_ctry

LossModel_ctry <- function(Data,timeSeriesDataToBeImputed,ctry_modelvar,HierarchicalCluster,keys_lower){
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
  # ctry_modelvar: is if a specific country is to be modeled
  #	HierarchicalCluster: is for the group/cluster ("foodgroupname" was the best preformer)
  Impute <- 'ctry' # FALSE
  
  CB <- function(dataIn){
    r = exp(dataIn)/(1+exp(dataIn)) 
    return(r)}
  
  x_impute <- function(x, fun) {
    x[is.na(x)] <- fun(x, na.rm = TRUE)
    return(x)
  } 
  
  modelversion <- packageVersion("faoswsLoss")
  
  minobs <- 4
  minctry <- 2
  NumImportVarUse <- 8
  names(Data) <- tolower(names(Data))
  
  datasetN <- names(timeSeriesDataToBeImputed)
  
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
  Data <- Data %>% filter (loss_per_clean > 0.01)
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
  
 
  fbsTree$GFLI_Basket <- 'NA'
  fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals',]
  fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
  fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
  fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
  fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
  fbsTree[foodgroupname %in% c(2943, 2946,2945,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",
  fbsTree[foodgroupname %in% c(2949), GFLI_Basket :='Eggs',] 
  #fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Fish',] #Fish needs to be included after it has losses in the SWS
  fbsTree[GFLI_Basket == "NA", 'GFLI_Basket'] <- NA
  
  
  ## extracts number of countries to model.

  if(any(ctry_modelvar %in% Data$geographicaream49)){
    Data <-  Data %>% filter(geographicaream49 %in% ctry_modelvar)
   
  }else{
    Data <- Data
  }
  
  print(sort(na.omit(unique(Data$geographicaream49))))
  ##### Part 2A - trend countries with data #####
  # Section will estimate first countries with official data by country and commodity group 
  start <- Sys.time()
  for (vii in 1:length(na.omit(unique(Data$geographicaream49)))){
    for (vi in 1:length(na.omit(unique(fbsTree$GFLI_Basket)))){
      
      
      print(unique(Data$geographicaream49)[vii])
      print(paste("percent complete: ",vii/ length(na.omit(unique(Data$geographicaream49)))))
      print(vi)
      # for each subgroup in the cluster the model is created - with new varaibles selected
      name = unique(fbsTree[GFLI_Basket ==na.omit(unique(fbsTree$GFLI_Basket))[vi],foodgroupname])
      print(name)
    
      # filters out the official data and prepares to estimate by country and commodity
      data_byctry <- Data %>% filter(foodgroupname %in% name &
                                    geographicaream49 %in% na.omit(unique(Data$geographicaream49))[vii] & 
                                    (fsc_location %in% c("sws_total","SWS")))
      
      data_byctry <- unique(data_byctry)
      
      if(dim(data_byctry)[1]< 3){
        print("not enough observations")
        next
      }
      for(viii in unique(data_byctry$measureditemcpc)){
        print(viii)
        if((dim(data_byctry[measureditemcpc == viii,"loss_per_clean",with=F])[1]>1) &
            (var(data_byctry[measureditemcpc == viii,"loss_per_clean",with=F]) == 0)){
          # for countries that use carry-over percentages 
          
          carryover <- mean(data_byctry[measureditemcpc == viii ,loss_per_clean], na.rm=TRUE)
          TS2 <-  timeSeriesDataToBeImputed %>% filter( geographicaream49 %in% na.omit(unique(Data$geographicaream49))[vii]& 
                                                          measureditemcpc %in% viii)
          
          TS2[value_measuredelement_5016==0,loss_per_clean := carryover]
          nameadd <- paste(names(TS2)[!names(TS2) %in% keys_lower],'a',sep="")
          names(TS2)[!names(TS2) %in% keys_lower] <- paste(names(TS2)[!names(TS2) %in% keys_lower],'a',sep="")
          
          timeSeriesDataToBeImputed <-  merge(timeSeriesDataToBeImputed, TS2, by=keys_lower, all.x= TRUE)
          
          timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,flagcombination:= flagcombinationa,]
          timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,loss_per_clean:= loss_per_cleana,]
          timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagobservationstatus := 'I',] 
          timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagmethod:= 'i',]
          timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagcombination := paste(flagobservationstatus,flagmethod, sep=";"),]
          timeSeriesDataToBeImputed[,(nameadd):= NULL,]
          
          timeSeriesDataToBeImputed[flagcombination == "I;i" & is.na(protected),"protected"] <-TRUE
          
          print(dim(timeSeriesDataToBeImputed))
          print('for this commodity a carryover was applied')
          data_byctry <- data_byctry %>% filter(!measureditemcpc ==viii )
        }else{
        next}
        
      }
      if(dim(data_byctry)[1] == 0){
        next
      }
      
      CPCs <- unique(data_byctry$measureditemcpc)
      
      nums1 <- sapply(data_byctry, is.numeric)
      dropCV <- list()
      stop = length(colnames(data_byctry))
      ii = 1
      while(ii){
        nam = colnames(data_byctry)[ii]
        if(is.na(nam)){break}
        if(sapply(data_byctry[,nam,with=F],class)== "numeric"){ 
          corrV  <- cor(data_byctry[,nam,with=F],data_byctry[,colnames(data_byctry) %in% names(nums1[nums1==T]) ,with=F],use="pairwise.complete.obs")
          corrV2 <- colnames(corrV)[corrV >.85 | is.na(corrV) ]
          corrV2  <- corrV2[!corrV2 %in% c(keys_lower,nam)]
          dropCV <- c(dropCV,na.omit(corrV2))
          if(length(unique(na.omit(corrV2))) >0){
            data_byctry[,c(na.omit(unique(corrV2))):= NULL]
            nums1 <- sapply(data_byctry, is.numeric)
          }}
        ii =ii +1
        
      }
      
     
      nums1[tolower(keys_lower)] <- TRUE
      nums1[names(nums1) == "sdg_regions"]<- TRUE
      explanatory <- names(nums1)[nums1 == TRUE]
      data_byctry <-  data_byctry[ , explanatory ,with=FALSE]
      
      data_byctry[,losstransf := log(loss_per_clean/(1-loss_per_clean))]
      datamod_ctry <- data_byctry[,!names(data_byctry) %in% unique(c("loss_per_clean")),with=F]
      
      print("Break 1")
      
      ###### Variable Selection ####
      ## CLuster wide Variable selection ##
      fit2 <- rpart(losstransf ~ ., data =  data_byctry[,!names(data_byctry) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
      ImportVar2 <- names(fit2$variable.importance)[1:NumImportVarUse]
      
      UseVari2 <- unique(na.omit(c(keys_lower,depVar,ImportVar2)))
      datamod_ctry <- datamod_ctry[,UseVari2,with=F]
      
      Predvar2 <- unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",UseVari2 ,"protected")))
      DataPred <-  timeSeriesDataToBeImputed %>% filter(measureditemcpc %in% CPCs &
                                         geographicaream49 %in% na.omit(unique(Data$geographicaream49))[vii] &
                                          is.na(protected))
      
      print(dim(DataPred))
      if(dim(DataPred)[1] < 1){
        next
      }
      
      DataPred <- VariablesAdd1(DataPred,keys_lower,Predvar2,Impute,name)
      print(dim(DataPred))
      
      Predvar2 <- Predvar2[!Predvar2 %in% c("losstransf", "protected")]
      Predvar2 %in% names(DataPred)
      Predvar2 %in% names(datamod_ctry)
      
      # drops variables that dont have any data in the predictive set
      drop_a <- names(datamod_ctry)[!names(datamod_ctry) %in% c(names(DataPred),"losstransf")]
      if(length(drop_a)>0){
        Predvar2 <- Predvar2[!Predvar2 %in% drop_a]
        datamod_ctry[,c(drop_a) := NULL] 
        ImportVar2 <- na.omit(ImportVar2[!ImportVar2 %in% drop_a])
        UseVari2 <- UseVari2[!UseVari2 %in% drop_a]
      }
    
      DataPred <- DataPred[, Predvar2, with=F] 
      names(DataPred) <- tolower(names(DataPred))
      names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 
      
      datapred <- DataPred
      datamod_ctry$measureditemcpc<- as.factor(datamod_ctry$measureditemcpc)
      
      print("Break 2- dPred")
      print(dim(datapred))
      
      # To impute data for the predictive set for missing observations in the explanatory data
      drop4 <- ""
      if(length(ImportVar2)>0){
        for(ir in 1:length(na.omit(ImportVar2))){
          for( j in unique(datapred$geographicaream49)){
            drop4 <- c()
            if(is.na(sum(datapred[geographicaream49 == j,ImportVar2[ir],with=F] ))){
              drop4 <- ImportVar2[ir]
            }else{
          
              datapred[geographicaream49 == j,ImportVar2[ir]] <- with(datapred[geographicaream49 ==j,], x_impute(datapred[[ImportVar2[ir]]], mean))
              datapred[,ImportVar2[ir]] <- na.approx(datapred[,ImportVar2[ir],with=F], na.rm = T)
            }
            datamod_ctry[geographicaream49 == j,ImportVar2[ir]] <- with(datamod_ctry[geographicaream49 ==j,], x_impute(datamod_ctry[[ImportVar2[ir]]], mean))
            datamod_ctry[,ImportVar2[ir]] <- na.approx(datamod_ctry[,ImportVar2[ir],with=F], na.rm = T)
            
            ## Drops the variables that are na
            r <- var(datamod_ctry)
            
            for( n in 4:length(names(datamod_ctry))){
              drop3 <- names(datamod_ctry)[!names(datamod_ctry) %in% names(na.omit(r[,n]))]
              drop3 <- drop3[drop3 %in% names(datamod_ctry)[n:length(names(datamod_ctry))]]
              drop4 <- c(drop4,drop3)
            }
            drop4 <-unique(drop4)
          
            
          } 
          
          # if(is.integer(datapred[[ImportVar[ir]]])){
          #   datapred[is.na(datapred[[ImportVar[ir]]]), ImportVar[ir]] <- as.integer(sum(datapred[[ImportVar[ir]]], na.rm=TRUE)/dim(datapred[is.na(datapred[[ImportVar[ir]]]), ImportVar[ir],with=F])[1])
          #  }else{
          #   datapred[is.na(datapred[[ImportVar[ir]]]), ImportVar[ir]] <-
          #        (sum(datapred[[ImportVar[ir]]], na.rm=TRUE)/dim(datapred[is.na(datapred[[ImportVar[ir]]]), ImportVar[ir],with=F])[1])}
        }
      }
      ##### PART 3 - Full Specified Heirarchical model ####
      # The choice of model is based on the assumption that countries are inherehently different in their loss structure.
      # Given that the panel data is unbalanced, the model has been specified and tested  for different specifications
      # With the challenge that losses may have a linear trend, but be increasing (decreasing) at decreasing rates
     
      UseVari2 <- UseVari2[!UseVari2 %in% drop4]
      
      keys_lower_WOC <-keys_lower[!keys_lower %in% c("geographicaream49")]
      if(length(unique(na.omit(datamod_ctry$measureditemcpc)))<= 1){
        keys_lower_WOC <-  keys_lower_WOC[! keys_lower_WOC %in% c("measureditemcpc")]
      }
      
      ## Model 
      if(length(ImportVar2)>0){
        formula_ctry <- paste(paste(depVar," ~",sep=""),paste(keys_lower_WOC,sep="+", collapse= " + "),'+',paste(unique(UseVari2[!UseVari2 %in% c(depVar,keys_lower)]), collapse= " + ")) #
      }else{
        formula_ctry <- paste(paste(depVar," ~",sep=""), paste(keys_lower_WOC,sep="+", collapse= " + "), collapse= " + ")
      }
      #paste("factor(", keys_lower[3], ")",sep="", collapse= " + "),'+',
      mod2_rlm <- lm(as.formula(formula_ctry), data = datamod_ctry)
      
      mod2_rand <- plm(as.formula(formula_ctry), data = datamod_ctry , index=c("measureditemcpc"), model ="pooling")
      
      print("Break 3")
      
      mod2 <- mod2_rand
      summary(mod2)
      mod2res = resid(mod2)
      mod2res <-  as.data.table(mod2res)
      coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
      modrun =1
      
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
      }
      if(modelstr == "random" |modelstr == "pooling"){  
        # COmbines the coefficients to create an estimate for every column in the group
        coeffN <- c(UseVari2[!UseVari2 %in% c(index1,DV,depVar)])
        coeffN <- na.omit(coeffN) 
        
        if(OnlySigCoeff){
          coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
          coeffSig <- names(coeffSig)[names(coeffSig) %in%  UseVari2]
          Inters <- names(summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1])
        }else{
          coeffSig <- coeffN
          Inters <- names(coefficients(mod2))
        }
        
        coeffindex <-  grep(index1,Inters, perl=TRUE, value=TRUE)
        coeffDV <-     grep(DV,Inters, perl=TRUE, value=TRUE)
        
        datapred$countydummy =0
        datapred$cropdummy  =0
        datapred$intercept  =0
        
        print("Break 4")
        
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
                    if(dim(datapred)[1]>2){ 
                     rowSums(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffSig],datapred[ ,coeffSig,with=F]), na.rm=TRUE)
                      }else{
                        mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffSig],datapred[ ,coeffSig,with=F])
                      }+
                     countydummy+cropdummy+intercept,] 
        }else{ datapred[,losstransf := countydummy+cropdummy+intercept,]}
      }
      CB(datapred$losstransf)
      if(modrun ==1){
        #Transform the losses back to % and not logged numbers 
        
        names(datapred) <- tolower(names(datapred))
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
        medianLossRaw <- median(unlist(Data[measureditemcpc %in% CPCs,"loss_per_clean", with=F]))
        print(paste('average loss:',medianLoss*100, "%"))
        
        print(paste('average loss Raw:',medianLossRaw*100, "%"))
        print(paste('Number of countries:',length(unique(datamod_ctry$geographicaream49))))
        print(paste('Number of comodities:',length(unique(datamod_ctry$measureditemcpc))))
      }
      print("Break 5")
      # In the cases where the model over estimates the loss to unrealistic numbers then the dataset reverts to the mean of the data available
      datapred[loss_per_clean > mean(data_byctry$loss_per_clean, na.rm = T) + 3*sd(data_byctry$loss_per_clean, na.rm = T),"loss_per_clean"] <- mean(data_byctry$loss_per_clean, na.rm = T) 
      datapred[loss_per_clean <.01,"loss_per_clean"] <- mean(data_byctry$loss_per_clean, na.rm = T) 
      
      print(paste('max loss:',max(datapred$loss_per_clean, na.rm=TRUE)*100, "%"))
      timeSeriesDataToBeImputed$geographicaream49 <- as.character(timeSeriesDataToBeImputed$geographicaream49)
      
      int1 <-datapred[,tolower(datasetN), with=F]
      nameadd <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
      names(int1)[!names(int1) %in% keys_lower] <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
      #int1 <- int1[!duplicated(int1),]
      nameadd2 <- c(keys_lower,"value_measuredelement_5016","value_measuredelement_5126", "flagcombination","flagobservationstatus","flagmethod","loss_per_clean","protected")
      
      timeSeriesDataToBeImputed <-  merge(timeSeriesDataToBeImputed, int1, by=keys_lower, all.x= TRUE)
      print(dim(int1))
      timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,]
                                
      timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,flagcombination:= flagcombinationa,]
      timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,loss_per_clean:= loss_per_cleana,]
      timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagobservationstatus := 'I',] 
      timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagmethod:= 'e',]
      timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagcombination := paste(flagobservationstatus,flagmethod, sep=";"),]
      dropsend <- names(timeSeriesDataToBeImputed) [!names(timeSeriesDataToBeImputed) %in% nameadd2]
      timeSeriesDataToBeImputed[,c(dropsend):=NULL,]
      print(dim(timeSeriesDataToBeImputed))
      #tt <- timeSeriesDataToBeImputed %>% filter(geographicaream49 %in% data_byctry$geographicaream49 & measureditemcpc %in%CPCs)
      #write.table(tt , "C:/Users/Englisha.FAODOMAIN/Desktop/tt.csv", sep=",")
      ######################<><><><><><><><><>
      print("Break 6") 
      
      # ##### Save model parameters
      # 
      SavResult <- list(
        cluster=name,
        formula=formula_ctry,
        coeffnames = paste(unlist(names(coefficients(mod2))),collapse = "##"),
        mean_intercept= mod2_rand$coefficients[1],
        coeff =paste(unlist(coefficients(mod2)), collapse = "##"),
        coeffsig=paste(unlist(coeffSig), collapse = "##"), 
        coeffindex=paste(unlist(coeffindex), collapse = "##"),
        coeffdv= paste(unlist(coeffDV), collapse = "##") 
        )
      
      
      lossmodelruns = as.data.table(SavResult)
      

      lossmodelruns[,daterun := date() ]
      lossmodelruns[,modelversion := modelversion ]
      setcolorder(lossmodelruns, c("daterun","modelversion",names(SavResult)))
      names(lossmodelruns) <- tolower(names(lossmodelruns) )

      table = "lossmodelruns_ctry"
      changeset <- Changeset(table)
      newdat <- ReadDatatable(table, readOnly = FALSE)
      newdat2 <- newdat[0,]
      Finalise(changeset)
      AddInsertions(changeset,  lossmodelruns)
      Finalise(changeset)
      
  
    }
    }
  end <- Sys.time()
  print(end - start)
  print(paste("Number of estimated points: ",dim(timeSeriesDataToBeImputed[loss_per_clean>0 & is.na(protected),])[1],sep=""))
  print(paste("Percent of total: ",dim(timeSeriesDataToBeImputed[ !is.na(flagobservationstatus) & is.na(protected),])[1]/
                                    dim(timeSeriesDataToBeImputed[is.na(flagobservationstatus) & is.na(protected),])[1],sep=""))
  
  timeSeriesDataToBeImputed[!is.na(flagobservationstatus) & is.na(protected),"protected"] <-TRUE
  
  #write_json(list(DataIN = Data), paste(dirmain,'\\ModelResults\\',HierarchicalCluster,'_ModelResults.json',sep="")) 
  return(timeSeriesDataToBeImputed)
  
}
