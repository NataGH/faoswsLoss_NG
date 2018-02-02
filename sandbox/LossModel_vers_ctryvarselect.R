#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 

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
  
  minobs <- 4
  minctry <- 2
  names(Data) <- tolower(names(Data))
  modelversion <- "0.1.0"
 
  

  datasetN <- names(timeSeriesDataToBeImputed)
  #### Protected Data ###
  flagValidTableLoss <- as.data.table(flagValidTable)
  protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
    .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
  timeSeriesDataToBeImputed[,flagcombination :=  paste(flagobservationstatus, flagmethod, sep = ";")] 
  
  timeSeriesDataToBeImputed[flagcombination %in% protectedFlag$flagCombination,Protected := TRUE,]
  timeSeriesDataToBeImputed[Protected == TRUE,loss_per_clean:= loss_per_clean/100]
  
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
  Data[,loss_per_clean := loss_per_clean/100 ]
  Data[loss_per_clean == 0,loss_per_clean := 0.0001,]
  Data[loss_per_clean == 1,loss_per_clean :=  0.9999,] 
  Data[,losstransf :=  log(loss_per_clean/(1-loss_per_clean))] #
  names(Data) <- gsub("[[:punct:]]","_",names(Data)) 
 
  Data$geographicaream49 <- as.character(Data$geographicaream49)
  
  IdentVar <- c(keys_lower,"isocode","country","sdg_region")
  newcol  <- names(sapply(Data, is.numeric))[!(names(sapply(Data, is.numeric)) %in% IdentVar)]

  drops2 <- c("lag1yr","lag2yr", "lag3yr", "month","month_x","month_y","harvesting_month_onset")
  drops1  <- c("loss_per_clean", IdentVar)
  Data <- Data[,names(Data)[!names(Data) %in% drops2],with=FALSE]

  fbsTree <- ReadDatatable("fbs_tree")
  names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
  names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
  
  timeSeriesDataToBeImputedGroups <- join(timeSeriesDataToBeImputed, fbsTree, by = c("measureditemcpc"),type= 'left', match='all')
  ##### PART 2 - Random Forest ####
  for (vi in 1:length(unique(timeSeriesDataToBeImputedGroups $foodgroupname)[!is.na(unique(timeSeriesDataToBeImputedGroups $foodgroupname))])){
    print(vi)
    
    # for each subgroup in the cluster the model is created - with new varaibles selected
    name = unique(timeSeriesDataToBeImputedGroups$foodgroupname)[!is.na(unique(timeSeriesDataToBeImputedGroups$foodgroupname))][vi]
    print(name)
    modrun <- 0
    # filter the dataset to the
    data1 <- Data %>% filter(Data[,HierarchicalCluster,with=FALSE]== name)
    
    idx_country <- unique(as.character(data1$geographicaream49))[(table(as.character(data1$geographicaream49))>=2) == T]
    #data1 <- data1[,names(data1) %in%  c(newcol,index1, DV) ,with=FALSE] #'Year'
    data1 <- data1 %>% filter(!(is.na(geographicaream49) | (geographicaream49== "")))
    nums1 <- sapply(data1, is.numeric)
    nums1[names(nums1) == tolower(keys_lower)] <- TRUE
    #nums1[names(nums1) == "sdg_regions"]<- TRUE
    explanatory <- names(nums1)[nums1 == TRUE]
    data1 <-  data1[ , explanatory ,with=FALSE]
    ###
    
    # Section determines which variables are the most useful by country
    Use_varibles <- list() 
    AllVar <- list() 
    Use_varibles <- colnames(data1)
    Use_varibles <- Use_varibles[!(Use_varibles %in% drops2)] 
    AllVar <- AllVar[!(AllVar %in% drops2)] 
    
    NumImportVarUse = 8
    ## For countries that havRe less than the min observations to estimate the equation, they get grouped by region
    data_mix <- data1[1,] 
    data_mix[1, geographicaream49:= "999"]
    check <- 0
    
    for(ctry in unique(data1$geographicaream49)){
      data_section <- data1 %>% filter(geographicaream49 == ctry)
      if(nrow(data_section) <minobs){
        data_mix <- rbind(data_mix,data_section)
        next
      }
      check <- nrow(data_section)
      #print(nrow(data_section))
      nums2 <- sapply(data_section , is.numeric)
      explanatory2 <- names(nums2)[nums2 == TRUE]
      nonZero <- names(na.omit(colSums(data_section[,explanatory2,with=FALSE]))>0)
      data_section <- data_section[,nonZero,with=FALSE]
      V <- sort(table(unlist(AllVar)),decreasing = TRUE)
      v2 <-names(V[0:(NumImportVarUse -length(Use_varibles))])
      
      #print(ctry)
      if(nrow(data_section)>= minobs){
        # Runs the Random Forest Algorithm to find the most important variables in the dataset
        #Deletes columns that might have nulls in the column
        idxcol <- apply(apply(data_section,2,is.na),2,sum) == dim(data_section)[1]
        cols <- c(colnames(data_section)[colnames(data_section) %in% newcol[!newcol  %in% idxcol]])
        fit <- rpart(losstransf ~ ., data = data_section[, unique(cols), with=FALSE],control=rpart.control(minsplit=30, cp=0.1))
        ImportVar <- names(fit$variable.importance)
        check <- check +nrow(data_section)
        if(!is.null(ImportVar)){
          # gets rid of variables that are highly correlated in the important variables 
          Data[,sapply(Data, is.numeric) ==TRUE, with=FALSE]
          dropd <- unique(ImportVar[rowSums(((cor(data_section[, colnames(data_section) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs")>.85) & (cor(data_section[, colnames(data_section) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs") < 1))*1) >0])
          dropd <- dropd[2:length(dropd)]
          ImportVar  <- ImportVar[!(ImportVar) %in% dropd]
          Use_varibles2 <- c(Use_varibles, v2[!v2 %in% dropd & !v2 %in% Use_varibles])
        }
        if(is.null(ImportVar)){
          ImportVar  <- index1
          Use_varibles2 <- c(Use_varibles)
        }
        Use_varibles <- intersect(ImportVar,Use_varibles) 
        AllVar <- c(AllVar,ImportVar) 
      }
      V <- sort(table(unlist(AllVar)),decreasing = TRUE)
      V <-V[!names(V) %in% drops1]
      v2 <-names(V[0:(NumImportVarUse -length(Use_varibles))])
      Use_varibles2 <- c(Use_varibles2[!is.na(Use_varibles2)],v2 )
    }
    if(length(na.omit(Use_varibles2)) < NumImportVarUse){ 
      data_section <- data1 
      if(nrow(data_section) <minobs){
        data_mix <- rbind(data_mix,data_section)
        next
      }
      nums2 <- sapply(data_section , is.numeric)
      explanatory2 <- names(nums2)[nums2 == TRUE]
      nonZero <- names(na.omit(colSums(data_section[,explanatory2,with=FALSE]))>0)
      data_section <- data_section[,nonZero,with=FALSE]
      V <- sort(table(unlist(AllVar)),decreasing = TRUE)
      v2 <-names(V[0:(NumImportVarUse -length(Use_varibles))])
      
      #print(ctry)
      if(nrow(data_section)>= minobs){
        # Runs the Random Forest Algorithm to find the most important variables in the dataset
        #Deletes columns that might have nulls in the column
        idxcol <- apply(apply(data_section,2,is.na),2,sum) == dim(data_section)[1]
        cols <- c(colnames(data_section)[colnames(data_section) %in% newcol[!newcol  %in% idxcol]])
        fit <- rpart(losstransf ~ ., data = data_section[, unique(cols), with=FALSE],control=rpart.control(minsplit=30, cp=0.1))
        ImportVar <- names(fit$variable.importance)
        check <- check +nrow(data_section)
        if(!is.null(ImportVar)){
          # gets rid of variables that are highly correlated in the important variables 
          Data[,sapply(Data, is.numeric) ==TRUE, with=FALSE]
          dropd <- unique(ImportVar[rowSums(((cor(data_section[, colnames(data_section) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs")>.85) & (cor(data_section[, colnames(data_section) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs") < 1))*1) >0])
          dropd <- dropd[2:length(dropd)]
          ImportVar  <- ImportVar[!(ImportVar) %in% dropd]
          Use_varibles2 <- c(Use_varibles, v2[!v2 %in% dropd & !v2 %in% Use_varibles])
        }
        if(is.null(ImportVar)){
          ImportVar  <- index1
          Use_varibles2 <- c(Use_varibles)
        }
        Use_varibles <- intersect(ImportVar,Use_varibles) 
        AllVar <- c(AllVar,ImportVar) 
      }
      V <- sort(table(unlist(AllVar)),decreasing = TRUE)
      V <-V[!names(V) %in% drops1]
      v2 <-names(V[0:(NumImportVarUse -length(Use_varibles))])
      Use_varibles2 <- c(Use_varibles2[!is.na(Use_varibles2)],v2 )
    }
    
    Use_varibles2 <- unique(na.omit(Use_varibles2[!(Use_varibles2 %in% drops2)]))
    Use_varibles2 <- unique(na.omit(Use_varibles2[!(Use_varibles2 %in% drops1)]))
    if(length(Use_varibles2) == 0){
      print("Not enough correlation with the explanatory variables")
      next
    }

    ###################
    Predvar<- unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",Use_varibles2[1:NumImportVarUse],"Protected")))
    DataPred <- VariablesAdd1(timeSeriesDataToBeImputed,keys_lower,Predvar)
    names(DataPred) <- tolower(names(DataPred))
    names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 
    #DataPred$geographicaream49 <- as.character(DataPred$geographicaream49)
  
    ################################  
    #Trims the variables to the the ones going to be used in the model  - included from the random forests
    datapred <- DataPred %>% filter(foodgroupname == name)
    datapred <- datapred[, unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",Use_varibles2[1:NumImportVarUse]))),with=FALSE] #, "Year"
    datapred <- datapred[,colSums(is.na(datapred))<nrow(datapred),with=FALSE]

    datamod <- data1[,  names(data1)[names(data1) %in% unique(na.omit(c(index1,"losstransf",Use_varibles2[1:NumImportVarUse], DV,"timepointyears")))], with=FALSE] 
   

    if(HierarchicalCluster != "geographicaream49"){datamod <- datamod %>% filter("geographicaream49" !="")}
    datamod <-  na.omit(datamod)
    #datapred <-  na.omit(datapred)<>
    UseV <- unique(Use_varibles2[1:NumImportVarUse])
    if(dim(datamod)[1] < minobs | length(unique(datamod$geographicaream49)) < minctry){
      sprintf('Not enough Observations to model %s',name)
      next
    }
    #Check for variation in columns
    usevartest <- unique(c(UseV[!is.na(UseV)],keys_lower))
    usevartest <- usevartest[!colSums(var(datamod[,usevartest ,with=F])) ==0]
    if(length(usevartest) <length(unique(c(UseV[!is.na(UseV)],keys_lower)))){
      UseVari <- usevartest
      datamod<- datamod[,c(UseVari,"losstransf"),with=FALSE]
    }else{UseVari <-unique(c(UseV[!is.na(UseV)],keys_lower)) }
    
    ## Constraining the data to after 1990 - as there is a structural shift in the SWS data
    datamod <- datamod %>% filter(timepointyears >1990)

    ##### PART 3 - Full Specified Heirarchical model ####
    # The choice of model is based on the assumption that countries are inherehently different in their loss structure.
    # Given that the panel data is unbalanced, the model has been specified and tested  for different specifications
    # With the challenge that losses may have a linear trend, but be increasing (decreasing) at decreasing rates

    if((length(index1)!=0) & (length(unique(c(UseV[!is.na(UseV)],index1,DV))) > 2)){
      ## Model 
      formula <- paste("losstransf ~ ", paste(unique(UseVari), collapse= "+")) #
      mod2_rand <- plm(as.formula(formula), data = datamod, index= c(index1), model ="random")
      mod2 <- mod2_rand
      coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
      mod2res = resid(mod2)
      mod2res <-  as.data.frame(cbind(datamod[,keys_lower,with=F], mod2res))
      
      # # test for heterskedastic err
      r <- bptest(as.formula(formula), data = datamod, studentize=F)
      if(r$p.value < 0.05){
        #time mean average
        tma2 <- datamod
        tma2 <- tma2[,geographicaream49 :=NULL ]
        tma2 <- tma2[, lapply(.SD, mean), by = c("timepointyears", "measureditemcpc")]
        
        formulatma <- paste("losstransf ~ ", paste(unique(UseVari)[!unique(UseVari) %in% 'geographicaream49'], collapse= "+"))
        mod2_randtma <- plm(as.formula(formulatma), data =  tma2, index= c("measureditemcpc"), model ="random")
        mod2_fixedtma <- plm(as.formula(formulatma), data = tma2, index=  c("measureditemcpc"), model ="within")
        mod2_poolingtma <- plm(as.formula(formulatma), data = tma2, index=  c("measureditemcpc"), model ="pooling")
        phtest(mod2_fixedtma, mod2_randtma)
        r <- bptest(as.formula(formulatma), data = tma2, studentize=F)
        mod2 <- mod2_randtma
       
        
        }
      modrun =1  
     }
  
    
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
    # ##### PART 3 - Demeaned Specified Heirarchical model ####
    # MeanDev_apply <- function(x) {apply(x, 2, function(y) y - mean(y))}
    # meanctr <- as.data.frame(MeanDev_apply(datamod[,!names(datamod) %in% keys_lower,with=F]))
    # meanctr <- cbind(meanctr, datamod[,keys_lower,with=F])
    # 
    # formula_mc  <- paste("losstransf ~ ", paste(unique(UseVari), collapse= "+")) #
    # mod2_mc <- plm(as.formula(formula_mc), data = meanctr, index= c(index1), model =modelstr)
    # 
    # ## Testing the impact of the groupings
    # n <- dim(summary(mod2_rand)$coeff)[1]
    # K <- dim(summary(mod2_fixed)$coeff)[1] - n
    # N <- dim(datamod)[1]
    # F_Test <- ((summary(mod2_rand)$r.squared - summary(mod2_fixed)$r.squared)/(n-1))/((1-summary(mod2_rand)$r.squared)/(n*N-n-K))
    # print(pf(F_Test,(n-1),(n*N-n-K)))
    # 
    # #Hausman (if p is less than 0.05 use the fixed)
    # phtest(mod2_fixed, mod2_rand)# Tests for random or fixed
    # plmtest(mod2_pooling, type=c("bp")) # Tests for random or ols
    # 
    # pcdtest(mod2_fixed, test = c("lm")) # Cross sectional
    # pcdtest(mod2_fixed, test = c("cd")) # Cross sectional
    # 
    # library(tseries)
    # adf.test(datamod$losstransf, k=2)
    # ################################################################
    
    ####################### Residuals #########################################
    OnlySigCoeff =T

    #DV <- names(fixef(mod2))
    if(index1 == "measureditemcpc"){PD_V2 <- unique(unlist(c(datapred[,index1,with=FALSE])))} 
    if(index1 == "geographicaream49"){PD_V2 <- levels(unlist(c(datapred[,index1,with=FALSE])))}
    if(modelstr == "within"){  
    # For each of the items in the index
    coeffN <- unique(c(Use_varibles2))
    for(ind1 in 1:length(unique(DV))){
          datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]) ,]$losstransf =
            fixef(mod2)[ind1] +coefficients(mod2)[names(coefficients(mod2))[ind2]]+
            rowSums(data.frame(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],  datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]),coeffN,with=FALSE]))) 
        }
      }
    if(modelstr == "random" |modelstr == "pooling"){  
        # COmbines the coefficients to create an estimate for every column in the group
        coeffN <- c(Use_varibles2[!(Use_varibles2 %in% index1 | Use_varibles2 %in% DV | Use_varibles2 %in%  "loss_per_clean")])
        coeffN <- na.omit(coeffN) 
        
        if(OnlySigCoeff){
          coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
          coeffSig <- names(coeffSig)[names(coeffSig) %in% Use_varibles2]
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
        datapred[(cropdummy == 0) & (countydummy ==0),intercept:=coefficients(mod2)[1]]
          
        if(length(coeffSig) >0){
          # Applies the weights of the estimation across the entire cluster sets, using the demeaned coefficient as the intercept  (coefficients(mod2)[1]  
          datapred[,losstransf := 
                     rowSums(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffSig],datapred[ ,names(coefficients(mod2)[names(coefficients(mod2)) %in% coeffSig]), with=FALSE]), na.rm=TRUE)+
                     countydummy+cropdummy,] 
        }else{ datapred[,losstransf := coefficients(mod2)[1] + countydummy+cropdummy+intercept,]}
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
      print(paste('average loss:',medianLoss*100, "%"))
     
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
   
  
    
    if(graphLoss){
      GraphLosses(timeSeriesDataToBeImputed2,Data_Use_train,mod2res)
    }
    
    
    ##### Save model parameters

    SavResult <- list(cluster=name,formula=formula,coeffnames = paste(unlist(names(coefficients(mod2))), collapse = "##"),mean_intercept=mean(ercomp(mod2)$theta),coeff =paste(unlist(coefficients(mod2)), collapse = "##"),
                     coeffsig=paste(unlist(coeffSig), collapse = "##"), coeffindex=paste(unlist(coeffindex), collapse = "##"), coeffDV= paste(unlist(coeffDV), collapse = "##") )

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
  
  
  timeSeriesDataToBeImputed <- merge(timeSeriesDataToBeImputed,production, by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
  timeSeriesDataToBeImputed[,value_measuredelement_5126 := loss_per_clean,]
  timeSeriesDataToBeImputed[,value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510,]
  timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed %>% filter(!is.na(value_measuredelement_5016))
  datasetN[datasetN=="loss_per_clean"] <- "value_measuredelement_5126"
  

  ### For the SWS ####
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

  ## Save to the SWS
   # stats = SaveData(domain = "lossWaste",
   #               dataset = "loss",
   #               data = timeSeriesDataToBeImputed_5016
   #              )
   # 
   # stats = SaveData(domain = "lossWaste",
   #                     dataset = "loss",
   #                      data = timeSeriesDataToBeImputed_5126
   #     )

    
      
    
    
  #write_json(list(DataIN = Data), paste(dirmain,'\\ModelResults\\',HierarchicalCluster,'_ModelResults.json',sep="")) 
  return(list(timeSeriesDataToBeImputed_5016=timeSeriesDataToBeImputed_5016,timeSeriesDataToBeImputed_5126=timeSeriesDataToBeImputed_5126))
  
}


