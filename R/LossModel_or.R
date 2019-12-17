#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' @export LossModel

LossModel <- function(Data,timeSeriesDataToBeImputed,production,HierarchicalCluster,keys_lower,CountryGroup,fbsTree,Temperature,Precipitation,CropCalendar,LossTables_Yr,LossTables_ctryYr){
  # Description:
  # The model operates in 3 parts, 
  #  1) sets the clusters for estimating for countries without data. 
  #      And it log transforming the data and setting the bounds just above 0 and below 1. 
  #  2) Random Forest variable selection. Runs the model across countries within the cluster to find the top preforming variables 
  #  3) Heirarchical model. Models the loss percentages on the results of the random forest 
  #     variable selection and then applies them to the country/commodities/years needed    timeSeriesDataToBeImputed_ctry2
  
  # inputs:
  #	Data: is the data used for training the indicator. This should be the final data set, 
  #     of the loss percentages by country, with data aggregated in the Markov model and with explanatory variables added. 
  #	DataPred: is the data that needs estimates predicted (finalPredictData)
  #	HierarchicalCluster: is for the group/cluster ("foodgroupname" was the best preformer)
  Impute <- FALSE
  
  CB <- function(dataIn){
    r = exp(dataIn)/(1+exp(dataIn)) 
    return(r)}
  
  x_impute <- function(x, fun) {
    x[is.na(x)] <- fun(x, na.rm = TRUE)
    return(x)
  } 
  
  modelversion <- "0.1.12"
  
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
  fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",
  fbsTree[GFLI_Basket == "NA", 'GFLI_Basket'] <- NA
  model_mem <-0
  model_restricted <- 0
  model_mean <- 0
  
  ##### PART 2 - Random Forest ####
  for (vi in 1:length(na.omit(unique(fbsTree$GFLI_Basket)))){
    print(vi)
    
    # for each subgroup in the cluster the model is created - with new varaibles selected
    name = unique(fbsTree[GFLI_Basket %in%na.omit(unique(fbsTree$GFLI_Basket))[vi],foodgroupname])
    print(name)
    modrun <- 0
    # filter the dataset to the
    data1 <- Data %>% filter(foodgroupname %in% name)
    
    # Truncates the data between 2 standard deviations of the mean 
    data1 <- data1 %>% filter(loss_per_clean <= median(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T))
    data1 <- data1 %>% filter(loss_per_clean >= median(data1$loss_per_clean, na.rm = T) - 3*sd(data1$loss_per_clean, na.rm = T))
 
    
    CPCs <- unique(data1$measureditemcpc)
    #Makes the columns numeric and looks at correlated variables
    nums1 <- sapply(data1, is.numeric)
    dropCV <- list()
    stop = length(colnames(data1))
    ii = 1
    while(ii){
      nam = colnames(data1)[ii]
      if(is.na(nam)){break}
      if(sapply(data1[,nam,with=F],class)== "numeric"){ 
        corrV  <- cor(data1[,nam,with=F],data1[,colnames(data1) %in% names(nums1[nums1==T]) ,with=F],use="pairwise.complete.obs")
        corrV2 <- colnames(corrV)[corrV >.85 | is.na(corrV) ]
        corrV2  <- corrV2[!corrV2 %in% c(keys_lower,nam)]
        dropCV <- c(dropCV,na.omit(corrV2))
        if(length(unique(na.omit(corrV2))) >0){
          data1[,c(na.omit(unique(corrV2))):= NULL]
          nums1 <- sapply(data1, is.numeric)
        }}
      ii =ii +1
      
    }
    
    
    nums1[tolower(keys_lower)] <- TRUE
    nums1[names(nums1) == "sdg_regions"]<- TRUE
    explanatory <- names(nums1)[nums1 == TRUE]
    data1 <-  data1[ , explanatory ,with=FALSE]
    
    data1[,losstransf := log(loss_per_clean/(1-loss_per_clean))]
    
    #data1 <- data1 %>% filter(timepointyears >1990) # there is a data incongruity with the SWS at yr 1988/89
    datamod <- data1[,!names(data1) %in% unique(c("loss_per_clean")),with=F]
    #datamodEXP <-MultiExp(datamod,2,"losstransf")
    print("Break 1")
    
    ###### Variable Selection ####
    ## CLuster wide Variable selection ##
    fit <- rpart(losstransf ~ ., data =  data1[,!names(data1) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
    ImportVar <- names(fit$variable.importance)[1:NumImportVarUse]
    
    UseVari <- na.omit(unique(c(keys_lower,depVar,ImportVar)))
    datamod <- datamod[,UseVari,with=F]
    
    # # Add a time lag
    # library(tseries)
    # lag = 3
    # s <- adf.test(datamod$losstransf, k=lag)
    # datamod <- dataLag(datamod,indexVar= keys_lower,var="losstransf",timeVar="timepointyears",lag,LType='fullset')
    
    ###################
    Predvar2<- unique(na.omit(c(HierarchicalCluster, keys_lower,"loss_per_clean",UseVari ,"protected")))
    DataPred <-  timeSeriesDataToBeImputed %>% filter(measureditemcpc %in% CPCs &
                                                        is.na(protected))
    
    r <- NULL
    while(is.null(r)){ 
      r <- tryCatch(VariablesAdd1(DataPred,keys_lower,Predvar2,Impute,name,CountryGroup,fbsTree,Temperature,Precipitation,CropCalendar,LossTables_Yr,LossTables_ctryYr),
                    error = function(error_condition) {
                      return(NULL)
                    }
                    
      )
    }
    DataPred <- r
    names(DataPred) <- tolower(names(DataPred))
    names(DataPred) <- gsub("[[:punct:]]","_",names(DataPred)) 
    
    datapred <- DataPred
    datamod$measureditemcpc<- as.factor(datamod$measureditemcpc)
    
    
    print("Break 2")
    
    # To impute data for the predictive set for missing observations in the explanatory data
    datamod_x <- datamod #<- datamod_x 
    datapred_x <- datapred #<- datapred_x 
    names(datamod_x) %in% names(datapred_x)

    for(ir in 1:length(ImportVar)){
      for( j in unique(datapred$geographicaream49)){
        if(!((ImportVar[ir] %in% names(datapred)) & (ImportVar[ir] %in% names(datamod))) ){
          ImportVar <- ImportVar[ImportVar != ImportVar[ir]]
          next
        }
        if(dim(na.omit(datapred[geographicaream49 %in% j,ImportVar[ir],with=F]))[1]== 0){
          ImportVar <- ImportVar[ImportVar != ImportVar[ir]]
          next
        }
        if( (ImportVar[ir] %in% names(datapred)) & (ImportVar[ir] %in% names(datamod))   ){
          datapred[geographicaream49 %in% j,ImportVar[ir]] <- with(datapred[geographicaream49 %in% j,], x_impute(na.omit(unlist(datapred[geographicaream49 %in% j,ImportVar[ir],with=F])), mean))
          datapred[,ImportVar[ir]] <- na.approx(datapred[,ImportVar[ir],with=F], na.rm = T)
        
          datamod[geographicaream49 %in% j,ImportVar[ir]] <- with(datamod[geographicaream49 %in%j,],  x_impute(na.omit(unlist(datapred[geographicaream49 %in% j,ImportVar[ir],with=F])), mean))
          datamod[,ImportVar[ir]] <- na.approx(datamod[,ImportVar[ir],with=F], na.rm = T)
        }
        var(datamod)
        }
        # if(is.integer(datapred[[ImportVar[ir]]])){
        #   datapred[geographicaream49 %in% j & is.na(datapred[[ImportVar[ir]]]), ImportVar[ir]] <- as.integer(sum(datapred[geographicaream49 %in% j,ImportVar[ir],with=F], na.rm=TRUE)/dim(na.omit(datapred[geographicaream49 %in% j,ImportVar[ir],with=F]))[1])
        # }
        # else{datapred[geographicaream49 %in% j & is.na(datapred[[ImportVar[ir]]]), ImportVar[ir]] <- sum(datapred[geographicaream49 %in% j,ImportVar[ir],with=F], na.rm=TRUE)/dim(na.omit(datapred[geographicaream49 %in% j,ImportVar[ir],with=F]))[1]}
      
        } 
      
    keep1 <-  na.omit(c(keys_lower, "value_measuredelement_5016", "value_measuredelement_5126", "loss_per_clean",ImportVar) )
    keep2 <- na.omit(c(keys_lower, "losstransf",ImportVar) )
    datapred <- datapred[, keep1, with=F]
    datamod <- datamod[, keep2, with=F]
    ##### PART 3 - Full Specified Heirarchical model ####
    # The choice of model is based on the assumption that countries are inherehently different in their loss structure.
    # Given that the panel data is unbalanced, the model has been specified and tested  for different specifications
    # With the challenge that losses may have a linear trend, but be increasing (decreasing) at decreasing rates
    print("Break 3")
    ## Model 
    ## Model 
    if(length(na.omit(ImportVar))>0){
      formula <- paste(paste(depVar," ~",sep=""),paste(keys_lower,sep="+", collapse= " + "),'+',paste(unique(keep2[!keep2 %in% c(depVar,keys_lower)]), collapse= " + ")) #
      model_mem <- model_mem +1
    }else{
      formula <- paste(paste(depVar," ~",sep=""), paste(keys_lower,sep="+", collapse= " + "), collapse= " + ")
      model_restricted <- model_restricted +1
    }
    
    #paste("factor(", keys_lower[3], ")",sep="", collapse= " + "),'+',
    mod2_rlm <- lm(as.formula(formula), data = datamod)

    mod2_rand <- plm(as.formula(formula), data = datamod , index=c("measureditemcpc"), model ="pooling")
    #mod2_red  <- plm(as.formula(formula4), data = datamod , index=c("geographicaream49"), model ="random")
    
    
    CB(mod2_rlm$coefficients[1] + mod2_rlm$coefficients[names(mod2_rlm$coefficients) == "timepointyears"]*2008)
    CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients) == "timepointyears"]*2008)
    #CB(mod2_red$coefficients[1]+ mod2_red$coefficients[names(mod2_rand$coefficients) == "timepointyears"]*2008)
    
    modelspec = 'random'
    # Given the unbalanced aspects of the panels, for some cases it creates heteroskedastic errors which skew the data beyond the max/min of reasonable estimates
    # In these cases the data is averaged over cpc (which provided better explanatory power than over country) by year and then re-selected the variables and 
    # re-estimate the series
    coeffSig <- summary(mod2_rand)$coeff[,4][summary(mod2_rand)$coeff[,4] <.1]
    coeffSig <- names(coeffSig)[names(coeffSig) %in%  c("timepointyears")]
    
    # if(any(CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008) >
    #     max(Data[measureditemcpc %in% CPCs, loss_per_clean]) | CB(mod2_rand$coefficients[1]+ mod2_rand$coefficients[names(mod2_rand$coefficients)=="timepointyears"]*2008)< .01 )){
    #   tma2 <- datamod
    #   tma2 <- tma2[,!names(tma2) %in% c('geographicaream49'),with=F]
    #   tma2 <- tma2[, lapply(.SD, mean), by = c("timepointyears", "measureditemcpc")]
    #   fitTMA <- rpart(losstransf ~ ., data = tma2[,!names(  tma2) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
    #   ImportVarTMA <- names(fitTMA$variable.importance)[1:NumImportVarUse]
    #   
    #   UseVariTMA <- na.omit(unique(c(keys_lower[2:3],depVar,ImportVarTMA)))
    #   datamodTMA <- tma2[,UseVariTMA,with=F]
    #   #### Exclude NaN
    #   datamodTMA <-datamodTMA[complete.cases(datamodTMA), ]
    #   formulaTMA <- paste(paste(depVar," ~",sep=""), paste(keys_lower[2:3], collapse= " + "),'+',paste(unique(UseVariTMA[!UseVariTMA %in% c(depVar,keys_lower)]), collapse= " + ")) #
    #   mod2_rand <- plm::plm(as.formula(formulaTMA), data = datamodTMA , index=c("measureditemcpc"), model ="random")
    #   modelspec = 'randomAveraged'
    # }
    
    
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
    
    print("Break 5")
    #DV <- names(fixef(mod2))
    if(index1 == "measureditemcpc"){PD_V2 <- unique(unlist(c(datapred[,index1,with=FALSE])))} 
    if(index1 == "geographicaream49"){PD_V2 <- levels(unlist(c(datapred[,index1,with=FALSE])))}
    if(modelstr == "within"){  
      # For each of the items in the index
      coeffN <- unique(c( UseVari[!UseVari %in% c(keys_lower,depVar)]))
      for(ind1 in 1:length(unique(DV))){
        datapred[ which(datapred[,index1,with=FALSE] %in%  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] %in%  gsub(DV,"", names(coefficients(mod2)))[ind2]) ,]$losstransf =
          fixef(mod2)[ind1] +coefficients(mod2)[names(coefficients(mod2))[ind2]]+
          rowSums(data.frame(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],  datapred[ which(datapred[,index1,with=FALSE] %in%  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] %in%  gsub(DV,"", names(coefficients(mod2)))[ind2]),coeffN,with=FALSE]))) 
      }
    }
    if(modelstr == "random" |modelstr == "pooling"){  
      # COmbines the coefficients to create an estimate for every column in the group
      coeffN <- c(UseVari[!UseVari %in% c(index1,DV,depVar)])
      coeffN <- na.omit(coeffN) 
      
      if(OnlySigCoeff){
        coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
        coeffSig <- names(coeffSig)[names(coeffSig) %in%  UseVari]
        Inters <- names(summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1])
      }else{
        coeffSig <- summary(mod2)$coeff[,4]
        coeffSig <- names(coeffSig)[names(coeffSig) %in%  UseVari]
        Inters <- names(summary(mod2)$coeff[,4])
      }
      
      coeffindex <-  grep(index1,Inters, perl=TRUE, value=TRUE)
      coeffDV <-     grep(DV,Inters, perl=TRUE, value=TRUE)
      
      datapred$countydummy =0
      datapred$cropdummy =0
      datapred$intercept =0
      
      for(ind1 in 1:length(unique(gsub(index1,"", coeffindex)))){
        datapred[geographicaream49 %in% gsub(index1,"", coeffindex)[ind1],countydummy := as.numeric(coefficients(mod2)[coeffindex[ind1]]),]
      }
      for(ind2 in 1:length(unique(gsub(DV,"", coeffDV)))){ 
        datapred[measureditemcpc %in% gsub(DV,"", coeffDV)[ind2],cropdummy:= as.numeric(coefficients(mod2)[coeffDV[ind2]]),]
        
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
      datapred[,flagcombination := 'I;ec',]
      datapred[,protected := FALSE,]
      medianLoss <- median(datapred$value_measuredelement_5126, na.rm=TRUE)
      medianLossRaw <- median(unlist(Data[measureditemcpc %in% CPCs,"loss_per_clean", with=F]))
      print(paste('average loss:',medianLoss*100, "%"))
      
      print(paste('average loss Raw:',medianLossRaw*100, "%"))
      print(paste('Number of countries:',length(unique(datamod$geographicaream49))))
      print(paste('Number of comodities:',length(unique(datamod$measureditemcpc))))
    }
    
    # In the cases where the model over estimates the loss to unrealistic numbers then the dataset reverts to the mean of the data available
    modelag <- unique(unlist(datapred[(value_measuredelement_5126< LB) & (value_measuredelement_5126< 3*sd(data1$loss_per_clean, na.rm = T)),"measureditemcpc",with=F]))
    datapred[measureditemcpc %in% modelag,protected := FALSE]
    if(length(modelag)>0){
      formula <- paste(paste(depVar," ~",sep=""), paste(keys_lower,sep="+", collapse= " + "), collapse= " + ")
      model_restricted <- model_restricted +1
      mod2_rand <- plm(as.formula(formula), data = datamod , index=c("measureditemcpc"), model ="pooling")
      mod2 <- mod2_rand
      summary(mod2)
      modelCI2 <- tidy(mod2,conf.int = TRUE)
      
      #DV <- names(fixef(mod2))
      if(index1 == "measureditemcpc"){PD_V2 <- unique(unlist(c(datapred[,index1,with=FALSE])))} 
      if(index1 == "geographicaream49"){PD_V2 <- levels(unlist(c(datapred[,index1,with=FALSE])))}
      if(modelstr == "within"){  
        # For each of the items in the index
        coeffN <- unique(c( UseVari[!UseVari %in% c(keys_lower,depVar)]))
        for(ind1 in 1:length(unique(DV))){
          datapred[ which(datapred[,index1,with=FALSE] %in%  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] %in%  gsub(DV,"", names(coefficients(mod2)))[ind2]) ,]$losstransf =
            fixef(mod2)[ind1] +coefficients(mod2)[names(coefficients(mod2))[ind2]]+
            rowSums(data.frame(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],  datapred[ which(datapred[,index1,with=FALSE] %in%  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] %in%  gsub(DV,"", names(coefficients(mod2)))[ind2]),coeffN,with=FALSE]))) 
        }
      }
      if(modelstr == "random" |modelstr == "pooling"){  
        # COmbines the coefficients to create an estimate for every column in the group
        coeffN <- c(keep2[!keep2 %in% c(index1,DV,depVar)])
        coeffN <- na.omit(coeffN) 
        
        if(OnlySigCoeff){
          coeffSig <- summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1]
          coeffSig <- names(coeffSig)[names(coeffSig) %in%  keep2]
          Inters <- names(summary(mod2)$coeff[,4][summary(mod2)$coeff[,4] <.1])
        }else{
          coeffSig <- summary(mod2)$coeff[,4]
          coeffSig <- names(coeffSig)[names(coeffSig) %in%  keep2]
          Inters <- names(summary(mod2)$coeff[,4])
        }
        
        coeffindex <-  grep(index1,Inters, perl=TRUE, value=TRUE)
        coeffDV <-     grep(DV,Inters, perl=TRUE, value=TRUE)
        
        datapred$countydummy =0
        datapred$cropdummy  =0
        datapred$intercept  =0
        
        print("Break 4X2")
        
        for(ind1 in 1:length(unique(gsub(index1,"", coeffindex)))){
          datapred[geographicaream49 %in% gsub(index1,"", coeffindex)[ind1],countydummy := as.numeric(coefficients(mod2)[coeffindex[ind1]]),]
        }
        for(ind2 in 1:length(unique(gsub(DV,"", coeffDV)))){ 
          datapred[measureditemcpc %in% gsub(DV,"", coeffDV)[ind2],cropdummy:= as.numeric(coefficients(mod2)[coeffDV[ind2]]),]
          
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
        datapred[,flagcombination := 'I;es',]
        datapred[,protected := TRUE,]
        medianLoss <- median(datapred$value_measuredelement_5126, na.rm=TRUE)
        medianLossRaw <- median(unlist(Data[measureditemcpc %in% CPCs,"loss_per_clean", with=F]))
        print(paste('average loss:',medianLoss*100, "%"))
        
        print(paste('average loss Raw:',medianLossRaw*100, "%"))
        print(paste('Number of countries:',length(unique(datamod$geographicaream49))))
        print(paste('Number of comodities:',length(unique(datamod$measureditemcpc))))
      }
    }
    modelag2 <- unique(unlist(datapred[(value_measuredelement_5126< 0) & (value_measuredelement_5126< 3*sd(data1$loss_per_clean, na.rm = T)),"measureditemcpc",with=F]))
    datapred[measureditemcpc %in% modelag2,protected := FALSE]
    
    if(length(modelag)>0){
      datapred[loss_per_clean > median(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T),flagcombination := 'I;m',]
      datapred[loss_per_clean > median(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T),"loss_per_clean" := median(data1$loss_per_clean, na.rm = T),]
      datapred[loss_per_clean <.01 ,flagcombination := 'I;m',]
      datapred[loss_per_clean <.01,"loss_per_clean" :=  median(data1$loss_per_clean, na.rm = T),]
      datapred[is.na(loss_per_clean ),flagcombination := 'I;m',]
      datapred[is.na(loss_per_clean ),"loss_per_clean":= median(data1$loss_per_clean, na.rm = T),] 
      
      
      
      model_mean <- model_mean+ nrow(datapred[flagcombination == 'I;m',])
      
    }
    
    datapred[loss_per_clean >median(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T),"loss_per_clean"] <- median(data1$loss_per_clean, na.rm = T) 
    datapred[loss_per_clean <.01,"loss_per_clean"] <- median(data1$loss_per_clean, na.rm = T) 
    
    datapred[loss_per_clean > median(data1$loss_per_clean, na.rm = T) + 3*sd(data1$loss_per_clean, na.rm = T),"flagmethod"] <- "m"
    datapred[loss_per_clean <.01,"flagmethod"] <- "m"
    model_mean <- model_mean+ nrow(datapred["flagmethod"== "m",])
    
    print(paste('max loss:',max(datapred$loss_per_clean, na.rm=TRUE)*100, "%"))
    timeSeriesDataToBeImputed$geographicaream49 <- as.character(timeSeriesDataToBeImputed$geographicaream49)
    
    int1 <-datapred[,tolower(datasetN), with=F]
    nameadd <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
    names(int1)[!names(int1) %in% keys_lower] <- paste(names(int1)[!names(int1) %in% keys_lower],'a',sep="")
    int1 <- int1[!duplicated(int1),]
    
    print("Break 6")
    timeSeriesDataToBeImputed <-  merge(timeSeriesDataToBeImputed, int1, by=keys_lower, all.x= TRUE)
    timeSeriesDataToBeImputed %>% filter(is.na(protected))
    
    timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,flagcombination:= flagcombinationa,]
    timeSeriesDataToBeImputed[is.na(protected) & value_measuredelement_5016a>=0,loss_per_clean:= loss_per_cleana,]
    timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagobservationstatus := 'I',] 
    timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagmethod:= 'e',]
    timeSeriesDataToBeImputed[is.na(protected) & loss_per_cleana >0,flagcombination := paste(flagobservationstatus,flagmethod, sep=";"),]
    timeSeriesDataToBeImputed[,(nameadd):= NULL,]
    
    ##### Save model parameters
    
    SavResult <- list(cluster=name,formula=formula,coeffnames = paste(unlist(names(coefficients(mod2))), collapse = "##"),mean_intercept= mod2_rand$coefficients[1],coeff =paste(unlist(coefficients(mod2)), collapse = "##"),
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
  print("Break 7")
  # # Multiplies loss percentages by production
  # timeSeriesDataToBeImputed <- merge(timeSeriesDataToBeImputed,production[,c("geographicaream49", "measuredelement", "measureditemcpc", "timepointyears", "value_measuredelement_5510"), with=F], by.x = (keys_lower), by.y = (keys_lower), all.x = TRUE, all.y = FALSE)
  # timeSeriesDataToBeImputed[,value_measuredelement_5126 := loss_per_clean,]
  # timeSeriesDataToBeImputed[,value_measuredelement_5016 := value_measuredelement_5126*value_measuredelement_5510,]
  # timeSeriesDataToBeImputed <- timeSeriesDataToBeImputed %>% filter(!is.na(value_measuredelement_5016))
  # datasetN[datasetN=="loss_per_clean"] <- "value_measuredelement_5126"
  # 
  # ### Narrows the data to the CPCs in the loss domain
  # 
  # ### Splits the data tables for the SWS ####
  # timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5016","flagobservationstatus", "flagmethod") ,with=F] 
  # 
  # timeSeriesDataToBeImputed_5016[, measuredElement := "5016"]
  # setnames(timeSeriesDataToBeImputed_5016, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5016", "flagobservationstatus", "flagmethod","measuredElement" ),
  #          new =  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"  ,"Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  # 
  # 
  # setcolorder(timeSeriesDataToBeImputed_5016, 
  #             c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  # 
  # timeSeriesDataToBeImputed_5016 <- timeSeriesDataToBeImputed_5016 %>% filter(!is.na(flagMethod))
  # 
  # ##---------------------
  # timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed[,c(keys_lower,"value_measuredelement_5126","flagobservationstatus", "flagmethod") ,with=F] 
  # 
  # timeSeriesDataToBeImputed_5126[, measuredElement := "5126"]
  # setnames(timeSeriesDataToBeImputed_5126, old =  c("geographicaream49", "timepointyears","measureditemcpc" , "value_measuredelement_5126", "flagobservationstatus", "flagmethod","measuredElement" ),
  #          new =  c("geographicAreaM49","timePointYears", "measuredItemSuaFbs"  , "Value", "flagObservationStatus", "flagMethod","measuredElementSuaFbs") )
  # 
  # 
  # setcolorder(timeSeriesDataToBeImputed_5126, 
  #             c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )
  # 
  # timeSeriesDataToBeImputed_5126 <- timeSeriesDataToBeImputed_5126 %>% filter(!is.na(flagMethod))
  # 
  # DataSave <- rbind(timeSeriesDataToBeImputed_5016,timeSeriesDataToBeImputed_5126)
  # # # Save to the SWS
  # # stats = SaveData(domain = "lossWaste",
  # #                  dataset="loss",
  # #                  data = DataSave
  # # )
  # # 
  # 
  #end <- Sys.time()
  #print(end - start)
  print(paste("Number of estimated points: ",dim(timeSeriesDataToBeImputed[loss_per_clean>0 & is.na(protected),])[1],sep=""))
  print(paste("Percent of total: ",dim(timeSeriesDataToBeImputed[ !is.na(flagobservationstatus) & is.na(protected),])[1]/
                dim(timeSeriesDataToBeImputed[is.na(flagobservationstatus) & is.na(protected),])[1],sep=""))
  
  print(paste("Percent of total estimates done with the full model: ", model_mean))
  print(paste("Percent of total estimates done with the restricted model: ", model_restricted))
  print(paste("Percent of total estimates adjusted to mean: ", model_mean))
  
  #write_json(list(DataIN = Data), paste(dirmain,'\\ModelResults\\',HierarchicalCluster,'_ModelResults.json',sep="")) 
  return(timeSeriesDataToBeImputed)
  
}
