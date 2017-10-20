#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 

LossModel <- function(Data,DataPred,flag){
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
  #	Flag: is for the group/cluster ("foodGroupName" was the best preformer)
  
  
  setwd(paste(dirmain,'\\variables\\general\\',sep="")) 
  minobs <- 4
  minctry <- 2
  load(paste(githubsite, 'General/fbsTree.RData',sep=""))
  
  ##### PART 1 - Data trasnformations and Clusters ####
  # The flags set up the clusters for the analysis - as several flags were tested
  if(flag == "foodGroupName" | flag == "foodPerishableGroup"){
    index1 <- c("ISOCode")
    DV <- c("Crop")
    modelstr <- 'random'
  }
  if(flag == "SDG.Regions"){
    index1 <- c("Crop")
    DV <-  c("ISOCode")
    modelstr <- 'random'
  }
  if(flag == "ISOCode"){
    index1 <- c("Crop")
    DV <-  c("ISOCode")
    modelstr <- 'within'
  }
  
  # Prepares the data for the analysis
  Data[,Loss_Per_clean := Loss_Per_clean/100 ]
  Data[Loss_Per_clean == 0,Loss_Per_clean := 0.0001,]
  Data[Loss_Per_clean == 1,Loss_Per_clean :=  0.9999,] 
  
  Data[,losstransf := log(Loss_Per_clean/(1-Loss_Per_clean)) ]
  colnames(Data) <- gsub("[[:punct:]]","_",colnames(Data)) 
  colnames(DataPred) <- gsub("[[:punct:]]","_",colnames(DataPred)) 
  IdentVar <- c("ID",'Country','ISOCode','M49Code',"Crop","measuredItemCPC",'SDG_Regions',"measuredItemFCL",
                'FSC_Location','foodGroupName','Loss_Per_clean')
  
  factorVar <- names(Data)[sapply(Data,is.factor) == T]
  for(jy in factorVar[! factorVar %in% IdentVar] ){
    if((sum(grep("[A-z]",levels(Data[,jy,with=FALSE]))) ==0 )){ 
      Data[,names(Data[,jy,with=FALSE]):= factortoNumeric(Data[,jy,with=FALSE]),]  
    }
  }
  factorVar2 <- names(DataPred)[sapply(DataPred,is.factor) == T]
  for(jy in factorVar2[! factorVar2 %in% IdentVar] ){
    if((sum(grep("[A-z]",levels(DataPred[,jy,with=FALSE]))) ==0 )){ 
      DataPred[,names(DataPred[,jy,with=FALSE]):= factortoNumeric(DataPred[,jy,with=FALSE]),]  
    }
  }
  
  newcol  <- names(sapply(Data, is.numeric))[!(names(sapply(Data, is.numeric)) %in% IdentVar)]
  
  ##### PART 2 - Random Forest ####
  for (vi in 1:length(unique(Data$foodGroupName)[!is.na(unique( Data$foodGroupName))])){
    # for each subgroup in the cluster the model is created - with new varaibles selected
    name = unique(Data_Use_train$foodGroupName)[!is.na(unique(Data_Use_train$foodGroupName))][vi]
    modrun <- 0
    # remove countries with less than 2 datapoints
    idx_country <- unique(as.character(Data$ISOCode))[(table(as.character(Data$ISOCode))>=2) == T]
    data1 <- Data %>% filter((ISOCode %in% idx_country) & (Data[,flag,with=FALSE]== name))
    data1 <- data1[,names(data1) %in%  c(newcol,index1, DV) ,with=FALSE] #'Year'
    data1 <- data1 %>% filter(!(is.na(ISOCode) | (ISOCode== "")))
    ###
    
    # Section determines which variables are the most useful by country
    Use_varibles <- list() 
    AllVar <- list() 
    Use_varibles <- colnames(data1)
    drops2 <- c("lag1yr","lag2yr", "lag3yr")
    Use_varibles <- Use_varibles[!(Use_varibles %in% drops2)] 
    
    NumImportVarUse = 8
    
    for(ctry in unique(data1$ISOCode)){
      data_country <- data1 %>% filter(ISOCode == ctry)
      
      #print(ctry)
      if(nrow(data_country)>=2){
        # Runs the Random Forest Algorithm to find the most important variables in the dataset
        #Deletes columns that might have nulls in the column
        idxcol <- apply(apply(data_country,2,is.na),2,sum) == dim(data_country)[1]
        cols <- c(colnames(data1)[colnames(data1) %in% newcol[!newcol  %in% idxcol]])
        fit <- rpart(losstransf ~ ., data = data_country[, unique(cols), with=FALSE],control=rpart.control(minsplit=30, cp=0.1))
        ImportVar <- names(fit$variable.importance)
        
        if(!is.null(ImportVar)){
          # gets rid of variables that are highly correlated in the important variables 
          Data[,sapply(Data, is.numeric) ==TRUE, with=FALSE]
          dropd <- unique(ImportVar[rowSums(((cor(data_country[, colnames(data_country) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs")>.85) & (cor(data_country[, colnames(data_country) %in% unique(c(ImportVar)), with=FALSE],use="pairwise.complete.obs") < 1))*1) >0])
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
      v2 <-names(V[0:(NumImportVarUse -length(Use_varibles))])
      Use_varibles2 <- c(Use_varibles2[!is.na(Use_varibles2)],v2 )
      
    }
    ################################  
    #Trims the variables to the the ones going to be used in the model  - included from the random forests
    datapred <- DataPred[, colnames(DataPred)[colnames(DataPred) %in% unique(c(colnames(FullSet),flag, index1,DV,"losstransf","SDG_Regions",Use_varibles2[1:NumImportVarUse]))],with=FALSE] #, "Year"
    datapred <- datapred[,colSums(is.na(datapred))<nrow(datapred),with=FALSE]
    Use_varibles2 <- Use_varibles2[Use_varibles2 %in% names(datapred[,colSums(is.na(datapred))<nrow(datapred),with=FALSE])]
    
    datamod <- data1[,  names(data1)[names(data1) %in% unique(c(index1,"losstransf",Use_varibles2[1:NumImportVarUse], DV,"Year"))], with=FALSE] #
    if(flag != 'ISOCode'){datamod <- datamod %>% filter(ISOCode !="")}
    datamod <-  na.omit(datamod)
    
    datapred <-  na.omit(datapred)
    UseV <- unique(Use_varibles2[1:NumImportVarUse])
    if(dim(datamod)[1] < minobs | length(unique(datamod$ISOCode)) < minctry){
      sprintf('Not enough Observations to model %s',name)
      next
    }
    ##### PART 3 - Heirarchical model ####
    if((length(index1)!=0) & (length(unique(c(UseV[!is.na(UseV)],index1,DV))) > 2)){
      ## Model 
      formula <- paste("losstransf ~ ", paste(unique(c(UseV[!is.na(UseV)],index1,DV,"Year")), collapse= "+")) #
      mod2 <- plm(as.formula(formula), data = datamod, index= c(index1), model =modelstr )
      modrun <- 1
      #For Predictions
      #Yhat_Sindex =data.frame(rowSums(data.frame(mapply(`*`,datapred[,names(coefficients(mod2))],coefficients(mod2))),na.rm = T))
      ###################################    
      #DV <- names(fixef(mod2))
      if(index1 == "Crop"){PD_V2 <- unique(unlist(c(datapred[,index1,with=FALSE])))} 
      if(index1 == "ISOCode"){PD_V2 <- levels(unlist(c(datapred[,index1,with=FALSE])))}
      if(modelstr == "within"){  
        # For each of the items in the index
        coeffN <- unique(c(Use_varibles2))
        for(ind1 in 1:length(unique(DV))){
          datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]) ,]$losstransf =
            fixef(mod2)[ind1] +coefficients(mod2)[names(coefficients(mod2))[ind2]]+
            rowSums(data.frame(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],  datapred[ which(datapred[,index1,with=FALSE] ==  names(fixef(mod2)[ind1]) & datapred[,DV,with=FALSE] ==  gsub(DV,"", names(coefficients(mod2)))[ind2]),coeffN,with=FALSE]))) 
        }
      }
      if(modelstr == "random"){  
        # COmbines the coefficients to create an estimate for every column in the group
        coeffN <- c(Use_varibles2[!(Use_varibles2 %in% index1 | Use_varibles2 %in% DV | Use_varibles2 %in%  "Loss_Per_clean")])
        coeffN <- na.omit(coeffN) 
        length(unique(unlist(c(datamod[,index1,with=FALSE]))))
        
        coeffindex <-  grep(index1,names(coefficients(mod2)), perl=TRUE, value=TRUE)
        coeffDV <- grep(DV,names(coefficients(mod2)), perl=TRUE, value=TRUE)
        
        datapred[,CountyDummy :=0]
        datapred[,CropDummy :=0]
        
        for(ind1 in 1:length(unique(gsub(index1,"", coeffindex)))){
          datapred[ISOCode == gsub(index1,"", coeffindex)[ind1],CountyDummy := as.numeric(coefficients(mod2)[coeffindex[ind1]]),]
        }
        for(ind2 in 1:length(unique(gsub(DV,"", coeffDV)))){ 
          datapred[Crop == gsub(DV,"", coeffDV)[ind2],CropDummy:= as.numeric(coefficients(mod2)[coeffDV[ind2]]),]
          
        }
        if(length(coeffN) >0){
          # Applies the weights of the estimation across the entire cluster sets
          datapred[,losstransf :=  coefficients(mod2)[1] +
                     rowSums(mapply(`*`,coefficients(mod2)[names(coefficients(mod2)) %in% coeffN],datapred[ ,names(coefficients(mod2)[names(coefficients(mod2)) %in% coeffN]), with=FALSE]))+
                     CountyDummy+CropDummy,] 
        }else{ datapred[,losstransf := coefficients(mod2)[1] + CountyDummy+CropDummy,]}
      }
    }else{mod2 = lm(losstransf ~ ., data = datamod)}
    if(modrun ==1){
      #Transform the losses back to % and not logged numbers 
      datapred$Loss_Per_clean[datapred$losstransf !=0] = exp(datapred$losstransf[datapred$losstransf !=0])/(1+exp(datapred$losstransf[datapred$losstransf !=0]))
      
      # Add the preditcted values back to the Total Predict set
      DataPred[ID == datapred$ID[1],,]
      for(rowNum in 1:nrow(datapred)){
        DataPred[ID == datapred$ID[rowNum],Loss_Per_clean := datapred[ID == datapred$ID[rowNum],Loss_Per_clean,],]
      }     
      print(name)
      # Write the different models to json format
      write_json(list(version =name, modelForm = formula,coeff = as.data.frame(coefficients(mod2)), Resultdata = datapred), paste(dirmain,'\\ModelResults\\',flag,'_',name,'_ModelResults.json',sep=""))
    }
    
  }  
  write_json(list(DataIN = Data), paste(dirmain,'\\ModelResults\\',flag,'_ModelResults.json',sep="")) 
  return(DataPred)
  
}


