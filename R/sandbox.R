#------- Create a Mean Centered dataset ---- #
tma2 <- datamod
tma2 <- tma2[,!names(tma2) %in% c('geographicaream49',"m49CPC"),with=F]
tma2 <- tma2[, lapply(.SD, mean), by = c("timepointyears", "measureditemcpc")]
fitTMA <- rpart(losstransf ~ ., data = tma2[,!names(  tma2) %in% unique(c(keys_lower, drops1)),with=F] ,control=rpart.control(minsplit=30, cp=0.001))
ImportVarTMA <- names(fitTMA$variable.importance)[1:NumImportVarUse]

UseVariTMA <- na.omit(unique(c(keys_lower,depVar,ImportVarTMA)))
datamodTMA <- datamod[,UseVariTMA,with=F]
#### Exclude NaN
datamodTMA <-datamodTMA[complete.cases(datamodTMA), ]

#Scale down the explanatory vari
scaled <- svd((datamodTMA[,ImportVarTMA,with=F]))
datamodTMA[,ImportVarTMA] <-scaled$u




#### Exclude NaN
datamod <- datamod[complete.cases(datamod), ]

#Scale down the explanatory vari
scaled <- svd((datamod[,ImportVar,with=F]))
datamod[,ImportVar] <-scaled$u
scaledDV <- svd((datamod[,depVar,with=F]))
datamod[,depVar] <-scaledDV$u

ImportVar <- names(rowSums(var(datamod[,ImportVar,with=F])))[rowSums(var(datamod[,ImportVar,with=F])) >0]
UseVari <- unique(c(keys_lower,depVar,ImportVar))
datamod <- datamod[,UseVari,with=F]


mod2_rlm <- lm(as.formula(formulaTMA), data = datamodTMA%>% filter(timepointyears>1990))
mod2_rand <- plm(as.formula(formulaTMA), data = datamodTMA%>% filter(timepointyears>1990) , index=c("measureditemcpc"), model ="random")

formula2 <- paste(paste(depVar," ~",sep=""), paste("factor(", keys_lower, ")",sep="", collapse= " + "),'+',paste(unique(UseVari[!UseVari %in% c(depVar,keys_lower)]), collapse= " + ")) #
formula3 <- paste(paste(depVar," ~",sep=""), 
                  keys_lower[2],'+',paste(unique(UseVari[!UseVari %in% c(depVar,keys_lower)][1]), collapse= " + ")) #
formulaTMA <- paste(paste(depVar," ~",sep=""), paste(keys_lower[2:3], collapse= " + "),'+',paste(unique(UseVariTMA[!UseVariTMA %in% c(depVar,keys_lower)]), collapse= " + ")) #



# sumr <- summary(mod2_rand)
# pr_rand <- pmodel.response(mod2_rand)
# MSE_rand <- sum((pr_rand  - test[,depVar,with=F])^2)/nrow(test)


mod2_fixed<- plm(as.formula(formula), data = tma2%>% filter(timepointyears>1990) , index=c("measureditemcpc"), model ="within")

mod2_ex1<- glm(as.formula(formula), data =train)
summary(mod2_ex1)
pr <- predict(mod2_ex1,test)
MSE <- sum((pr - test[,depVar,with=F])^2)/nrow(test)

mod2_ex2<- neuralnet(as.formula(formula3),data= train[,c('losstransf', keys_lower[2],UseVari[!UseVari %in% c(depVar,keys_lower)][1]),with=F],hidden=c(5,3),linear.output=T)
pr.nn  <- compute(mod2_ex2,test[,c(keys_lower[2],UseVari[!UseVari %in% c(depVar,keys_lower)][1]),with=F])
pr.nn_ <- pr.nn$net.result*(max(train$losstransf)-min(train$losstransf))+min(train$losstransf)
test.r <- (test$losstransf)*(max(datamod$losstransf)-min(datamod$losstransf))+min(datamod$losstransf)
sapply(pr.nn_,CB)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

par(mfrow=c(1,2))
plot(test$losstransf,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$losstransf,pr,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

mod2 <-  mod2_rand
# # test for heterskedastic err
# r <- bptest(as.formula(formula), data = datamod, studentize=F)
# if(r$p.value < 0.05){
#   formulatma <- paste("losstransf ~ ", paste(unique(UseVari)[!unique(UseVari) %in% c(depVar,'geographicaream49')], collapse= "+"))
#   mod2_randtma <- plm(as.formula(formulatma), data =  tma2, index= c("measureditemcpc"), model ="random")
#   mod2_fixedtma <- plm(as.formula(formulatma), data = tma2, index=  c("measureditemcpc"), model ="within")
#   mod2_poolingtma <- plm(as.formula(formulatma), data = tma2, index=  c("measureditemcpc"), model ="pooling")
#   phtest(mod2_fixedtma, mod2_randtma)
#   r <- bptest(as.formula(formulatma), data = tma2, studentize=F)
#    
#    
#     coeffSig <- mod2_randHet[,4][mod2_randHet[,4] <.1]
#     }

