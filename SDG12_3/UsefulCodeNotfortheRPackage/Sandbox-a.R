library(lme4)

#first case simple lmer, simulate 100 data points from 10 groups with one continuous fixed effect variable
x<-runif(100,0,10)
f<-gl(n = 6,k = 10)
data<-data.frame(x=datamod_ctry[,c(ImportVar2),with=F])
modmat<-model.matrix(~x,data)
#the fixed effect coefficient
fixed<-c(1,0.5)
#the random effect
rnd<-rnorm(10,0,0.7)
#the simulated response values
data$y<- datamod_ctry$losstransf

#model
m<-plm(as.formula(formula_ctry), data = datamod_ctry , index=c("measureditemcpc"), model ="pooling")

#first CI and PI using predict-like method, using code posted here: http://glmm.wikidot.com/faq
newdat<-data.frame(x=seq(0,10,length=20))
mm<-model.matrix(~x,newdat)
newdat$y<-mm%*%fixef(m) 
#predict(m,newdat,re.form=NA) would give the same results
pvar1 <- diag(mm %*% tcrossprod(vcov(m),mm))
tvar1 <- pvar1+VarCorr(m)$f[1] # must be adapted for more complex models
newdat <- data.frame(
  newdat
  , plo = newdat$y-1.96*sqrt(pvar1)
  , phi = newdat$y+1.96*sqrt(pvar1)
  , tlo = newdat$y-1.96*sqrt(tvar1)
  , thi = newdat$y+1.96*sqrt(tvar1)
)

#second version with bootMer
#we have to define a function that will be applied to the nsim simulations
#here we basically get a merMod object and return the fitted values
predFun<-function(.) mm%*%fixef(.) 
bb<-bootMer(m,FUN=predFun,nsim=200) #do this 200 times
#as we did this 200 times the 95% CI will be bordered by the 5th and 195th value
bb_se<-apply(bb$t,2,function(x) x[order(x)])
newdat$blo<-bb_se[1,]
newdat$bhi<-bb_se[2,]

plot(y~x,data)
lines(newdat$x,newdat$y,col="red",lty=2,lwd=3)
lines(newdat$x,newdat$plo,col="blue",lty=2,lwd=2)
lines(newdat$x,newdat$phi,col="blue",lty=2,lwd=2)
lines(newdat$x,newdat$tlo,col="orange",lty=2,lwd=2)
lines(newdat$x,newdat$thi,col="orange",lty=2,lwd=2)
lines(newdat$x,newdat$bhi,col="darkgreen",lty=2,lwd=2)
lines(newdat$x,newdat$blo,col="darkgreen",lty=2,lwd=2)
legend("topleft",legend=c("Fitted line","Confidence interval","Prediction interval","Bootstrapped CI"),col=c("red","blue","orange","darkgreen"),lty=2,lwd=2,bty="n")