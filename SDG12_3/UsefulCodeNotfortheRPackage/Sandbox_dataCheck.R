library(openxlsx)
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5016')

#'Data Check with the SUA Unbalanced
load("C:\\Users\\Englisha.FAODOMAIN\\Documents\\faoswsLossa\\data\\July_Estimates.RData")
names(July_Estimates) <- tolower(names(July_Estimates))
SUAdata <- read.table("C:/Users/Englisha.FAODOMAIN/Desktop/SUA_balanced.csv", header=TRUE, sep=",")
SUAdata$measuredItemFbsSua <- addHeadingsCPC(SUAdata$measuredItemFbsSua)
#geographicAreaM49 measuredElementSuaFbs measuredItemSuaFbs timePointYears      Value flagObservationStatus flagMethod
data1 <- SUAdata[,1:4]
data_00 <- as.data.table(cbind(data1,SUAdata[,7:9]))
data_01 <- as.data.table(cbind(data1,SUAdata[,10:12]))
data_02 <- as.data.table(cbind(data1,SUAdata[,13:15]))
data_03 <- as.data.table(cbind(data1,SUAdata[,16:18]))
data_04 <- as.data.table(cbind(data1,SUAdata[,19:21]))
data_05 <- as.data.table(cbind(data1,SUAdata[,22:24]))
data_06 <- as.data.table(cbind(data1,SUAdata[,25:27]))
data_07 <- as.data.table(cbind(data1,SUAdata[,28:30]))
data_08 <- as.data.table(cbind(data1,SUAdata[,31:33]))
data_09 <- as.data.table(cbind(data1,SUAdata[,34:36]))
data_10 <- as.data.table(cbind(data1,SUAdata[,37:39]))
data_11 <- as.data.table(cbind(data1,SUAdata[,40:42]))
data_12 <- as.data.table(cbind(data1,SUAdata[,43:45]))
data_13 <- as.data.table(cbind(data1,SUAdata[,46:48]))
data_15 <- as.data.table(cbind(data1,SUAdata[,49:51]))
data_16 <- as.data.table(cbind(data1,SUAdata[,52:54]))
data_17 <- as.data.table(cbind(data1,SUAdata[,55:57]))

data_00 	[,'timePointYears' :=	2000	,]
data_01 	[,'timePointYears' :=	2001	,]
data_02 	[,'timePointYears' :=	2002	,]
data_03 	[,'timePointYears' :=	2003	,]
data_04 	[,'timePointYears' :=	2004	,]
data_05 	[,'timePointYears' :=	2005	,]
data_06 	[,'timePointYears' :=	2006	,]
data_07 	[,'timePointYears' :=	2007	,]
data_08 	[,'timePointYears' :=	2008	,]
data_09 	[,'timePointYears' :=	2009	,]
data_10 	[,'timePointYears' :=	2010	,]
data_11 	[,'timePointYears' :=	2011	,]
data_12 	[,'timePointYears' :=	2012	,]
data_13 	[,'timePointYears' :=	2013	,]
data_15 	[,'timePointYears' :=	2014	,]
data_16 	[,'timePointYears' :=	2015	,]
data_17 	[,'timePointYears' :=	2016	,]

data_01[1:4,]

data_00 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_01 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_02 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_03 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_04 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_05 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_06 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_07 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_08 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_09 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_10 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_11 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_12 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_13 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_15 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_16 	[,	c("Geographic.Area.M49","Item")	:= NULL,]
data_17 	[,	c("Geographic.Area.M49","Item")	:= NULL,]

names(data_00)[3] <- "Value"
names(data_00)[2] <- "measuredItemSuaFbs"

names(data_01 ) <-  names(data_00)
names(data_02 ) <-  names(data_00)
names(data_03 ) <-  names(data_00)
names(data_04 ) <-  names(data_00)
names(data_05 ) <-  names(data_00)
names(data_06 ) <-  names(data_00)
names(data_07 ) <-  names(data_00)
names(data_08 ) <-  names(data_00)
names(data_09 ) <-  names(data_00)
names(data_10 ) <-  names(data_00)
names(data_11 ) <-  names(data_00)
names(data_12 ) <-  names(data_00)
names(data_13 ) <-  names(data_00)
names(data_15 ) <-  names(data_00)
names(data_16 ) <-  names(data_00)
names(data_17 ) <-  names(data_00)


SUA_nonadj <-rbind(data_00,data_01,data_02,data_03,data_04,data_05,data_06,data_07,data_08,data_09,data_10,data_11,data_12,data_13,data_15,data_16,data_17)
SUA_nonadj[,"measuredElementSuaFbs" := 5016,]
setcolorder(SUA_nonadj, 
            c("geographicAreaM49", "measuredElementSuaFbs" ,"measuredItemSuaFbs" ,"timePointYears", "Value", "flagObservationStatus", "flagMethod") )

names(SUA_nonadj) <- tolower(names(SUA_nonadj))
names(Losses) <- tolower(names(Losses))
Losses <-Losses[timepointyears >=2000,]
EstData <- Losses 
## Compare
f1 <- EstData %>% filter((geographicaream49 %in% SUA_nonadj$geographicaream49))
f2 <- f1 %>% filter((measuredItemSuaFbs %in% SUA_nonadj$measuredItemSuaFbs))
f3 <- f2 %>% filter((timePointYears %in% SUA_nonadj$timePointYears))

SUA_nonadj$geographicaream49 <- as.character(SUA_nonadj$geographicaream49)
Losses$geographicaream49 <- as.character(Losses$geographicaream49)

typeof(SUA_nonadj$geographicaream49)

toCompare <- merge(SUA_nonadj,Losses,by=c("geographicaream49", "measureditemsuafbs", "timepointyears"), all =T)
toCompare[geographicaream49 %in% SUA_nonadj$geographicaream49,]
unique(SUA_nonadj$geographicaream49)

toCompare[, "difference" := value.x-value.y,]
toCompare[is.na(difference) & value.x>=0,"difference" := value.x]
toCompare[is.na(difference) & value.y>=0,"difference" := -value.y]
toCompare <- toCompare[!(is.na(difference) &is.na(value.x)),]

productionNum <- prod_imports
protectedLossData<-lossData

names(toCompare) <- tolower(names(toCompare))
names(productionNum)[names(productionNum) == "measureditemcpc" ] <- "measureditemsuafbs"
names(toCompare)[names(toCompare) == "value.x" ] <- "value_fbsteam"
names(toCompare)[names(toCompare) == "value.y" ] <- "value_newmodel"

toCompare2 <-merge(toCompare, productionNum, by=c("geographicaream49", "timepointyears", "measureditemsuafbs"), all.x= T)


toCompare2[ , percent_prod := value_fbsteam/value_measuredelement_5510]
toCompare2[ , percent_prod2 := value_newmodel/value_measuredelement_5510]

names(fbsTree) <- tolower(names(fbsTree))
toCompare2 <- merge(toCompare2, fbsTree[,c("measureditemcpc", "gfli_basket"),with=F], by.x =c("measureditemsuafbs"),by.y =c("measureditemcpc"), all.x=T)
protectedLossData <-  merge(protectedLossData , fbsTree[,c("measureditemcpc", "gfli_basket"),with=F], by =c("measureditemcpc"))

## add in the old estimates ##
July_Estimates_5016 <- July_Estimates[measuredelementsuafbs == 5016,]
July_Estimates_5126 <- July_Estimates[measuredelementsuafbs == 5126,]
names(July_Estimates_5126)[names(July_Estimates_5126)=="value"] <- "July_5126_value"
names(July_Estimates_5016)[names(July_Estimates_5016)=="value"] <- "July_5016_value"
July_Estimates_5126[,measuredelementsuafbs := NULL]
July_Estimates_5016[,measuredelementsuafbs := NULL]
July_Estimates2 <- July_Estimates_5126
July_Estimates2 <- unique(July_Estimates2)
July_Estimates2 <-July_Estimates2[timepointyears>=2000,]


toCompare3 <- merge(toCompare2, July_Estimates2 ,  by=c("geographicaream49","measureditemsuafbs","timepointyears"), all.x=T, all.y=F)
toCompare3[, combp := paste(geographicaream49,measureditemsuafbs, sep=";")]

### Add the countries that have the loss percentage divided by the production plus imports ###
toCompare3[toCompare3$combp %in% comb, percent_prod := value_fbsteam/prod_imports]
toCompare3[toCompare3$combp %in% comb, percent_prod2 := value_newmodel/prod_imports]
toCompare3$per_prodimp <- F
toCompare3[toCompare3$combp %in% comb, per_prodimp := T]

### Add which country and years are modeled by country and which are modeled in the global ##
# toCompare3$model <- ""
# Losses[,flagcombination:= paste(flagObservationStatus ,flagmethod, sep=";")]
# 
# ctrymodel <- unique(Losses[flagcombination == "I;e",c("geographicaream49",  "measuredElementSuaFbs"), with=F])
# ctrymodel[, combp := paste(geographicaream49,measuredElementSuaFbs, sep=";")] 

# toCompare3[toCompare3$combp %in% ctrymodel$combp & modeled_flagcombo == "I;e",model := "ctry"]
# toCompare3[ modeled_flagcombo == "I;e",model := "global"]

##### Missing data #####
Mflag <- unique(toCompare3[modeled_flagcombo == "M;-",c("geographicaream49","measureditemsuafbs"),with=F])
checksM <- c()
toCompare3[,modeled_flagcombo := paste(flagobservationstatus.y ,flagmethod.y, sep=";")] 
for( mm in 1:nrow(Mflag)){
  mm2 <- toCompare3[geographicaream49== Mflag$geographicaream49[mm] & measureditemsuafbs == Mflag$measureditemsuafbs[mm],]
  if(!all(unique(mm2$modeled_flagcombo) %in% c("M;-","I;e"))){
    checksM <- rbind(checksM, mm2)
  }
}

toCompare3[,combp := NULL]

for(t in unique(toCompare3$geographicaream49)){
  wb <- createWorkbook()
  for(i in unlist(na.omit(unique(toCompare2[geographicaream49== t,"gfli_basket",with=F])))){
    addWorksheet(wb, unlist(strsplit(as.character(i), "&"))[1])
    check1 <- toCompare3[geographicaream49 == t & gfli_basket == unname(unlist(i)) ,]
    writeDataTable(wb, unlist(strsplit(as.character(i), "&"))[1], check1, startRow = 1, startCol = 1 ,withFilter = T)  
  }
  
  saveWorkbook(wb, file =  paste("comparison_balanced_Newmodel", t,".xlsx",sep=""), overwrite = TRUE)
  
}

addWorksheet(wb, "MissingMixed")
writeDataTable(wb, "MissingMixed",checksM, startRow = 1, startCol = 1 ,withFilter = T)  
saveWorkbook(wb, file =  paste("comparison_balanced_Newmodel", "Missing",".xlsx",sep=""), overwrite = TRUE)
