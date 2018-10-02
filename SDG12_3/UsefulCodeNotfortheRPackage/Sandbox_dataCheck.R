

#'Data Check with the SUA Unbalanced

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


EstData <- DataSave %>% filter( measuredElementSuaFbs=="5016") 
## Compare
f1 <- EstData %>% filter((geographicAreaM49 %in% SUA_nonadj$geographicAreaM49))
f2 <- f1 %>% filter((measuredItemSuaFbs %in% SUA_nonadj$measuredItemSuaFbs))
f3 <- f2 %>% filter((timePointYears %in% SUA_nonadj$timePointYears))

SUA_nonadj$geographicAreaM49 <- as.character(SUA_nonadj$geographicAreaM49)
typeof(SUA_nonadj$geographicAreaM49)

toCompare <- merge(SUA_nonadj,f3,by=c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"))
toCompare[, "difference" := Value.x-Value.y,]

toCompare %>% filter(difference ==0)
write.table(toCompare , "C:/Users/Englisha.FAODOMAIN/Desktop/SUA_balanced_compare_15AUg18.csv", sep=",")
