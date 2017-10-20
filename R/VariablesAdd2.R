#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' 

VariablesAdd2 <- function(DataUseInt){  
  setwd(paste(dirmain,'\\variables\\general\\',sep="")) 
  
  #######################################
  ####Import by .Rdata ####
  ########### FAO In-House Data  ################### 
  ## Perishability  ################### 
  load("foodPerishableGroup.RData")
  foodPerishableGroup$measuredItemCPC
  foodPerishableGroup$foodGroupName <- as.character(foodPerishableGroup$foodGroupName)
  foodPerishableGroup$foodGeneralGroup <- as.character(foodPerishableGroup$foodGeneralGroup)
  typeof(foodPerishableGroup$foodGroupName)
  
  DataUseInt = merge(DataUseInt, foodPerishableGroup, by.x = c('measuredItemCPC'), by.y = c('measuredItemCPC'), 
                     all.x = TRUE, all.y = FALSE)
  
  #Adds the regional groupings and 3 years of  time lag
  CountryGroup <- read.csv('a2017regional groupings02Feb2017.csv')   ## Import CSV
  CountryGroup$CountryName <- tolower(CountryGroup$CountryName)
  DataUseInt <- merge(DataUseInt , CountryGroup[, c('ISOCode', 'SDG.Regions')], by.x = c('ISOCode'),
                      by.y = c('ISOCode'), all.x = TRUE, all.y = FALSE)
  
  DataUseInt[,lag1yr:= Year - 1]
  DataUseInt$lag2yr <- DataUseInt$Year - 2
  DataUseInt$lag3yr <- DataUseInt$Year - 3
  
  ######## Weather Data  ################ 
  ConvFactor_cal <- unique(DataUseInt[, c('ISOCode', 'Year')])
  Temperature <- read.csv("Temp_climate.csv")
  Temperature <- subset(Temperature, select = c(Temperature.C, Year, Month, Country))
  Temperature <- merge(Temperature, ConvFactor_cal, by.x = c('Country', 'Year'), by.y = c('ISOCode', 'Year'), all.x = FALSE, all.y = TRUE)
  
  Precipitation <- read.csv("Rain_climate.csv")
  Precipitation <- subset(Precipitation, select = c(Rainfall.mm, Year, Month, Country))
  Precipitation <- merge(Precipitation, ConvFactor_cal, by.x = c('Country', 'Year'), by.y = c('ISOCode', 'Year'), all.x = FALSE, all.y = TRUE)
  
  
  CropCalendar <- read.csv("AllCropCalendar.csv")
  CropCalendar <- CropCalendar[, c("Country", "Crop", "Harvesting_month_onset", "Harvesting_month_end")]
  CropCalendar$Country <- tolower(CropCalendar$Country)
  CountryGroup$CountryName <- tolower(CountryGroup$CountryName)
  CropCalendar$Crop <- tolower(CropCalendar$Crop)
  
  CropCalendar1 <- merge(CropCalendar, CountryGroup[, c('CountryName', 'ISOCode')], by.x = c('Country'), by.y = c('CountryName'), all.x = TRUE, all.y = FALSE)
  AggCropCalendar <- aggregate(Harvesting_month_end~ ISOCode+Crop, data = CropCalendar1, min)
  
  CropCalendar2 <- merge(AggCropCalendar, ConvFactor_cal, by.x = c('ISOCode'), by.y = c('ISOCode'), all.x = FALSE, all.y = TRUE)
  CropCalendar2$Year <- as.integer(CropCalendar2$Year)
  
  Temperature2 <- merge(CropCalendar2, Temperature, by.x = c('ISOCode', 'Harvesting_month_end', 'Year'), by.y = c('Country', 'Month', 'Year'), all.x = TRUE, all.y = FALSE)
  Precipitation2 <- merge(CropCalendar2, Precipitation, by.x = c('ISOCode', 'Harvesting_month_end', 'Year'), by.y = c('Country', 'Month', 'Year'), all.x = TRUE, all.y = FALSE)
  
  DataUseInt <- merge(DataUseInt, Temperature2, by.x = c('ISOCode', 'Year', 'Crop'), by.y = c('ISOCode', 'Year', 'Crop'), all.x = TRUE, all.y = FALSE)  ### FInal Data set @@
  DataUseInt <- merge(DataUseInt, Precipitation2, by.x = c('ISOCode', 'Year', 'Crop'), by.y = c('ISOCode', 'Year', 'Crop'), all.x = TRUE, all.y = FALSE)  ### FInal Data set @@
  
  ####Import By Spreadsheet###############
  setwd(paste(dirmain,'\\variables\\byYear\\',sep=""))  
  for(iy in 1:length(dir())){
    data <- lapply(dir()[iy],read.csv) 
    vr<-as.data.frame(data)
    DataUseInt <- merge(DataUseInt,vr, by.x = c('Year'), by.y = c('Year'), all.x = TRUE, all.y = FALSE)
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,vr, by.x = c('lag1yr'), by.y = c('Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr),'lag1yr', sep='.')[2:length(paste(colnames(vr),'lag1yr', sep='.'))]
    
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,vr, by.x = c('lag2yr'), by.y = c('Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr),'lag2yr', sep='.')[2:length(paste(colnames(vr),'lag2yr', sep='.'))]
    
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,vr, by.x = c('lag3yr'), by.y = c('Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr),'lag3yr', sep='.')[2:length(paste(colnames(vr),'lag3yr', sep='.'))]
  }
  
  setwd(paste(dirmain,'\\variables\\byCtryYear\\',sep=""))  
  for(iy in 1:length(dir())){
    drops = c('Area.Code',
              'Domain',  
              'Domain.lag1yr',
              'Domain.lag2yr',
              'Domain.lag3yr',
              'Element',
              'Element.Code', 
              'Flag.Description',
              'Flag.lag1yr',
              'Flag.lag2yr',
              'Flag.lag3yr',
              'ISOCode.lag1yr',
              'ISOCode.lag2yr',
              'ISOCode.lag3yr',
              'ISOCode.y',
              'Item.Code',
              'Item',
              'Unit.lag1yr',
              'Unit.lag2yr',
              'Unit.lag3yr'
              
    )
    
    data_crtyYR <- lapply(dir()[iy],read.csv) 
    
    vr_crtyYR <-as.data.frame(data_crtyYR)
    vr_crtyYR$Country <- tolower(vr_crtyYR$Country)
    if(is.factor(vr_crtyYR$Year)){vr_crtyYR$Year <- factortoNumeric(vr_crtyYR$Year)}
    DataUseInt  <-  merge(DataUseInt,  vr_crtyYR, by.x = c('Country', 'Year'),  by.y = c('Country', 'Year'), all.x = TRUE, all.y = FALSE)
    DataUseInt <- DataUseInt %>% subset(., select=which(!duplicated(names(.)))) 
    
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,  vr_crtyYR, by.x = c('Country', 'lag1yr'), by.y = c('Country', 'Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr_crtyYR)[!(colnames(vr_crtyYR) %in% c('Country', 'Year'))],'lag1yr', sep='.')
    DataUseInt <- DataUseInt %>% subset(., select=which(!duplicated(names(.)))) 
    
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,  vr_crtyYR, by.x = c('Country', 'lag2yr'), by.y = c('Country', 'Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr_crtyYR)[!(colnames(vr_crtyYR) %in% c('Country', 'Year'))],'lag2yr', sep='.')
    DataUseInt <-DataUseInt %>% subset(., select=which(!duplicated(names(.)))) 
    
    a <- dim(DataUseInt)[2]
    DataUseInt  <-  merge(DataUseInt,  vr_crtyYR, by.x = c('Country', 'lag3yr'), by.y = c('Country', 'Year'), all.x = TRUE, all.y = FALSE)
    colnames(DataUseInt)[(a+1):dim(DataUseInt)[2]]  = paste(colnames(vr_crtyYR)[!(colnames(vr_crtyYR) %in% c('Country', 'Year'))],'lag3yr', sep='.')
    DataUseInt <-DataUseInt %>% subset(., select=which(!duplicated(names(.)))) 
    
    #    DataUseInt[,c(names(DataUseInt)[names(DataUseInt) %in% drops]) := NULL] 
    #DataUseInt = DataUseInt[,!(names(DataUseInt) %in% drops)]  <*>
  }
  colnames(DataUseInt)[6] <- "ISOCode"
  
  setwd(dirmain)
  return(DataUseInt)  
}  