#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' 

VariablesAdd2 <- function(DataUseInt,keys){  
  #setwd(paste(githubsite, 'General/',sep="")) 
  
  #######################################
  ####Import by .Rdata ####
  ########### FAO In-House Data  ################### 
 
  #Adds the regional groupings and 3 years of  time lag
  #CountryGroup <- as.data.table(read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep='')))
  CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  CountryGroup$CountryName <- tolower(CountryGroup$CountryName)
  names(CountryGroup)[names(  CountryGroup) == 'm49code'] <- "geographicAreaM49"

  DataUseInt <- merge(DataUseInt , CountryGroup[, c('isocode', 'sdg_regions')], by.x = c('isocode'),
                      by.y = c('isocode'), all.x = TRUE, all.y = FALSE)
  DataUseInt[,Year:= timePointYears]
  DataUseInt[,lag1yr:= timePointYears - 1]
  DataUseInt[,lag2yr:= timePointYears - 2]
  DataUseInt[,lag3yr:= timePointYears - 3]

  
  ######## Weather Data  ################ 
  ConvFactor_cal <- unique(DataUseInt[, c("geographicAreaM49", "timePointYears")])
  ConvFactor_cal2 <- unique(DataUseInt[,keys ,with=FALSE])

  #Temperature <-  as.data.table(read.csv(paste(githubsite, 'General/Temp_climate.csv', sep='')))
  Temperature <-  ReadDatatable("temp_climate_month_ctry")
  names(Temperature)[names(Temperature) == 'year'] <- "timePointYears"
  Temperature <- merge( Temperature , CountryGroup[,c("geographicAreaM49", "isocode")], by.x = c('isocode'),
                      by.y = c('isocode'), all.x = TRUE, all.y = FALSE)
  Temperature <-Temperature %>% filter(geographicAreaM49 %in%  ConvFactor_cal$geographicAreaM49  & timePointYears %in%  ConvFactor_cal$timePointYears)
  
   
  #Precipitation <- as.data.table(read.csv(paste(githubsite, 'General/Rain_climate.csv', sep='')))
  Precipitation <- ReadDatatable("rain_climate_month_ctry")
  names(Precipitation)[names(Precipitation) == 'year'] <- "timePointYears"
  Precipitation <- merge(Precipitation, CountryGroup[,c("geographicAreaM49", "isocode")], by.x = c('isocode'),
                        by.y = c('isocode'), all.x = TRUE, all.y = FALSE)
  Precipitation <-Precipitation %>% filter(geographicAreaM49 %in%  ConvFactor_cal$geographicAreaM49  & timePointYears %in%  ConvFactor_cal$timePointYears)
  
  #CropCalendar <- as.data.table(read.csv(paste(githubsite, 'General/AllCropCalendar.csv', sep='')))
  CropCalendar <- ReadDatatable("crop_calendar_nov17")
  CropCalendar <- merge(CropCalendar, CountryGroup[,c("geographicAreaM49", "isocode")], by.x = c('isocode'),
                         by.y = c('isocode'), all.x = TRUE, all.y = FALSE)
  
  CropCalendar <- CropCalendar[, c("geographicAreaM49","measureditemcpc","crop", "harvesting_month_onset", "harvesting_month_end")]
  CropCalendar$measuredItemCPC <- addHeadingsCPC(CropCalendar$measureditemcpc) 
  CropCalendar$crop <- tolower(CropCalendar$crop)
  
  AggCropCalendar <- aggregate(harvesting_month_end ~ geographicAreaM49+measuredItemCPC, data = CropCalendar, min)
  AggCropCalendar <-join(ConvFactor_cal2,AggCropCalendar, by= c('geographicAreaM49','measuredItemCPC'),type= 'left', match='all')
  names(AggCropCalendar)[names(AggCropCalendar) == 'harvesting_month_end'] <- 'month'
  
 
  Temperature2 <- join(AggCropCalendar, Temperature, by= c('geographicAreaM49','timePointYears', 'month'),type= 'left', match='all')
  Precipitation2 <- join(AggCropCalendar, Precipitation , by= c('geographicAreaM49','timePointYears', 'month'),type= 'left', match='all')
  
  DataUseInt <- join(DataUseInt, Temperature2, by = keys,type= 'left', match='all')  ### FInal Data set @@
  DataUseInt <- join(DataUseInt, Precipitation2,by = keys,type= 'left', match='all')  ### FInal Data set @@
  
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
