#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' 

WorldBankAPIData <- function(LossPercentages){
  # Adds the factors for estimation to the loss factors, these are factors that are included from the literature
  # As well as other factors that are considered proxies for losses. 
  
  # Args:
  #   LossFactorSet: Matrix of Loss Factors that includes ISO3Code, Year, Country, CPC, Description, loss%, ID
  
  # Returns: 
  #   BaseVar
  
  WB1_M <-LossPercentages
  
  #WB1_M <-LossPercentages
  ###### Api based data merge#######
  url <- 'http://api.worldbank.org/countries/'
  setwd(paste(dirmain,'\\variables\\general\\',sep="")) 
  countryWB <- read.csv('WB_UN_CoutryCodes.csv')
  countryWB$Country <- tolower(countryWB$Country) 
  country1 <- merge(WB1_M, countryWB, by.x = c('ISOCode'), by.y = c('ISO.alpha3_Code'), all.x = TRUE, all.y = FALSE, allow.cartesian=TRUE)
  
  DataSource <- '/indicators/'
  WB_indicators_Metadata<- read.csv('WB_indicators_Metadata.csv')
  
  frmt <- '?format=json'
  for (j in 1:length(WB_indicators_Metadata[, 1])){
    WBdf <- data.frame(country = character(), indicator = character(), date = integer(), value = double())  
    factor1 <- 0
    for (i in(2:length(unique(WB1_M$ISOCode)))){
      yr <- unique(WB1_M$Year)
      date_min <- min(yr, na.rm = T)
      date_max <- max(yr, na.rm = T)
      date <- paste('?date = ', date_min, ':', date_max, sep = "")
      # data access on the worldbank api
      if(GET(paste(url, unique(country1$Code)[i], DataSource, WB_indicators_Metadata[j, 1], date, sep = ""),path="/")$status != 200){
        WorldBank1 <- tryCatch(xmlParse(paste(url, unique(country1$Code)[i], DataSource, WB_indicators_Metadata[j, 1], date, sep = "")), error=function(cond) cond = next)
      }else{next}
      WorldBank1 <- xmlToDataFrame(WorldBank1)
      if(length(WorldBank1)<2){
        next
      }
      # lower country names to match easier
      WorldBank1$country <- tolower(WorldBank1$country)
      WBdf <- rbind(WorldBank1, WBdf)
      print(i)
      
    }
    colnames( WBdf)[length( WBdf)-1] <-WB_indicators_Metadata[, 1][j]
    colnames( WBdf)[2] <- "Country"
    colnames( WBdf)[3] <- "Year"
    write.csv(WBdf, paste(dirmain,'\\variables\\byCtryYear\\',WB_indicators_Metadata[, 1][j],'_WB_CI.csv',sep=""),row.names=FALSE)
    
    # WBdf$date <- as.numeric(levels(WBdf$date ))[WBdf$date ]
    # WB1_M <- merge(WB1_M[c('Country','ISOCode','M49Code', 'Year'),], WBdf, by.x = c('Country', 'Year'), by.y = c('country', 'date'), all.x = TRUE, all.y = FALSE)
    # colnames(WB1_M )[length(WB1_M )-1] <- gsub(" ", "", WBdf$indicator[1])
    # print(j)
    # drops = c( 'decimal.x',
    #            'decimal.y',
    #            'decimal',
    #            'indicator',
    #            'indicator.x',
    #            'indicator.y'
    # )
    # WB1_M[,c(names(WB1_M)[names(WB1_M) %in% drops]) := NULL]  
    # 
    # 
  }
  #LossFactor_train_SWSProdOnly <-  WB1_M 
  #LossFactor_Predict <-  WB1_M 
  
  #return(LossFactor_train_SWSProdOnly)
=======
#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' 

WorldBankAPIData <- function(LossPercentages){
  # Adds the factors for estimation to the loss factors, these are factors that are included from the literature
  # As well as other factors that are considered proxies for losses. 
  
  # Args:
  #   LossFactorSet: Matrix of Loss Factors that includes ISO3Code, Year, Country, CPC, Description, loss%, ID
  
  # Returns: 
  #   BaseVar
  
  WB1_M <-LossPercentages
  
  #WB1_M <-LossPercentages
  ###### Api based data merge#######
  url <- 'http://api.worldbank.org/countries/'
  setwd(paste(dirmain,'\\variables\\general\\',sep=""))
  country1  <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  country1 $countryname <- tolower(CountryGroup$countryname)
  
  countryWB <- read.csv('WB_UN_CoutryCodes.csv')
  countryWB$Country <- tolower(countryWB$Country) 
  country1 <- merge(WB1_M, countryWB, by.x = c('ISOCode'), by.y = c('ISO.alpha3_Code'), all.x = TRUE, all.y = FALSE, allow.cartesian=TRUE)
  
  DataSource <- '/indicators/'
  WB_indicators_Metadata<- read.csv('WB_indicators_Metadata.csv')
  
  frmt <- '?format=json'
  for (j in 1:nrow(WB_indicators_Metadata)){
    keepCol = c("country","isocode", "indicator", "date", "value")
    WBdf <- data.frame(country = character(),isocode = character(), indicator = character(), date = integer(), value = double())  
    factor1 <- 0
    for (i in(1:length(unique(CountryGroup$isocode)))){
      yr <- unique(WB1_M$Year)
      date_min <- 1961
      date_max <- 2017
      date <- paste('?date = ', date_min, ':', date_max, sep = "")
      # data access on the worldbank api
      if(GET(paste(url, unique(country1$Code)[i], DataSource,  WB_indicators_Metadata[j, 1], date, sep = ""),path="/")$status != 200){
        WorldBank1 <- tryCatch(xmlParse(paste(url, tolower(unique(CountryGroup$iso2code))[i], DataSource,  WB_indicators_Metadata[j, 1], date, sep = "")), error=function(cond) cond = next)
      }else{next}
      WorldBank1 <- xmlToDataFrame(WorldBank1)
      
      if(length(WorldBank1)<2){
        next
      }
      WorldBank1 <- as.data.table(WorldBank1)
      WorldBank1[,isocode := unique(country1$isocode)[i]]
      # lower country names to match easier
      WorldBank1$country <- tolower(WorldBank1$country)
      WorldBank1<-WorldBank1[,keepCol,with=FALSE]
      WBdf <- rbind(WorldBank1, WBdf)
      print(i)
      
    }
    names(WBdf)[names(WBdf) == "indicator"] <- as.character(WB_indicators_Metadata[j, 1])
    colnames( WBdf)[3] <- "timePointYears"
    WBdf <- merge( WBdf , CountryGroup[,c("geographicAreaM49", "isocode")], by.x = c('isocode'),
                          by.y = c('isocode'), all.x = TRUE, all.y = FALSE)
    write.csv(WBdf, paste(githubsite, 'byCtryYear/',WB_indicators_Metadata[j, 1],'_WB_CI.csv',sep=""),row.names=FALSE)
    
    # WBdf$date <- as.numeric(levels(WBdf$date ))[WBdf$date ]
    # WB1_M <- merge(WB1_M[c('Country','ISOCode','M49Code', 'Year'),], WBdf, by.x = c('Country', 'Year'), by.y = c('country', 'date'), all.x = TRUE, all.y = FALSE)
    # colnames(WB1_M )[length(WB1_M )-1] <- gsub(" ", "", WBdf$indicator[1])
    # print(j)
    # drops = c( 'decimal.x',
    #            'decimal.y',
    #            'decimal',
    #            'indicator',
    #            'indicator.x',
    #            'indicator.y'
    # )
    # WB1_M[,c(names(WB1_M)[names(WB1_M) %in% drops]) := NULL]  
    # 
    # 
  }
  #LossFactor_train_SWSProdOnly <-  WB1_M 
  #LossFactor_Predict <-  WB1_M 
  
  #return(LossFactor_train_SWSProdOnly)

}