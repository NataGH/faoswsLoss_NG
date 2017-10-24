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
  indicators <- read.csv('WB_indicators_Metadata.csv')
  
  frmt <- '?format=json'
  for (j in 1:length(indicators[, 1])){
    WBdf <- data.frame(country = character(), indicator = character(), date = integer(), value = double())  
    factor1 <- 0
    for (i in(2:length(unique(WB1_M$ISOCode)))){
      yr <- unique(WB1_M$Year)
      date_min <- min(yr, na.rm = T)
      date_max <- max(yr, na.rm = T)
      date <- paste('?date = ', date_min, ':', date_max, sep = "")
      # data access on the worldbank api
      if(GET(paste(url, unique(country1$Code)[i], DataSource, indicators[j, 1], date, sep = ""),path="/")$status != 200){
        WorldBank1 <- tryCatch(xmlParse(paste(url, unique(country1$Code)[i], DataSource, indicators[j, 1], date, sep = "")), error=function(cond) cond = next)
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
    colnames( WBdf)[length( WBdf)-1] <-indicators[, 1][j]
    colnames( WBdf)[2] <- "Country"
    colnames( WBdf)[3] <- "Year"
    write.csv(WBdf, paste(dirmain,'\\variables\\byCtryYear\\',indicators[, 1][j],'_WB_CI.csv',sep=""),row.names=FALSE)
    
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