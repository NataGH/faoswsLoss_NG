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
  library(jsonlite)
  library(httr)
  library(XML)
  library(dplyr)
  library(data.table)
  library(faosws)
  library(faoswsUtil)
  library(faoswsLoss)
  
  maxYear <- format(Sys.Date(), "%Y")
  selectedYear =  as.character(1990:maxYear)
  if(CheckDebug()){
    message("Not on server, so setting up environment...")
    USER <- if_else(.Platform$OS.type == "unix",
                    Sys.getenv('USER'),
                    Sys.getenv('USERNAME'))
    
    
    library(faoswsModules)
    settings <- ReadSettings(file = file.path(paste(getwd(),"sws.yml", sep='/')))
    SetClientFiles(settings[["certdir"]])
    
    
    GetTestEnvironment(
      baseUrl = settings[["server"]],
      token = settings[["token"]]
    )
    
    
    
  }else if(CheckDebug() & LocalRun){
    #Load local last dataset
    load("InputData.RData")
    
    # CountryGroup <- as.data.table(read.csv(paste(githubsite, 'General/a2017regionalgroupings_SDG_02Feb2017.csv', sep='')))
    # FAOCrops <- as.data.table(read.csv(paste(githubsite, 'General/Cpc.csv', sep=''))) ## All Crops in the CPC system
    # ConvFactor1 <- as.data.table(read.csv(paste(githubsite, 'General/FLW_LossPercFactors.csv', sep='')))
    # names(CountryGroup) <- tolower(names(CountryGroup))
    # names(FAOCrops) <- tolower(names(FAOCrops))
    # names(ConvFactor1) <- tolower(names(ConvFactor1))
    # ConvFactor1[,loss_per_clean := as.numeric(levels(loss_per_clean))[loss_per_clean]]
    
    
  }else{
    # Remove domain from username
    USER <- regmatches(
      swsContext.username,
      regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
    )
    
    options(error = function(){
      dump.frames()
      
      filename <- file.path(Sys.getenv("R_SWS_SHARE_PATH"), USER, "PPR")
      
      dir.create(filename, showWarnings = FALSE, recursive = TRUE)
      
      save(last.dump, file = file.path(filename, "last.dump.RData"))
    })
  } 
  
  #WB1_M <-LossPercentages
  ###### Api based data merge#######
  url <- 'http://api.worldbank.org/countries/'
  setwd(paste(dirmain,'\\data',sep="")) 
  countryWB  <- as.data.table(read_csv("~/faoswsLossa/data/WB_UN_CoutryCodes.csv"))
  countryWB$Country <- tolower(countryWB$Country) 
  countryWB$M49_Code <- as.character(countryWB$M49_Code)
  WB1_M <- ReadDatatable("aggregate_loss_table")
  country1 <- merge(WB1_M, countryWB, by.x = c('geographicaream49'), by.y = c('M49_Code'), all.x = TRUE, all.y = FALSE, allow.cartesian=TRUE)
  
  DataSource <- '/indicators/'
  WB_indicators_Metadata<- as.data.table(read_csv("~/faoswsLossa/data/WB_indicators_Metadata.csv"))
  
  frmt <- '?format=json'
  # 42, 43,49,50,64
  for (j in 0:nrow(WB_indicators_Metadata[,"Code",with=F])){
    WBdf <- data.frame(isocode = character(),country = character(), indicator = character(), date = integer(), value = double(), geographicaream49 = character())  
    factor1 <- 0
    for (i in(1:length(unique(WB1_M$geographicaream49)))){
      yr <- unique(selectedYear)
      date_min <- min(yr, na.rm = T)
      date_max <- max(yr, na.rm = T)
      date <- paste('?date=', date_min, ':', date_max, sep = "")
      # data access on the worldbank api
      if(GET(paste(url, unique(country1$Code)[i], DataSource, WB_indicators_Metadata[j, "Code", with=F], date, sep = ""),path="/")$status != 200){
        WorldBank1 <- tryCatch(xmlParse(paste(url, countryWB$Code[i], DataSource, WB_indicators_Metadata[j, "Code", with=F], date, sep = "")), error=function(cond) cond = next)
      }else{next}
      WorldBank1 <- xmlToDataFrame(WorldBank1)
      if(length(WorldBank1)<2){
        next
      }
      # lower country names to match easier
      WorldBank1$country <- tolower(WorldBank1$country)
      if(WorldBank1$country[1] != "namibia"){
         WorldBank1$geographicaream49 <- unlist(countryWB[Code == countryWB$Code[i],"M49_Code", with =F])
         WorldBank1$isocode <- unlist(countryWB[Code == countryWB$Code[i],"ISO-alpha3_Code", with =F])
      }else{
        WorldBank1$geographicaream49 <- "516"
        WorldBank1$isocode <- "NAM"
      }
      WBdf <- rbind(WorldBank1, WBdf)
      print(i)
      
    }
    WBdf <- as.data.table(WBdf) 
    WBdf <- unique(WBdf)
    setnames(WBdf, old= c("indicator" ,"country" ,"date", "value" , "geographicaream49","isocode"),
             new=c("description","country", "timepointyears",gsub("[.]+", "_", tolower(WB_indicators_Metadata[j, "Code", with=F])), "geographicaream49","isocode"))
    
    #write.csv(WBdf, paste(dirmain,'\\data\\',WB_indicators_Metadata[, 1][j],'_WB_CI.csv',sep=""),row.names=FALSE)
    indc <- gsub("[.]+", "_",WB_indicators_Metadata[j, "Code", with=F])
    table =  tolower(indc)
    changeset <- Changeset(table)
    newdat <- ReadDatatable(table, readOnly = FALSE)
    AddDeletions(changeset, newdat)
    Finalise(changeset)
    ## Add
    keepN <- names(newdat)[!names(newdat) %in% c("__id","__ts")]
    AddInsertions(changeset, WBdf[,keepN,with=F] )
    Finalise(changeset)
    
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
  library(httr)
  library(jsonlite)
  library(XML)
  library(dplyr)
  
  WB1_M <-LossPercentages
  
  #WB1_M <-LossPercentages
  ###### Api based data merge#######
  url <- 'http://api.worldbank.org/countries/'
  setwd(paste(dirmain,'\\data\\',sep=""))
  CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  CountryGroup$countryname <- tolower(CountryGroup$countryname)
  CountryGroup[isocode == "PRK", "countryname"] <-"Democratic Peoples Republic of Korea"
  CountryGroup[isocode == "LAO", "countryname"] <-"Lao Peoples Democratic Republic"
  CountryGroup[,"geographicaream49":=CountryGroup$m49code]
  
  countryWB <- read.csv('WB_UN_CoutryCodes.csv')
  countryWB$Country <- tolower(countryWB$Country) 
  country1 <- merge(WB1_M, countryWB, by.x = c('geographicaream49'), by.y = c('M49_Code'), all.x = TRUE, all.y = FALSE, allow.cartesian=TRUE)
  
  DataSource <- '/indicators/'
  WB_indicators_Metadata<- read.csv('WB_indicators_Metadata.csv')
  
  frmt <- '?format=json'
  for (j in 1:nrow(WB_indicators_Metadata)){
    keepCol = c("isocode", "country","geographicaream49", "indicator", "date", "value")
    WBdf <- data.frame(country = character(),isocode = character(), geographicaream49 = character(), indicator = character(), date = integer(), value = double())  
    factor1 <- 0
    for (i in(1:length(unique(CountryGroup$geographicaream49)))){
      yr <- unique(WB1_M$Year)
      date_min <- 1970
      date_max <- 2017
      date <- paste('?date=', date_min, ':', date_max, sep = "")
      # data access on the worldbank api
      if(GET(paste(url, unique(CountryGroup$iso2code)[i], DataSource,  WB_indicators_Metadata[j, 1], date, sep = ""),path="/")$status != 200){
        WorldBank1 <- tryCatch(xmlParse(paste(url, tolower(unique(CountryGroup$iso2code))[i], DataSource,  WB_indicators_Metadata[j, 1], date, sep = "")), error=function(cond) cond = next)
      }else{next}
      WorldBank1 <- xmlToDataFrame(WorldBank1)
      
      if(length(WorldBank1)<2){
        next
      }
      WorldBank1 <- as.data.table(WorldBank1)
      WorldBank1[,isocode := unlist(CountryGroup[iso2code == unique(CountryGroup$iso2code)[i],"isocode"])]
      WorldBank1[,geographicaream49 := unlist(CountryGroup[iso2code == unique(CountryGroup$iso2code)[i],"geographicaream49"])]
      # lower country names to match easier
      WorldBank1$country <- tolower(gsub("[[:punct:]]"," ",WorldBank1$country))
      WorldBank1<-WorldBank1[,keepCol,with=FALSE]
      WBdf <- rbind(WorldBank1, WBdf)
      print(i)
      
    }
   
    names(WBdf)[names( WBdf) =='date'] <- "timepointyears"
    names(WBdf)[names( WBdf) =='indicator'] <- "description"
    setcolorder(WBdf, 
                c("isocode", "country", "description", "timepointyears", "value","geographicaream49") )
    names(WBdf)[names(WBdf) == "value"] <- tolower(gsub("[[:punct:]]","_",as.character(WB_indicators_Metadata[j, 1]))) 
    WBdf <- WBdf %>% filter(!is.na(geographicaream49))
    
    write.csv(WBdf, paste(paste(dirmain,'\\data\\',sep=""),WB_indicators_Metadata[j, 1],'_WB_CI.csv',sep=""),row.names=FALSE)
    
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
  