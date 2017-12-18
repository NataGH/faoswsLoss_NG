#' Part of the FAO Loss Module
#' 
#' @author Alicia English
options(warn=-1)
FSC_Markov <- function(RawData,opt){
  # Collapses the supply chain into singular observations at the country level
  # Args:
  #   RawData: Matrix of Conversion Factors
  #   opt: Options for aggregating up to the national level for each country/crop/year
  ##      opt = aveatFSP
  ##      This option averages at each supply chain point for each country,crop,year -  delineated by
  ##      ("farm","storage", "processing", "retail","trader", "transport", "wholesale","whole supply chain", "sws_total")
  ##
  ##      From the FLW_LossPercFactors.xls the column FSC_Location is the column that is to delineate the stages. 
  ##      If there are datapoints that cover multiple stages, the function splits it at the ‘/’ and attributes it to the first stage provided. 
  
  # Returns: 
  #   FullSet
  
  # Adjustments to the raw data

  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  RawData$country <- tolower(RawData$country)
  RawData$crop <- tolower(RawData$crop)
  RawData$fsc_location<- tolower(RawData$fsc_location)

  names(RawData) <- tolower(names(RawData))

  FullSet <- subset(RawData,
         select = c(keys_lower, "isocode",  "country",  "crop", "loss_per_clean", "fsc_location")
         )
  
  nro = length(unique(paste(RawData$measureditemcpc,RawData$timepointyears,RawData$geographicaream49, sep=";")))
  FullSet <-   FullSet[1,,] 
  FullSet [,country:='antarctica']
  FullSet <-   FullSet[,loss_per_clean :=0,]
  FullSeta <-   FullSet[1,,] 
  TransitionMatrix = matrix(0,1,9)
  
  # Sets the stage sequence for the markov chain
  colnames(TransitionMatrix) = c("farm","transport","storage", "trader","wholesale", "processing", "retail", "wholesupplychain", "sws_total")
  count = 1
  for( i in 1:length(unique(RawData$geographicaream49))){
    # this first level selects the data for a specific country
    
    data2 <- RawData %>% filter(geographicaream49 == unique(RawData$geographicaream49)[i])
    data2$fsc_location1 = sapply(strsplit(data2$fsc_location,"/"), '[', 1)
    for( ii in  1:length(unique(data2$measureditemcpc))){
      # this first level selects the data for a specific crop
      data3 <- data2 %>% filter(measureditemcpc == unique(data2$measureditemcpc)[ii])
      for( iii in 1:length(unique(data3$timepointyears))){
        # this first level selects the data for a specific Year
        data4 <- data3 %>% filter(timepointyears == unique(data3$timepointyears)[iii])
        data4 <- data4[!(is.na(data4$loss_per_clean)),] 
        if(nrow(data4)==0){next}
        if(dim(data4)[1]> 1){
          if(opt == "aveatFSP"){
            # Averages at each location in the FSC, excluding the whole chain and the SWS 
            for(iiii in 1:9){
              TransitionMatrix[iiii] = sum(data4$loss_per_clean*(data4$fsc_location1 == colnames(TransitionMatrix)[iiii]))/(sum(data4$fsc_location1 == colnames(TransitionMatrix)[iiii]))
              TransitionMatrix[is.nan(TransitionMatrix)] = 0
            }
            
            ref <- 1000 # reference amount
            ref0 <- ref 
            amt <- 0 
            WS <- TransitionMatrix[,8] # Whole supply chain
            SWS <- TransitionMatrix[,9] # Estimate from the SWS data 
            FSC <- TransitionMatrix[,1:7][TransitionMatrix[,1:7] > 0] #aggregates of the food supply chain
            for(ni in 1:length(FSC)){
              # Uses the referent quantity to aggregate losses alog the supply chain
              ref <- ref- ref*(FSC[ni]/100)
              amt <- ref*(FSC[ni]/100) + amt
            }
            # Takes the average for the compounded losses over the FSC and averages with the Whole supply chain and the SWS
            est = c(amt/ref0, WS/100, SWS/100 )
            lossPer = mean(est[!est==0])*100

            FullSeta[1, geographicaream49 := data4$geographicaream49,]
            FullSeta[1, timepointyears := data4$timepointyears,]
            FullSeta[1, measureditemcpc := data4$measureditemcpc,]
            FullSeta[1, isocode := data4$isocode,]
            FullSeta[1, country := data4$country,]
            FullSeta[1, crop := data4$crop,]
            FullSeta[1, 'loss_per_clean':=lossPer]
            FullSeta[1, 'fsc_location'] <- "Calc"
          }} else{
            # If there is only one estimate for the country/commodity/year the estimate just gets added to the dataset   
            FullSeta[1, geographicaream49 := data4$geographicaream49,]
            FullSeta[1, timepointyears := data4$timepointyears,]
            FullSeta[1, measureditemcpc := data4$measureditemcpc,]
            FullSeta[1, isocode := data4$isocode,]
            FullSeta[1, country := data4$country,]
            FullSeta[1, crop := data4$crop,]
            FullSeta[1, loss_per_clean:=data4$loss_per_clean,]
            FullSeta[1, fsc_location := data4$fsc_location,]

          }
        
        #  if(opt == "model"){
        #Model here
        #  }
        count = count +1
        FullSet <- rbind(FullSet,FullSeta)
      }
      }}
  FullSet <- FullSet %>% filter(loss_per_clean != 0)
  FullSet <- FullSet %>% filter(country!='antarctica')
  FullSet$measureditemcpc <- addHeadingsCPC(FullSet$measureditemcpc) 
  names(FullSet) <- tolower(names(FullSet))
  return(FullSet)
}  
