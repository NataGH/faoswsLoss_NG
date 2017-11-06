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
  RawData <- as.data.table(RawData)
  RawData$Country <- tolower(RawData$country)
  RawData$crop <- tolower(RawData$crop)
  RawData$fsc_location<- tolower(RawData$fsc_location)
  RawData$crop <- trim(RawData$crop)

  RawData$IDNum <- 0 
  data1 <- as.data.frame(RawData)
  
  FullSet = data1[0,c("geographicAreaM49", "isocode", "year", "country", "measureditemcpc", "crop", "loss_per_clean", "fsc_location")]
  FullSeta =data1[0,c("geographicAreaM49", "isocode", "year", "country", "measureditemcpc", "crop", "loss_per_clean", "fsc_location")]
  TransitionMatrix = matrix(0,1,9)
  
  # Sets the stage sequence for the markov chain
  colnames(TransitionMatrix) = c("farm","transport","storage", "trader","wholesale", "processing", "retail", "wholesupplychain", "sws_total")
  
  for( n in 1:length(unique(data1$isocode))){
    # this first level selects the data for a specific country
    
    data2 <- data1 %>% filter(isocode == unique(data1$isocode)[n])
    data2$FSC_Location1 = sapply(strsplit(data2$fsc_location,"/"), '[', 1)
    for( nn in  1:length(unique(data2$crop))){
      # this first level selects the data for a specific crop
      data3 <- data2 %>% filter(crop == unique(data2$crop)[nn])
      for( nnn in 1:length(unique(data3$year))){
        # this first level selects the data for a specific Year
        data4 <- data3 %>% filter(year == unique(data3$year)[nnn])
        data4 <- data4[!(is.na(data4$loss_per_clean)),] 
        if(dim(data4)[1]> 1){
          if(opt == "aveatFSP"){
            # Averages at each location in the FSC, excluding the whole chain and the SWS 
            for(nnnn in 1:9){
              TransitionMatrix[nnnn] = sum(data4$loss_per_clean*(data4$FSC_Location1 == colnames(TransitionMatrix)[nnnn]))/(sum(data4$FSC_Location1 == colnames(TransitionMatrix)[nnnn]))
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
            
            FullSeta <- data4[1,c('geographicAreaM49', 'isocode', 'year', 'Country', 'measureditemcpc', 'crop', 'loss_per_clean', 'fsc_location')]
            FullSeta[, 'loss_per_clean'] <-lossPer
            FullSeta[, 'fsc_location'] <- "Calc"
            FullSet = rbind(FullSet, FullSeta)
          }} else{
            # If there is only one estimate for the country/commodity/year the estimate just gets added to the dataset   
            FullSet = rbind(FullSet, data4[,c('geographicAreaM49', 'isocode', 'year', 'Country', 'measureditemcpc', 'crop', 'loss_per_clean', 'fsc_location')])
          }
        #  if(opt == "model"){
        #Model here
        #  }
        
      }}}
  FullSet$measureditemcpc <- addHeadingsCPC(FullSet$measureditemcpc) 
  return(FullSet)
}  
