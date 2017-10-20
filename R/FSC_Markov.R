#' Part of the FAO Loss Module
#' 
#' @author Alicia English

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
  RawData$Country <- tolower(RawData$Country)
  RawData$Crop <- tolower(RawData$Crop)
  RawData$FSC_Location <- tolower(RawData$FSC_Location)
  RawData$Crop <- trim(RawData$Crop)
  RawData$Loss_Per_clean <- as.numeric(levels(RawData$Loss_Per_clean)[RawData$Loss_Per_clean])
  
  RawData$IDNum <- 0 
  data1 <- as.data.frame(RawData)
  
  FullSet =data1[0,c('ISOCode', 'Year', 'Country', 'measuredItemCPC', 'Crop', 'Loss_Per_clean', 'FSC_Location')]
  FullSeta =data1[0,c('ISOCode', 'Year', 'Country', 'measuredItemCPC', 'Crop', 'Loss_Per_clean', 'FSC_Location')]
  TransitionMatrix = matrix(0,1,9)
  
  # Sets the stage sequence for the markov chain
  colnames(TransitionMatrix) = c("farm","transport","storage", "trader","wholesale", "processing", "retail", "wholesupplychain", "sws_total")
  
  for( n in 1:length(unique(data1$ISOCode))){
    # this first level selects the data for a specific country
    data2 <- data1[(data1$ISOCode == unique(data1$ISOCode)[n]),]
    data2$FSC_Location1 = sapply(strsplit(data2$FSC_Location,"/"), '[', 1)
    for( nn in  1:length(unique(data2$Crop))){
      # this first level selects the data for a specific crop
      data3 <- data2[ (data2$Crop == unique(data2$Crop)[nn]),]
      for( nnn in 1:length(unique(data3$Year))){
        # this first level selects the data for a specific Year
        data4 <- data3[data3$Year == unique(data3$Year)[nnn], ]
        data4 <- data4[!(is.na(data4$Loss_Per_clean)),] 
        if(dim(data4)[1]> 1){
          if(opt == "aveatFSP"){
            # Averages at each location in the FSC, excluding the whole chain and the SWS 
            for(nnnn in 1:9){
              TransitionMatrix[nnnn] = sum(data4$Loss_Per_clean*(data4$FSC_Location1 == colnames(TransitionMatrix)[nnnn]))/(sum(data4$FSC_Location1 == colnames(TransitionMatrix)[nnnn]))
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
            
            FullSeta <- data4[1,c('ISOCode', 'Year', 'Country', 'measuredItemCPC', 'Crop', 'Loss_Per_clean', 'FSC_Location')]
            FullSeta[, "Loss_Per_clean"] <-lossPer
            FullSeta[, "FSC_Location"] <- "Calc"
            FullSet = rbind(FullSet, FullSeta)
          }} else{
            # If there is only one estimate for the country/commodity/year the estimate just gets added to the dataset   
            FullSet = rbind(FullSet, data4[,c('ISOCode', 'Year', 'Country', 'measuredItemCPC', 'Crop', 'Loss_Per_clean', 'FSC_Location')])
          }
        #  if(opt == "model"){
        #Model here
        #  }
        
      }}}
  FullSet$measuredItemCPC <- addHeadingsCPC(FullSet$measuredItemCPC ) 
  FullSet$ID = paste(FullSet$ISOCode,FullSet$measuredItemCPC,FullSet$Year,sep = ";")
  return(FullSet)
}  
