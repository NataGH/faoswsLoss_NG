#' Get Official Food Loss Data
#'
#' Function to obtain official food loss data at primary level
#' @param protected Logical only return observations with protected flag combination 
#' @import faoswsFlag faosws
#' @export getNutritionData

getNutritionData = function(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE){
  
  ## define measured elements
  NutrientKey = DatasetKey(domain = "suafbs", dataset = "nutrient_factors",
      dimensions = list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                        keys = GetCodeList("suafbs", "nutrient_factors","geographicAreaM49" )[,code]),
          measuredItemCPC = Dimension(name = "measuredItemCPC", 
                                      keys = GetCodeList("suafbs", "nutrient_factors","measuredItemCPC" )[,code]),
          measuredElement = Dimension(name = "measuredElement", 
                                      keys = c("1001", "1003","1005")),
          timePointYearsSP = Dimension(name = "timePointYearsSP", 
                                       keys = GetCodeList("suafbs", "nutrient_factors","timePointYearsSP" )[,code] )
     ))
  

  
  ## Pivot to vectorize yield computation
 
  NutrientQuery = GetData(
    NutrientKey)
  

  ## Convert time to numeric
  NutrientQuery[, timePointYearsSP := as.numeric(timePointYearsSP )]
  
  ## lossQuery[, geographicAreaFS := as.numeric(geographicAreaFS)]
  NutrientQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
  
  
  ## Reading FlagValidTable specific for loss
  #flagValidTableLoss <- read_csv("~/faoswsLoss/data-raw/flagValidTable.csv")
  flagValidTableLoss <- as.data.table(flagValidTable)
  
 
  if (protected) {
    protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
      .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
    
    col_keep <- names(NutrientQuery) %>%
      .[.!="flagCombination"]
    
    ## subset to protected flags
    ## requires dtplyr, the data table back-end for 'dplyr'
    NutrientQuery <-
      NutrientQuery[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")] %>%
      merge(., protectedFlag, by = "flagCombination") %>%
      filter(Protected == TRUE) 
  }
  NutrientQuery
}
