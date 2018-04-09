#' Get Official Food Loss Data
#'
#' Function to obtain official food loss data at primary level
#' @param protected Logical only return observations with protected flag combination 
#' @import faoswsFlag faosws
#' @export getLossData

getLossData_LossDomain = function(areaVar,itemVar,yearVar,elementVar,selectedYear,KeyEle, protected = FALSE){
  #install.packages("faoswsFlags")

  ## define measured elements
  lossKey = DatasetKey(
    domain = "lossWaste",
    dataset = "loss",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domain = "lossWaste",
                                   dataset = "loss",
                                   dimension = "geographicAreaM49")[type == "country", code]),
      Dimension(name = "measuredElementSuaFbs", keys = KeyEle), 
      Dimension(name = "timePointYears", keys = as.character(1990:max(selectedYear))),
      Dimension(name = "measuredItemSuaFbs",
                keys = GetCodeList(domain = "lossWaste",
                                   dataset = "loss",
                                   dimension = "measuredItemSuaFbs")[, code]))
  )
  
  lossQuery = GetData(
    lossKey,
    flags = T)
  

  
  ## Pivot to vectorize yield computation
  # lossPivot = c(
  #   ## Pivoting(code = areaVarFS, ascending = TRUE),
  #   ## Pivoting(code = itemVarFS, ascending = TRUE),
  #   ## Pivoting(code = yearVar, ascending = FALSE),
  #   ## Pivoting(code = elementVarFS, ascending = TRUE)
  #   Pivoting(code = areaVar, ascending = TRUE),
  #   Pivoting(code = itemVar, ascending = TRUE),
  #   Pivoting(code = yearVar, ascending = FALSE),
  #   Pivoting(code = elementVar, ascending = TRUE)
  # )
  
  ## Query the data
  # lossQuery = GetData(
  #   key = lossKey,
  #   flags = TRUE,
  #   normalized = FALSE,
  #   pivoting = lossPivot
  # )
  # 
  ## setnames(lossQuery,
  ##          old = names(lossQuery),
  ##          new = c("geographicAreaFS","measuredItemFCL","timePointYears",
  ##                  "Value_measuredElement_5120","flagFaostat_measuredElementFS_5120")
  ##          )
  
  
  ## ## Convert geographicAreaM49 to geographicAreaFS
  ## lossQuery[, geographicAreaM49 := as.numeric(faoswsUtil::fs2m49(as.character(geographicAreaFS)))]
  
  ## ## Convert measuredItemCPC to measuredItemFCL
  ## lossQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  ## lossQuery[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
  
  ## Convert time to numeric
  lossQuery[, timePointYears := as.numeric(timePointYears)]
  
  ## lossQuery[, geographicAreaFS := as.numeric(geographicAreaFS)]
  lossQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
  
  
  ## Reading FlagValidTable specific for loss
  #flagValidTableLoss <- read_csv("~/faoswsLoss/data-raw/flagValidTable.csv")
  flagValidTableLoss <- as.data.table(flagValidTable)
  
  
  ## Taking only official data
  ## distinct(lossQuery,flagFaostat_measuredElementFS_5120)
  ## lossQuery = lossQuery[flagFaostat_measuredElementFS_5120 == "", ]
  
  if (protected) {
    protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
      .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
    
    col_keep <- names(lossQuery) %>%
      .[.!="flagCombination"]
    
    ## subset to protected flags
    ## requires dtplyr, the data table back-end for 'dplyr'
    lossQuery <-
      lossQuery[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")] %>%
      merge(., protectedFlag, by = "flagCombination") %>%
      filter(Protected == TRUE) 
  }
  
  lossQuery
  
}
