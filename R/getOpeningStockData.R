#' Get Opening Stock Data
#'
#' Function to obtain opening stocks data calculated by the faoswsStock module.
#'
#' @export getOpeningStockData

getOpeningStockData = function(){
  

  ## define measured elements
  openingStockKey <- getCompleteImputationKey(table = "production")
  openingStockKey@dimensions[["measuredElement"]]@keys <- "5113"
  
  ## Pivot to vectorize yield computation
  openingStockPivot = c(
    ## Pivoting(code = areaVarFS, ascending = TRUE),
    ## Pivoting(code = itemVarFS, ascending = TRUE),
    ## Pivoting(code = yearVar, ascending = FALSE),
    ## Pivoting(code = elementVarFS, ascending = TRUE)
    Pivoting(code = areaVar, ascending = TRUE),
    Pivoting(code = itemVar, ascending = TRUE),
    Pivoting(code = yearVar, ascending = FALSE),
    Pivoting(code = elementVar, ascending = TRUE)
  )

  ## Query the data
  openingStockQuery = GetData(
    key = openingStockKey,
    flags = TRUE,
    normalized = FALSE,
    pivoting = openingStockPivot
  )
  
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
  openingStockQuery[, timePointYears := as.numeric(timePointYears)]

  openingStockQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]


  ## Taking only official data
  ## distinct(lossQuery,flagFaostat_measuredElementFS_5120)
  ## lossQuery = lossQuery[flagFaostat_measuredElementFS_5120 == "", ]

  ## if (protected) {
  ##   protectedFlag <- flagValidTable[flagValidTable$Protected == TRUE,] %>%
  ##     .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]

  ##   col_keep <- names(openingStockQuery) %>%
  ##     .[.!="flagCombination"]

  ##   ## subset to protected flags
  ##   openingStockQuery <-
  ##     openingStockQuery[, flagCombination := paste(flagObservationStatus_measuredElement_5113, flagMethod_measuredElement_5113, sep = ";")] %>%
  ##     merge(., protectedFlag, by = "flagCombination") %>%
  ##     filter(Protected == TRUE) %>%       # only keep protected values
  ##     select_(.dots = col_keep)
  ## }

  openingStockQuery

}
