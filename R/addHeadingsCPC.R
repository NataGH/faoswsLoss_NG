#' Part of the FAO Loss Module
#' 
#' @author Alicia English

addHeadingsCPC <- function(measuredItemCPC){ 
  # Adds the appropriate number of zeros to the CPC code  
  # Args:
  #   measuredItemCPC: The CPC codes that dont have leading zeros 
  # Returns: 
  #   measuredItemCPC
  measuredItemCPC <- as.character(measuredItemCPC)
  ifelse(nchar(measuredItemCPC) == 2, 
         paste0("00",measuredItemCPC, sep = ""),
         ifelse(substring(measuredItemCPC, 1, 1) ==0,measuredItemCPC,
                ifelse((nchar(measuredItemCPC) == 3 |nchar(measuredItemCPC) == 4),
                       paste0("0",measuredItemCPC, sep = ""),
                       measuredItemCPC)))
}