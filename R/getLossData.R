#' Get Official Food Loss Data
#'
#' Function to obtain official food loss data at primary level
#' @param protected Logical only return observations with protected flag combination 
#' @import faoswsFlag faosws
#' @export getLossData

getLossData = function(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE){
  #install.packages("faoswsFlags")
  ## ################################################################################################
  ##   Code to get Import data from the Old System 
  ## ################################################################################################
  ##  
  ##   measuredItemFS = ReadDatatable(table = "loss_food_group") %>%
  ##     select(measured_item_fs,food_general_group,measured_item_cpc) %>%
  ##     filter(food_general_group == "primary")
  ##   
  ##   
  ##   lossQuery = data.table(sws_query(area = 1:299, 
  ##                                    item = as.character(measuredItemFS$measured_item_fs), 
  ##                                    ele = 121, 
  ##                                    year = 1961:2015, 
  ##                                    value.names = F
  ##   ))
  ##   
  ##   
  ##   setnames(lossQuery, 
  ##            old = c(names(lossQuery)),
  ##            new = c("geographicAreaFS","measuredItemFCL","measuredElement","timePointYears",
  ##                    "Value_measuredElement_5120","flagObservationStatus_measuredElement_5120")
  ##   )
  ##   
  ##   lossQuery = 
  ##     lossQuery%>%
  ##     select(geographicAreaFS,measuredItemFCL,timePointYears,
  ##            Value_measuredElement_5120,flagObservationStatus_measuredElement_5120)
  ##   
  ##   
  ##   ### Convert measuredItemFCL to measuredItemCPC
  ##   #   lossQuery[, measuredItemCPC := faoswsUtil::cpc2fcl(as.character(measuredItemFCL))]
  ##   
  ##   ## Convert geographicAreaFS to geographicAreaM49
  ##   #   lossQuery[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
  ##   
  ##   
  ##   ## Convert time to numeric
  ##   lossQuery[, timePointYears := as.numeric(timePointYears)]
  ##   
  ##   ## Taking only official data
  ##   lossQuery = lossQuery[flagObservationStatus_measuredElement_5120 == " ", ]
  ##   
  ##   ## Adding headings to FCL codes
  ##   lossQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  ##   
  ## #################################################################################################  
  ##
  ##
  ## ################################################################################################
  ##   Code to get Import data from the New System 
  ## ################################################################################################

  ## allCountries =
  ##   GetCodeList(domain = "agriculture",
  ##               dataset = "aproduction",
  ##               dimension = "geographicAreaM49")[type == "country", code]
  
  ## ## Convert geographicAreaM49 to geographicAreaFS  
  ## allCountriesFS = faoswsUtil::m492fs(as.character(allCountries))
  ## ## Remove "NULL"s  beacuse they are invalide codes for dimension geographicAreaFS
  ## allCountriesFS = na.omit(allCountriesFS) 
  ## ## Remove codes "274" "283" "280" "281" "279"
  ## ## because They are invalide codes for dimension geographicAreaFS
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "274")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "283")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "280")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "281")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "279")]
  
  ## lossKey = DatasetKey(
  ## domain = "faostat_one",
  ## dataset = "FS1_SUA",
  ## dimensions = list(
  ##     Dimension(name = areaVarFS,
  ##           keys = allCountriesFS),
  ##     Dimension(name = elementVarFS,
  ##           keys = "121"),
  ##     Dimension(name = itemVarFS,
  ##           keys = as.character(as.numeric(requiredItems$measuredItemFCL))),
  ##     Dimension(name = yearVar,
  ##           keys = selectedYear)
  ##    )
  ## )
  
  ## define measured elements
  lossKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domain = "agriculture",
                                   dataset = "aproduction",
                                   dimension = "geographicAreaM49")[type == "country", code]),
      Dimension(name = "measuredElement", keys = c("5016")), 
      Dimension(name = "timePointYears", keys = as.character(2016:2016)),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domain = "agriculture",
                                   dataset = "aproduction",
                                   dimension = "measuredItemCPC")[, code]))
  )
  
  ## Pivot to vectorize yield computation
  lossPivot = c(
    ## Pivoting(code = areaVarFS, ascending = TRUE),
    ## Pivoting(code = itemVarFS, ascending = TRUE),
    ## Pivoting(code = yearVar, ascending = FALSE),
    ## Pivoting(code = elementVarFS, ascending = TRUE)
    Pivoting(code = areaVar, ascending = TRUE),
    Pivoting(code = itemVar, ascending = TRUE),
    Pivoting(code = yearVar, ascending = FALSE),
    Pivoting(code = elementVar, ascending = TRUE)
  )
  lossQuery = GetData(
    lossKey,
    flags = T)

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

  
  ### all places for losses data
  GetDatasetConfig("faostat_one","updated_sua_data")
  GetDatasetConfig("faostat_one","updated_sua_2013_data") 
  
  getDataFAOSTAT1 <- function(geographicAreaM49, element, measuredItemCPC, yearRange, dataset) {
    code <- element
    fcl =  suppressWarnings(as.character(as.numeric(cpc2fcl(measuredItemCPC, returnFirst = T))))
    fcl = fcl[!is.na(fcl)]
    countryFS =  suppressWarnings(m492fs(geographicAreaM49))
    countryFS = countryFS[!is.na(countryFS)]
    Key = DatasetKey(
      domain = "faostat_one",
      dataset = dataset,
      dimensions = list(
        Dimension(name = "geographicAreaFS",
                  keys = countryFS),
        Dimension(name = "measuredElementFS", keys = code),
        Dimension(name = "timePointYears", keys = yearRange),
        Dimension(name = "measuredItemFS",
                  keys = fcl)
      )
    )
    
    data = GetData(
      Key,
      flags = TRUE)
    
    data[, geographicAreaM49 := fs2m49(geographicAreaFS)]
    data[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS), width = 4,
                                              flag = "0"))]
    
    data[, flagObservationStatus := getFlagObservationStatus(flagFaostat)]
    data[, flagMethod := getFlagMethod(flagFaostat)]    
    
    data[, measuredElement := "5141"]
    data[, c("geographicAreaFS", "measuredItemFS", "measuredElementFS", "flagFaostat") := NULL]
    
    setcolorder(data, c("geographicAreaM49", "measuredElement",
                        "measuredItemCPC", "Value", "timePointYears",
                        "flagObservationStatus", "flagMethod"))
    
    data <- data[!is.na(geographicAreaM49)]
    
  }
  ##############################################################################
  #losses = 121
  countriesM49 <- GetCodeList(domain = "agriculture",dataset = "aproduction",
                              dimension ="geographicAreaM49")[type == "country", code]
  
  countriesM49 <- countriesM49[!(countriesM49 %in% c("831", "832", "274"))]
  
  cpcCodes <- GetCodeList(domain = "agriculture",dataset = "aproduction",
                          dimension ="measuredItemCPC")[, code]
  
  
  SuaY90_13data = getDataFAOSTAT1(countriesM49,
                         element = "121",
                         measuredItemCPC = cpcCodes,
                         yearRange = as.character(1990:2013),
                         dataset = "updated_sua_2013_data") 
  
  SuaY14_15data = getDataFAOSTAT1(countriesM49,
                         element = "121",
                         measuredItemCPC = cpcCodes,
                         yearRange = as.character(2014:2015),
                         dataset = "updated_sua_data") 
  
  
 Lossdata<-rbind(SuaY90_13data,lossQuery,SuaY14_15data)
 
 ## Taking only official data
 ## distinct(lossQuery,flagFaostat_measuredElementFS_5120)
 ## lossQuery = lossQuery[flagFaostat_measuredElementFS_5120 == "", ]
 
 if (protected) {
   protectedFlag <- flagValidTableLoss[flagValidTableLoss$Protected == TRUE,] %>%
     .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
   
   col_keep <- names(Lossdata) %>%
     .[.!="flagCombination"]
   
   ## subset to protected flags
   ## requires dtplyr, the data table back-end for 'dplyr'
   Lossdata <-
     Lossdata[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")] %>%
     merge(., protectedFlag, by = "flagCombination") %>%
     filter(Protected == TRUE) 
 }
 Lossdata
}
