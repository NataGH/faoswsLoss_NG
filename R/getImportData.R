##' Get Import Data
##' 
##' Function to obtain import data at primary level
##'  
##' @export


getImportData = function(source = "sws"){

  
##################################################################################################
#   Code to get Import data from the Old System 
##################################################################################################
#
#   measuredItemFS = ReadDatatable(table = "loss_food_group") %>%
#     select(measured_item_fs,food_general_group,measured_item_cpc) %>%
#     filter(food_general_group == "primary")
#   
#   
#   importQuery = data.table(sws_query(area = 1:299, 
#                                      item = as.character(measuredItemFS$measured_item_fs), 
#                                      ele = 61, 
#                                      year = 1961:2015, 
#                                      value.names = F
#   ))
#  
#   setnames(importQuery, 
#            old = c(names(importQuery)),
#            new = c("geographicAreaFS","measuredItemFCL","measuredElement","timePointYears",
#                    "Value_measuredElement_5600","flagObservationStatus_measuredElement_5600")
#   )
#   
#   importQuery = 
#     importQuery%>%
#     select(geographicAreaFS,measuredItemFCL,timePointYears,
#            Value_measuredElement_5600,flagObservationStatus_measuredElement_5600)
#   
#   
#   ## Convert measuredItemFCL to measuredItemCPC
#   #   importQuery[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
#   
#   ## Convert geographicAreaFS to geographicAreaM49
#   #   importQuery[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
#   
#   
#   ## Adding headings to FCL codes
#   importQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
#   
#   ## Convert time to numeric
#   importQuery[, timePointYears := as.numeric(timePointYears)]
#   
#   importQuery
##################################################################################################  
#
#
##################################################################################################
#   Code to get Import data from the New System 
##################################################################################################

  ## allCountries =
  ##   GetCodeList(domain = "agriculture",
  ##               dataset = "aproduction",
  ##               dimension = "geographicAreaM49")[type == "country", code]
  ##
  ## # Convert geographicAreaM49 to geographicAreaFS  
  ## allCountriesFS = faoswsUtil::m492fs(as.character(allCountries))
  ## # Remove "NULL"s  beacuse they are invalide codes for dimension geographicAreaFS
  ## allCountriesFS = na.omit(allCountriesFS) 
  ## # Remove codes "274" "283" "280" "281" "279"
  ## # because They are invalide codes for dimension geographicAreaFS
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "274")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "283")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "280")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "281")]
  ## allCountriesFS = allCountriesFS[-which(allCountriesFS == "279")]
  ##
  ## importKey = DatasetKey(
  ##   domain = "faostat_one",
  ##   dataset = "FS1_SUA",
  ##   dimensions = list(
  ##     Dimension(name = areaVarFS,
  ##               keys = allCountriesFS),
  ##     Dimension(name = elementVarFS,
  ##               keys = "61"),
  ##     Dimension(name = itemVarFS,
  ##               keys = as.character(as.numeric(requiredItems$measuredItemFCL))),
  ##     Dimension(name = yearVar,
  ##               keys = selectedYear)
  ##   )
  ## )
  ##
  importKey <- faoswsUtil::getCompleteImputationKey(table = "loss")
  importKey@domain <- "trade"
  importKey@dataset <- "total_trade_cpc_m49"
  names(importKey@dimensions) <-
    sub("measuredElement", "measuredElementTrade", names(importKey@dimensions))
  importKey@dimensions[["measuredElementTrade"]]@name <- "measuredElementTrade"
  importKey@dimensions[["measuredElementTrade"]]@keys <- "5610"
  ## ## currently not working due to insufficient permissions
  ## faosws::GetDatasetConfig(domainCode = "trade",
  ##                          datasetCode = "total_trade_cpc_m49")

  if (tolower(source) == "sws") {
    
    ## Pivot to vectorize yield computation
    importPivot = c(
      ## Pivoting(code = areaVarFS, ascending = TRUE),
      ## Pivoting(code = itemVarFS, ascending = TRUE),
      ## Pivoting(code = yearVar, ascending = FALSE),
      ## Pivoting(code = elementVarFS, ascending = TRUE)
      Pivoting(code = areaVar, ascending = TRUE),
      Pivoting(code = itemVar, ascending = TRUE),
      Pivoting(code = yearVar, ascending = FALSE),
      ## Pivoting(code = elementVar, ascending = TRUE)
      Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )
    
    ## Query the data
    importQuery <-
      GetData(
        key = importKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = importPivot
      )

    ## rename(measuredElement = measuredElementTrade)
    
    ## setnames(importQuery,
    ##          old = names(importQuery),
    ##          new = c("geographicAreaFS","measuredItemFCL","timePointYears",
    ##                   "Value_measuredElement_5600","flagFaostat_measuredElementFS_5600")
    ##          )


    ## ## Convert geographicAreaM49 to geographicAreaFS
    ## importQuery[, geographicAreaM49 := as.numeric(faoswsUtil::fs2m49(as.character(geographicAreaFS)))]
    
    ## ## Convert measuredItemCPC to measuredItemFCL
    ## importQuery[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
    ## importQuery[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
    
    
    # ## Convert time to numeric
    # importQuery[, timePointYears := as.numeric(timePointYears)]
    # 
    # ## importQuery[, geographicAreaFS := as.numeric(geographicAreaFS)]
    # importQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
    # 
    # importQuery
    
  } else if (tolower(source) == "faostat") {

    ## importKey@dimensions[["geographicAreaM49"]]@keys
    ## measuredItemCPC
    ## cpc2fcl(measuredItemCPC, returnFirst = T)
    ## geographicAreaM49
    ## m492fs(geographicAreaM49)
    ## ## from faoswsFood
    ## totalTradeData <-faosws::ReadDatatable(table = "fal_2_m49")
    ## issue with M49 831 / fal 274 Guernsey
    ## issue with NA after conversion M49 -> fal
    ## importKey@dimensions[["geographicAreaM49"]]@keys <-
    ##   importKey@dimensions[["geographicAreaM49"]]@keys %>%
    ##   .[!. %in% c("831") &
    ##     !is.na(m492fs(importKey@dimensions[["geographicAreaM49"]]@keys))]
    
    importQuery = faoswsFood::getTotalTradeDataFAOSTAT1(
        geographicAreaM49 = importKey@dimensions[["geographicAreaM49"]]@keys,
        measuredItemCPC = importKey@dimensions[["measuredItemCPC"]]@keys,
        yearRange = importKey@dimensions[["timePointYears"]]@keys
      )
  
  importQuery[, measuredElement := NULL]    
  setnames(importQuery, "Value", "Value_measuredElement_5610")  

  }
  ## Convert time to numeric
  importQuery[, timePointYears := as.numeric(timePointYears)]
  
  ## importQuery[, geographicAreaFS := as.numeric(geographicAreaFS)]
  importQuery[, geographicAreaM49 := as.numeric(geographicAreaM49)]
  
  importQuery
  
}
