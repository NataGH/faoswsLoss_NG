context("Dataset read/write")

suppressPackageStartupMessages(library(data.table))

if(CheckDebug()) {
  SetClientFiles("~/certificates/qa")
}

test_that("Connecting to SWS for write tests works", {
  expect_error({
    GetTestEnvironment(
      baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
      token = "c6ce2c30-dc50-444a-99de-eceaa9bb1554"
    )
    
    swsContext.datasets[[1]] = DatasetKey(
      domain = "agriculture",
      dataset = "aproduction",
      dimensions = list(
        Dimension(name = "geographicAreaM49", keys = "32"),
        Dimension(name = "measuredElement",
                  keys = c("5031", "53200", "53201", "55100", "5514")),
        Dimension(name = "measuredItemCPC", keys = c("02111", "21131.01")),
        Dimension(name = "timePointYears", keys = as.character(2011:2014))
      ))
  }, NA)
})

###############################################################################
# Observing behavior with incorrect years                                     #
###############################################################################
test_that("Test writing to other countries", {
  expect_error({
  temp = GetData(swsContext.datasets[[1]])
  toBind = copy(temp)
  toBind[, geographicAreaM49 := "232"]
  temp = rbind(temp, toBind)
  toBind[, geographicAreaM49 := "230"]
  temp = rbind(temp, toBind)
  SaveData(domain = swsContext.datasets[[1]]@domain,
         dataset = swsContext.datasets[[1]]@dataset,
         temp)
  }, NA)
})


###############################################################################
# Processing                                                                  #
###############################################################################

test_that("processNormalizedResult works", {
  input <- structure(list(keyDefinitions = list(structure(c("geographicAreaM49", 
                     "Geographic Area", "normal"), .Names = c("code", "description", 
                     "type")), structure(c("measuredElement", "Element", "measurementUnit"
                     ), .Names = c("code", "description", "type")), structure(c("measuredItemCPC", 
                     "Item", "normal"), .Names = c("code", "description", "type")), 
                     structure(c("timePointYears", "Year", "time"), .Names = c("code", 
                     "description", "type"))), flagDefinitions = list(structure(c("flagObservationStatus", 
                     "Status"), .Names = c("code", "description")), structure(c("flagMethod", 
                     "Method"), .Names = c("code", "description"))), data = list(c("4", 
                     "5031", "02111", "2005"), list("4", "5417", "02111", "2005", 
                     0, "M", "u"))), .Names = c("keyDefinitions", "flagDefinitions", 
                     "data"))
  
  output <- structure(list(geographicAreaM49 = c("4", "4"), measuredElement = c("5031", 
                      "5417"), measuredItemCPC = c("02111", "02111"), timePointYears = c("2005", 
                      "2005"), Value = c(NA, 0), flagObservationStatus = c(NA, "M"), 
                      flagMethod = c(NA, "u")), .Names = c("geographicAreaM49", 
                      "measuredElement", "measuredItemCPC", "timePointYears", "Value", 
                      "flagObservationStatus", "flagMethod"), row.names = c(NA, -2L
                      ), class = c("data.table", "data.frame"))
  
  expect_equal(GetData.processNormalizedResult(input, flag = TRUE), output)
})

test_that("denormalizeData works", {
  input_data <- structure(list(keyDefinitions = list(structure(c("geographicAreaM49", 
                "Geographic Area", "normal"), .Names = c("code", "description", 
                "type")), structure(c("measuredElement", "Element", "measurementUnit"
                ), .Names = c("code", "description", "type")), structure(c("measuredItemCPC", 
                "Item", "normal"), .Names = c("code", "description", "type")), 
                structure(c("timePointYears", "Year", "time"), .Names = c("code", 
                "description", "type"))), flagDefinitions = list(structure(c("flagObservationStatus", 
                "Status"), .Names = c("code", "description")), structure(c("flagMethod", 
                "Method"), .Names = c("code", "description")))), .Names = c("keyDefinitions", 
                "flagDefinitions"))

  input_query <- data.table::data.table(geographicAreaM49 = c("4", "466", "466", "466", "466"), 
             measuredElement = c("5417", "5031", "5031", "5417", "5417"), 
             measuredItemCPC = c("02111", "02111", "02111", "02111", "02111"), 
             timePointYears = c("2005", "2007", "2005", "2007", "2005"), 
             Value = c(0, 5005300, 4500000, 0, 0), 
             flagObservationStatus = c("M", "", "E", "M", "M"), 
             flagMethod = c("u", "-", "f", "u", "u")
             )
  
  input_key <- new("DatasetKey"
                   , domain = "agriculture"
                   , dataset = "aproduction"
                   , dimensions = structure(list(geographicAreaM49 = new("Dimension"
                   , name = "geographicAreaM49"
                   , keys = c("AH01", "466", "4")), 
                   measuredElement = new("Dimension"
                   , name = "measuredElement"
                   , keys = c("5141", "5031", "5417")), 
                   measuredItemCPC = new("Dimension"
                   , name = "measuredItemCPC"
                   , keys = c("AH01", "02111")), 
                   timePointYears = new("Dimension"
                   , name = "timePointYears"
                   , keys = c("2015", "2007", "2005", "2016")
                   )), .Names = c("geographicAreaM49", 
                   "measuredElement", "measuredItemCPC", "timePointYears")
                   ), sessionId = integer(0))
  
  output <- structure(list(geographicAreaM49 = c("4", "466", "466"), measuredElement = c("5417", 
                      "5031", "5417"), measuredItemCPC = c("02111", "02111", "02111"
                      ), Value_timePointYears_2005 = c(0, 4500000, 0), flagObservationStatus_timePointYears_2005 = c("M", 
                      "E", "M"), flagMethod_timePointYears_2005 = c("u", "f", "u"), 
                      Value_timePointYears_2007 = c(NA, 5005300, 0), flagObservationStatus_timePointYears_2007 = c(NA, 
                      "", "M"), flagMethod_timePointYears_2007 = c(NA, "-", "u"
                      ), Value_timePointYears_2015 = c(NA_real_, NA_real_, NA_real_
                      ), flagObservationStatus_timePointYears_2015 = c(NA_character_, 
                      NA_character_, NA_character_), flagMethod_timePointYears_2015 = c(NA_character_, 
                      NA_character_, NA_character_), Value_timePointYears_2016 = c(NA_real_, 
                      NA_real_, NA_real_), flagObservationStatus_timePointYears_2016 = c(NA_character_, 
                      NA_character_, NA_character_), flagMethod_timePointYears_2016 = c(NA_character_, 
                      NA_character_, NA_character_)), .Names = c("geographicAreaM49", 
                      "measuredElement", "measuredItemCPC", "Value_timePointYears_2005", 
                      "flagObservationStatus_timePointYears_2005", "flagMethod_timePointYears_2005", 
                      "Value_timePointYears_2007", "flagObservationStatus_timePointYears_2007", 
                      "flagMethod_timePointYears_2007", "Value_timePointYears_2015", 
                      "flagObservationStatus_timePointYears_2015", "flagMethod_timePointYears_2015", 
                      "Value_timePointYears_2016", "flagObservationStatus_timePointYears_2016", 
                      "flagMethod_timePointYears_2016"), sorted = c("geographicAreaM49", 
                      "measuredElement", "measuredItemCPC"), class = c("data.table", 
                      "data.frame"), row.names = c(NA, -3L))
  
  expect_equal(denormalizeData(input_data, input_query, input_key), output)
})
