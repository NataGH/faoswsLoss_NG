context("Dataset read/write")

suppressPackageStartupMessages(library(data.table))

if(CheckDebug()) {
  SetClientFiles("~/certificates/qa")
}

test_that("Connecting to SWS for write tests works", {
  expect_error(not({
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
  }))
})

###############################################################################
# Observing behavior with incorrect years                                     #
###############################################################################
test_that("Test writing to other countries", {
  expect_error(not({
  temp = GetData(swsContext.datasets[[1]])
  toBind = copy(temp)
  toBind[, geographicAreaM49 := "232"]
  temp = rbind(temp, toBind)
  toBind[, geographicAreaM49 := "230"]
  temp = rbind(temp, toBind)
  SaveData(domain = swsContext.datasets[[1]]@domain,
         dataset = swsContext.datasets[[1]]@dataset,
         temp)
  }))
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
