context("GetMetadata")

test_that("GetMetadata produces data in the right format",{
  # Test introduced due to SWS-1556
  testdata <- structure(list(keyDefinitions = list(structure(c("geographicAreaM49", 
  "Geographic Area", "normal"), .Names = c("code", "description", 
  "type")), structure(c("measuredElement", "Element", "measurementUnit"
  ), .Names = c("code", "description", "type")), structure(c("measuredItemCPC", 
  "Item", "normal"), .Names = c("code", "description", "type")), 
  structure(c("timePointYears", "Year", "time"), .Names = c("code", 
  "description", "type"))), data = list(list("4", "5417", "02111", 
  "2005", 0, list(list("COMPUTATION_INFO", "Computation Information", 
  "en", list(c("PARAMETER", "Parameter", "updateAll = session"
  ), c("COMPUTATION", "Computation", "Compute Yield"), 
  c("PARAMETER", "Parameter", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)"
  ))), list("GENERAL", "General", "en", list(c("COMMENT", 
  "Comment", "The observation was rolled back to the version 1"
  ))))))), .Names = c("keyDefinitions", "data"))
 
  expected_output <- structure(list(geographicAreaM49 = c("4", "4", "4", "4"), measuredElement = c("5417", 
                      "5417", "5417", "5417"), measuredItemCPC = c("02111", "02111", 
                      "02111", "02111"), timePointYears = c("2005", "2005", "2005", 
                      "2005"), Metadata = c("COMPUTATION_INFO", "COMPUTATION_INFO", 
                      "COMPUTATION_INFO", "GENERAL"), Metadata_Element = c("PARAMETER", 
                      "COMPUTATION", "PARAMETER", "COMMENT"), Metadata_Language = c("en", 
                      "en", "en", "en"), Metadata_Group = c(1L, 2L, 3L, 1L), Metadata_Value = c("updateAll = session", 
                      "Compute Yield", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)", 
                      "The observation was rolled back to the version 1")), .Names = c("geographicAreaM49", 
                      "measuredElement", "measuredItemCPC", "timePointYears", "Metadata", 
                      "Metadata_Element", "Metadata_Language", "Metadata_Group", "Metadata_Value"
                      ), row.names = c(NA, -4L), class = c("data.table", "data.frame"))
  
  expect_equal(GetData.processNormalizedResultMetadata(testdata), expected_output)
})
