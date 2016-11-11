context("SaveData")

test_that("SaveData validates correctly",{
  testDenormalisedData <- structure(list(geographicAreaM49 = "4", measuredElement = "5417", 
                                         measuredItemCPC = "02111", Value_timePointYears_2005 = 0, 
                                         flagObservationStatus_timePointYears_2005 = "M", flagMethod_timePointYears_2005 = "u"), 
                                    .Names = c("geographicAreaM49", 
                                               "measuredElement", "measuredItemCPC", "Value_timePointYears_2005", 
                                               "flagObservationStatus_timePointYears_2005", "flagMethod_timePointYears_2005"                                  )
                                    , row.names = c(NA, -1L), class = c("data.table", "data.frame")
  )
# Test for regression where denormalised was falsely rejected
  expect_error(not(
    SaveData.validate("agriculture", "aproduction", testDenormalisedData, normalized = FALSE)
    ))
})