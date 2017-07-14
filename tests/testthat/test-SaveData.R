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
  expect_error(SaveData.validate("agriculture", "aproduction", testDenormalisedData, normalized = FALSE),
    NA)
})

test_that("normalizeData works correctly", {

  testdata_normalizeData <- structure(list(measuredItemCPC = c("Beef", "Wheat"), Value_area_France = c(803.493349812925, 
    847.820020280778), Value_area_Spain = c(NA, 246.175302891061)), .Names = c("measuredItemCPC", 
    "Value_area_France", "Value_area_Spain"), row.names = c(NA, -2L
    ), class = c("data.table", "data.frame"))
  
  expected_value <- structure(list(measuredItemCPC = c("Beef", "Wheat", "Wheat"), 
    area = c("France", "France", "Spain"), Value = c(803.493349812925, 
    847.820020280778, 246.175302891061)), .Names = c("measuredItemCPC", 
    "area", "Value"), row.names = c(NA, -3L), class = c("data.table", 
    "data.frame"))
  expected_value_NA <- structure(list(measuredItemCPC = c("Beef", "Beef", "Wheat", "Wheat"
    ), area = c("France", "Spain", "France", "Spain"), Value = c(803.493349812925, 
    NA, 847.820020280778, 246.175302891061)), .Names = c("measuredItemCPC", 
    "area", "Value"), row.names = c(NA, -4L), class = c("data.table", 
    "data.frame"))
  
  expect_equal(normalizeData(testdata_normalizeData, "measuredItemCPC", "area", keepNA = FALSE),
               expected_value)
  expect_equal(normalizeData(testdata_normalizeData, "measuredItemCPC", "area"),
               expected_value_NA)
})

test_that("SaveData doesn't save missing values with flags", {
  #SWS-1411
  d <- data.table(k1 = letters[1:3],
                  Value = c(1,2,3),
                  f1 = letters[7:9])
                  
                  
                  f <- "f1"
                  expect_error(SaveData.validateFlagValues(d, f), NA)
                  
                  
                  d2 <- data.table(k1 = "a",
                                   Value = NA_character_,
                                   f1 = "-")
                  
                  expect_error(SaveData.validateFlagValues(d2, f))
                  
})

test_that("Missing values and flags are accepted while missing values and present flags are rejected", {
  d <- data.table(x="a", Value = NA_real_, flag1 = NA_character_, flag2 = NA_character_)
  expect_error(SaveData.validateFlagValues(d, c("flag1", "flag2")), NA)
})
