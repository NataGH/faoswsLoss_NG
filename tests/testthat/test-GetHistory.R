context("GetHistory")

test_that("GetHistory buildJSON works", {
  test_dsk <- DatasetKey(domain = "testdomain", dataset = "testdataset", 
                         dimensions = list(
                           Dimension(name = "key1", keys = letters[1:3]),
                           Dimension(name = "key2", keys = letters[4:6])
                         ))
  #cat(capture.output(pryr::where("swsContext.token")))
  swsContext.token <<- "abcdef123456"
  #cat(capture.output(environment(GetHistory.buildJSON)))
  test_output  <- GetHistory.buildJSON(test_dsk)
  expect_equal(test_output[["dimension2codes"]], structure(list(key1 = structure(c("a", "b", "c"), class = "AsIs"), 
                                                                key2 = structure(c("d", "e", "f"), class = "AsIs")), .Names = c("key1", 
                                                                                                                                "key2")))
  expect_equal(test_output[["token"]], swsContext.token)
  
  
})

test_that("GetHistory processing works",{
  input <- structure(list(keyDefinitions = list(structure(c("geographicAreaM49", 
            "Geographic Area", "normal"), .Names = c("code", "description", 
            "type")), structure(c("measuredElement", "Element", "measurementUnit"
            ), .Names = c("code", "description", "type")), structure(c("measuredItemCPC", 
            "Item", "normal"), .Names = c("code", "description", "type")), 
            structure(c("timePointYears", "Year", "time"), .Names = c("code", 
            "description", "type"))), flagDefinitions = list(structure(c("flagObservationStatus", 
            "Status"), .Names = c("code", "description")), structure(c("flagMethod", 
            "Method"), .Names = c("code", "description"))), data = list(list(
            "4", "5417", "02111", "2005", 0, 4, 1473774149730, NULL, 
            "M", "u", list(list("COMPUTATION_INFO", "Computation Information", 
            "en", list(c("PARAMETER", "Parameter", "updateAll = session"
            ), c("COMPUTATION", "Computation", "Compute Yield"), 
            c("PARAMETER", "Parameter", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)"
            ))), list("GENERAL", "General", "en", list(c("COMMENT", 
            "Comment", "The observation was rolled back to the version 1"
            )))), list(list(2, 3, 1473774106302, 1473774149730, "M", 
            "u", list(list("COMPUTATION_INFO", "Computation Information", 
            "en", list(c("PARAMETER", "Parameter", "code = 'test'"
            ), c("COMPUTATION", "Computation", "Dummy module"
            ), c("PARAMETER", "Parameter", "Main dataset Agriculture Production/Agriculture Production (main)"
            ))))), list(1, 2, 1473774035059, 1473774106302, "M", 
            "u", list(list("COMPUTATION_INFO", "Computation Information", 
            "en", list(c("PARAMETER", "Parameter", "code = 'meh'"
            ), c("COMPUTATION", "Computation", "Dummy module"
            ), c("PARAMETER", "Parameter", "Main dataset Agriculture Production/Agriculture Production (main)"
            ))))), list(0, 1, 1449577134707, 1473774035059, "M", 
            "u", list(list("COMPUTATION_INFO", "Computation Information", 
            "en", list(c("PARAMETER", "Parameter", "updateAll = session"
            ), c("COMPUTATION", "Computation", "Compute Yield"
            ), c("PARAMETER", "Parameter", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)"
            ))))))))), .Names = c("keyDefinitions", "flagDefinitions", 
            "data"))
  
  output <- structure(list(geographicAreaM49 = c("4", "4", "4", "4", "4", 
             "4", "4", "4", "4", "4", "4", "4", "4"), measuredElement = c("5417", 
             "5417", "5417", "5417", "5417", "5417", "5417", "5417", "5417", 
             "5417", "5417", "5417", "5417"), measuredItemCPC = c("02111", 
             "02111", "02111", "02111", "02111", "02111", "02111", "02111", 
             "02111", "02111", "02111", "02111", "02111"), timePointYears = c("2005", 
             "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", 
             "2005", "2005", "2005", "2005"), Version = c(4, 4, 4, 4, 3, 3, 
             3, 2, 2, 2, 1, 1, 1), StartDate = c(1473774149730, 1473774149730, 
             1473774149730, 1473774149730, 1473774106302, 1473774106302, 1473774106302, 
             1473774035059, 1473774035059, 1473774035059, 1449577134707, 1449577134707, 
             1449577134707), EndDate = c(NA, NA, NA, NA, 1473774149730, 1473774149730, 
             1473774149730, 1473774106302, 1473774106302, 1473774106302, 1473774035059, 
             1473774035059, 1473774035059), Metadata = c("COMPUTATION_INFO", 
             "COMPUTATION_INFO", "COMPUTATION_INFO", "GENERAL", "COMPUTATION_INFO", 
             "COMPUTATION_INFO", "COMPUTATION_INFO", "COMPUTATION_INFO", "COMPUTATION_INFO", 
             "COMPUTATION_INFO", "COMPUTATION_INFO", "COMPUTATION_INFO", "COMPUTATION_INFO"
             ), Metadata_Element = c("PARAMETER", "COMPUTATION", "PARAMETER", 
             "COMMENT", "PARAMETER", "COMPUTATION", "PARAMETER", "PARAMETER", 
             "COMPUTATION", "PARAMETER", "PARAMETER", "COMPUTATION", "PARAMETER"
             ), Metadata_Language = c("en", "en", "en", "en", "en", "en", 
             "en", "en", "en", "en", "en", "en", "en"), Metadata_Group = c(1L, 
             2L, 3L, 1L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), Metadata_Value = c("updateAll = session", 
             "Compute Yield", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)", 
             "The observation was rolled back to the version 1", "code = 'test'", 
             "Dummy module", "Main dataset Agriculture Production/Agriculture Production (main)", 
             "code = 'meh'", "Dummy module", "Main dataset Agriculture Production/Agriculture Production (main)", 
             "updateAll = session", "Compute Yield", "Main dataset Agriculture Production/Agriculture Production on session APR 2015-12-08 11:54:20 (524168)"
             ), Value = c(0, 0, 0, 0, 2, 2, 2, 1, 1, 1, 0, 0, 0), flagObservationStatus = c("M", 
             "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M"), 
             flagMethod = c("u", "u", "u", "u", "u", "u", "u", "u", "u", 
             "u", "u", "u", "u")), .Names = c("geographicAreaM49", "measuredElement", 
             "measuredItemCPC", "timePointYears", "Version", "StartDate", 
             "EndDate", "Metadata", "Metadata_Element", "Metadata_Language", 
             "Metadata_Group", "Metadata_Value", "Value", "flagObservationStatus", 
             "flagMethod"), row.names = c(NA, -13L), class = c("data.table", 
             "data.frame"))
  
  expect_equal(GetHistory.processNormalizedResult(input), output)
  
})
