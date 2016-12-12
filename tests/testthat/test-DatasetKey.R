context("DatasetKey")

test_that("DatasetKey allows empty keys but not dimensions", {
  expect_error(DatasetKey(domain = "agriculture", 
                              dataset = "aproduction", 
                              dimensions = c(geographicAreaM49 = Dimension(name = "geographicAreaM49"))),
               NA)

  expect_error(Dimension())
})
