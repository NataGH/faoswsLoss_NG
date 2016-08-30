context("GetCodeList")

test_that("GetCodeList returns correct empty object for no codes", {
  expect_error(not(gcl <<- GetCodeList("agriculture", "aproduction", "measuredItemCPC", character())))
  # For testthat v0.11
  #expect_error(GetCodeList("agriculture", "aproduction", "measuredItemCPC", character()), NA)
  expect_is(gcl, "data.table")
  expect_identical(names(gcl), c("code", "description", "selectionOnly", "type", "startDate", 
                                 "endDate"))
  expect_equal(ncol(gcl), 6)
  expect_equal(nrow(gcl), 0)
})
