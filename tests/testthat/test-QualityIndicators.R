context("QualityIndicators")

test_that("FormatIndicatorList works", {
  test_basic <- list(a = list(a = c("a", "b"),
                              b = 1:2))
  test_nodt <- list(b = list(a = 1:3))
  test_lengthmismatch <- list(a = list(a = c("a", "b"),
                                       b = 1:3))
  test_notlist <- 1:3
  
  expect_equal(FormatIndicatorList(test_basic), 
                   list(a = data.table(a = c("a", "b"),
                                       b = 1:2)))
  expect_identical(FormatIndicatorList(test_nodt), test_nodt)
  expect_identical(FormatIndicatorList(test_lengthmismatch),
                   test_lengthmismatch)
  expect_identical(FormatIndicatorList(test_notlist), test_notlist)
})
