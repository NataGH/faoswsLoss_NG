context("Datatables")

test_that("DtDimension works", {
  expect_error(DtDimension(), "An id attribute must be specified")
  expect_error(DtDimension(c("a", "b")), "The id attribute was not properly set")
})
