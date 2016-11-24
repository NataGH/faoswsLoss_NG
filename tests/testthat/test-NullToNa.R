context("NullToNa")

test_that("Simple cases work", {
  expect_identical(NullToNa(NULL), NULL)
  expect_identical(NullToNa(list(NULL)), NA_character_)
  expect_identical(NullToNa(list(NULL, NULL)), as.character(c(NA, NA)))
})

test_that("Function works with other elements", {
  expect_identical(NullToNa(list("a", NULL, "b")), c("a", NA, "b"))
})
