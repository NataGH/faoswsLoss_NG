suppressPackageStartupMessages(library(data.table))

context("Dataset read/write")

if(CheckDebug()) {
  SetClientFiles("~/certificates/qa")
}

GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = "515d04ce-dffd-44b9-9324-2a880dfe9f8e"
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

###############################################################################
# Observing behavior with incorrect years                                     #
###############################################################################

temp = GetData(swsContext.datasets[[1]])
toBind = copy(temp)
toBind[, geographicAreaM49 := "232"]
temp = rbind(temp, toBind)
toBind[, geographicAreaM49 := "230"]
temp = rbind(temp, toBind)
SaveData(domain = swsContext.datasets[[1]]@domain,
         dataset = swsContext.datasets[[1]]@dataset,
         temp)


###############################################################################
# Reordering/Pivoting                                                         #
###############################################################################

temp = GetData(swsContext.datasets[[1]])
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               temp)
test_that("Default save works ok", {
  expect_equal(out$ignored, 2)
  expect_equal(out$discarded, 0)
})

temp = GetData(swsContext.datasets[[1]], normalized = FALSE)
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               temp, normalized = FALSE)
test_that("Denormalized save works ok", {
  expect_equal(out$ignored, 2)
  expect_equal(out$discarded, 6)
  expect_equal(nrow(out$warnings), 6)
})

pivot = list(
  Pivoting(code = "geographicAreaM49"),
  Pivoting(code = "timePointYears"),
  Pivoting(code = "measuredItemCPC"),
  Pivoting(code = "measuredElement")
)
temp = GetData(swsContext.datasets[[1]], normalized = FALSE, pivoting = pivot)
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               temp, normalized = FALSE)
test_that("Denormalized/Pivoted save (first attempt) works ok", {
  expect_equal(out$ignored, 2)
  expect_equal(out$discarded, 3)
})

pivot = list(
  Pivoting(code = "geographicAreaM49"),
  Pivoting(code = "measuredElement"),
  Pivoting(code = "timePointYears"),
  Pivoting(code = "measuredItemCPC")
)
temp = GetData(swsContext.datasets[[1]], normalized = FALSE, pivoting = pivot)
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               temp, normalized = FALSE)
test_that("Denormalized/Pivoted save (second attempt) works ok", {
  expect_equal(out$ignored, 2)
  expect_equal(out$discarded, 2)
})
