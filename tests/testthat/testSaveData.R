# How to use this script:
# 
# First, install the version of faosws that you would like to test.  Then,
# source this entire script.  If no errors occur, than SaveData passed all the
# tests!

# install.packages("~/Documents/SVN/faosws_0.3.6.tar.gz", type = "src", repo = NULL)
# install.packages("~/Documents/SVN/faosws_0.3.7.tar.gz", type = "src", repo = NULL)

library(faosws)
library(data.table)
library(testthat)
sessionInfo() # Confirmed that I'm running version 0.3.6

GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = "fd7b11a1-f144-41ec-8bfb-1003da93ec7f"
    ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    ## token = "7449da27-d4ab-43c2-8e8e-55fe806d473b"
)

swsContext.datasets[[1]] = DatasetKey(
    domain = "agriculture",
    dataset = "agriculture",
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
    expect_equal(out$ignored, 17)
    expect_equal(out$discarded, 0)
})

temp = GetData(swsContext.datasets[[1]], normalized = FALSE)
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               temp, normalized = FALSE)
test_that("Denormalized save works ok", {
    expect_equal(out$ignored, 17)
    expect_equal(out$discarded, 3)
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
    expect_equal(out$ignored, 17)
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
    expect_equal(out$ignored, 17)
    expect_equal(out$discarded, 17)
})


###############################################################################
# Check flag logic                                                            #
###############################################################################


temp = GetData(swsContext.datasets[[1]], flags = FALSE)
test_that("Fails with both flags missing", {
    expect_error(
        SaveData(domain = swsContext.datasets[[1]]@domain,
                dataset = swsContext.datasets[[1]]@dataset,
                temp))
})

temp = GetData(swsContext.datasets[[1]])
temp[, flagMethod := NULL]
test_that("Fails with method flag missing", {
    expect_error(
        SaveData(domain = swsContext.datasets[[1]]@domain,
                dataset = swsContext.datasets[[1]]@dataset,
                temp)
    )
})

temp = GetData(swsContext.datasets[[1]])
temp[, flagObservationStatus := NULL]
test_that("Fails with both observation flag missing", {
    expect_error(
        SaveData(domain = swsContext.datasets[[1]]@domain,
                dataset = swsContext.datasets[[1]]@dataset,
                temp)
    )
})

###############################################################################
# Verifying invalid values aren't saved                                       #
###############################################################################

temp = GetData(swsContext.datasets[[1]])

temp[, flagObservationStatus := "1"]
## No data is saved because "1" is an invalid flag: see warnings
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               data = temp)
test_that("No rows are saved with if flag = '1'", {
    expect_equal(do.call("max", out[1:3]), 0)
    expect_equal(out$discarded, 17)
})

temp[, flagObservationStatus := ""]
## Some data is written back, if there's a change in the flag.  Any values that
## originally had "" are ignored.
out = SaveData(domain = swsContext.datasets[[1]]@domain,
               dataset = swsContext.datasets[[1]]@dataset,
               data = temp)
test_that("Everything is ok if flag = 'M'", {
    expect_equal(out$ignored, 17)
})