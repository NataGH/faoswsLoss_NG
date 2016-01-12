library(faosws)

context("Authentication")

#Production URL and token
prbaseUrl <- "https://hqlprswsas1.hq.un.fao.org:8181/sws"
prtoken <- "d0caea7d-c1c8-4259-9e1f-7ecaf4343706" 


test_that("Authentication to production database", {
  expect_is(object = GetTestEnvironment(prbaseUrl, prtoken)[[1]], "DatsetKey")
  expect_error(GetTestEnvironment(prbaseUrl, "badtoken"))
})


#QA URL and token
qabaseUrl <- "https://hqlqasws1.hq.un.fao.org:8181/sws" #qa
qatoken <- "d858f232-9ecd-4af6-945d-b872cd140e9f" #qa

test_that("Authentication should fail with the wrong certificates", {
  expect_error(GetTestEnvironment(qabaseUrl, qatoken))
})