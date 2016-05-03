context("Authentication")

#EA URL and token
test_that("Authentication initiates successfully", {
  
  eabaseUrl <<- "https://hqlqasws1.hq.un.fao.org:9453/sws"
  eatoken <<- "50ced34b-2663-43c3-8691-eaad30924e16" 
  
  if(CheckDebug()){
      certlocation <- "~/certificates/qa"
      SetClientFiles(certlocation)
    }
    
})
## Skip if authentication fails
#catch <- try(GetTestEnvironment(eabaseUrl, eatoken), silent = TRUE)
#should_skip <- inherits(catch, "try-error")

testAuthentication <- function(server="https://hqlprswsas1.hq.un.fao.org:8181/sws", 
                               token="d0caea7d-c1c8-4259-9e1f-7ecaf4343706"){
  #if(should_skip) skip("Unable to connect. Check the certificate locations")
  GetTestEnvironment(server, token)[[1]]
}

test_that("Authentication to EA database", {
  expect_is(object = testAuthentication(eabaseUrl, eatoken), "DatasetKey")
  expect_error(testAuthentication(eabaseUrl, "badtoken"))
})

