context("Authentication")

#EA URL and token
test_that("Authentication initiates successfully", {
  
  eabaseUrl <<- "https://hqlqasws1.hq.un.fao.org:9453/sws"
  eatoken <<- "7cb7f85c-9083-41f2-a034-721304db4574" 
  
  if(CheckDebug()){
      certlocation <- "~/certificates/qa"
      SetClientFiles(certlocation)
    }
    
})
## Skip if authentication fails
#catch <- try(GetTestEnvironment(eabaseUrl, eatoken), silent = TRUE)
#should_skip <- inherits(catch, "try-error")

testAuthentication <- function(server, 
                               token){
  #if(should_skip) skip("Unable to connect. Check the certificate locations")
  GetTestEnvironment(server, token)[[1]]
}

test_that("Authentication to EA database", {
  expect_is(object = testAuthentication(eabaseUrl, eatoken), "DatasetKey")
  expect_error(testAuthentication(eabaseUrl, "badtoken"))
})
