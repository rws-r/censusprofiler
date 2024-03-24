test_that("set_api_key() works", {
  key <- "001001001001AABAABAAB"
  keynew <- "988988988988ZYYZYYZYYZYY"
  
  sk <- Sys.getenv("TEST_CENSUS_API_KEY")
  expect_identical(sk,"")
  
  set_api_key(key,test=TRUE)
  sk <- Sys.getenv("TEST_CENSUS_API_KEY")
  expect_identical(sk,key)
  
  set_api_key(keynew,test=TRUE)
  sk <- Sys.getenv("TEST_CENSUS_API_KEY")
  expect_identical(sk,keynew)
  
  expect_error(set_api_key())
  
  
  Sys.unsetenv("TEST_CENSUS_API_KEY")

})
