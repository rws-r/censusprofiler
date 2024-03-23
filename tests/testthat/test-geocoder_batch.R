
test_that("geocoder_batch() works with vector", {

  x <- geocoder_batch(addy,
                      addressCol = "address",
                      start=NULL,
                      limit=NULL,
                      verbose=verbose)
  
  expect_s3_class(x,c("sf","data.frame"))
  expect_contains(names(x),"geometry")
})

test_that("geocoder_batch() works with dataframe", {
  
  y <- geocoder_batch(addyf,
                      addressCol = "address",
                      start=NULL,
                      limit=NULL,
                      verbose=verbose)
  
  expect_s3_class(y,c("sf","data.frame"))
  expect_contains(names(y),"geometry")
})

test_that("geocoder_batch() works with start and limit", {
  
  z <- geocoder_batch(addyf,
                      addressCol = "address",
                      start=2,
                      limit=3,
                      verbose=verbose)
  
  expect_s3_class(z,c("sf","data.frame"))
  expect_contains(names(z),"geometry")
  expect_length(z,2)
})

