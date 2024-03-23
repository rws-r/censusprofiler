test_that("geo_census() works", {
  x <- geo_census(address2,year=2020)
  expect_s3_class(x,c("sf","data.frame"))
})
