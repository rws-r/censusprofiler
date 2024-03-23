test_that("geo_OSM() works", {
  x <- geo_OSM(address,year=year)
  expect_s3_class(x,c("sf","data.frame"))
})
