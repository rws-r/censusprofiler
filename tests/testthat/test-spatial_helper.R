test_that("Spatial Helper links up data with geometries and returns an sf data object.", {
  baddata <- data$data$type2data
  gooddata <- data$data$type1data
  
  expect_s3_class(spatial_helper(gooddata,
                                 geography = "tract",
                                 geosObject = geos,
                                 verbose=verbose),"sf")
  
  # TODO Do I need to throw this error? 
  # expect_error(spatial_helper(baddata,
  #                             geography = "tract",
  #                             geosObject = geos,
  #                             verbose=verbose))
  
})