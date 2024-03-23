test_that("Geo: Road Helper filters roads as expected.", {
  
  roads <- tigris::roads(state=36,county=061,year=2021,filter_by=buffer)
  
  expect_s3_class(geo_road_helper(roads),"sf")
  expect_contains(names(geo_road_helper(roads)),"suffix")
  
})