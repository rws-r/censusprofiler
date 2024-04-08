test_that("map_locations() adequately plots locations.", {
  
  expect_identical(class(map_locations(addressList)),c("gg","ggplot"))
  
})
