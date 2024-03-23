x <- geo_marker_builder(obj=NULL,
                        dispRoads=TRUE,
                        dispWater=TRUE,
                        dispPlaces=TRUE,
                        dispRails=TRUE,
                        geo_resolution=2,
                        ggrObject=ggrObject,
                        year=year,
                        verbose=verbose,
                        st=NULL)

test_that("geo_marker_builder() works", {

  y <- geo_marker_builder(obj=x,
                          dispRoads=TRUE,
                          dispWater=TRUE,
                          dispPlaces=TRUE,
                          dispRails=TRUE,
                          geo_resolution=2,
                          ggrObject=ggrObject,
                          year=year,
                          verbose=verbose,
                          st=NULL)
  
  expect_type(x,"list")
  expect_type(y,"list")
  expect_identical(names(x),c("roads","water","places","rails"))
  expect_identical(names(y),c("roads","water","places","rails"))
})

test_that("geo_marker_builder() throws appropriate errors", {
  
  expect_error(geo_marker_builder(obj=NULL,
                                  dispRoads=TRUE,
                                  dispWater=TRUE,
                                  dispPlaces=TRUE,
                                  dispRails=TRUE,
                                  geo_resolution=2,
                                  ggrObject=NULL,
                                  year=year,
                                  verbose=verbose,
                                  st=NULL))
  
  z <- geo_marker_builder(obj=x,
                          dispRoads=FALSE,
                          dispWater=TRUE,
                          dispPlaces=TRUE,
                          dispRails=TRUE,
                          geo_resolution=2,
                          ggrObject=ggrObject,
                          year=year,
                          verbose=verbose,
                          st=NULL)
  
  expect_error(geo_marker_builder(obj=z,
                                  dispRoads=TRUE,
                                  dispWater=TRUE,
                                  dispPlaces=TRUE,
                                  dispRails=TRUE,
                                  geo_resolution=2,
                                  ggrObject=ggrObject,
                                  year=year,
                                  verbose=verbose,
                                  st=NULL))
  
})



