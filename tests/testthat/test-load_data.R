test_that("load_data() works", {
  x <- load_data(load_censusVars = TRUE,
                 load_geos = TRUE,
                 load_stats = TRUE,
                 load_profile_compare = TRUE,
                 geography = "tract",
                 geo_data = c("state","county","tract"),
                 loadToGlobal = FALSE,
                 year = 2022,
                 tableID = "B01001",
                 variables = c("B01001_001","B01001_002","B01001_026"),
                 geosObject=geos,
                 test=TRUE,
                 verbose=verbose)
  
  expect_identical(names(x),c("CV","geos","stats","stateCompare","usCompare"))
  
})
