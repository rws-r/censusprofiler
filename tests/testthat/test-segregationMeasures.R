test_that("segregationMeasures() works", {
  x <- segregationMeasures(data = data,
                           majorityVar = 2,
                           minorityVar = 3,
                           tableID = "B02001",
                           censusVars = CV,
                           geosObject = geos)
  expect_s3_class(x,"data.frame")
  expect_contains(names(x),c("Measure","Value","Range","SegregationValue"))
})
