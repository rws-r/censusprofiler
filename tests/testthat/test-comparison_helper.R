test_that("comparison_helper() works", {
  
  expect_s3_class(comparison_helper(df=data$data$type1data,
                                    comparisonDF=statesCompare,
                                    comp_type = "state",
                                    tableID = "B02001",
                                    stateFilter = 17,
                                    verbose=FALSE),"data.frame")
  
  expect_s3_class(comparison_helper(df=data$data$type4data,
                                    comparisonDF=usCompare,
                                    comp_type = "us",
                                    tableID = "B02001",
                                    stateFilter = NULL,
                                    verbose=FALSE),"data.frame")
  
})
