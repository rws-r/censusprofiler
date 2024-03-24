test_that("stat_helper() works", {
    
    ## Check that zScores are included in colNames.
    expect_contains(names(stat_helper(data$data$type4data,
                                      statTable = stat_table,
                                      variables = profile_variables,
                                      censusVars = CV,
                                      tableID = "B02001",
                                      variable = "B02001_005",
                                      zThresh = 1.0,
                                      verbose=FALSE)),
                    c("zScore_pct","zScore_est"))
    
  ## Check that it produces verbose flag.
  expect_output(stat_helper(data,
                            statTable = stat_table,
                            variables = profile_variables,
                            censusVars = CV,
                            tableID = "B02001",
                            variable = "B02001_005",
                            zThresh = 1.0,
                            verbose=TRUE))
  
})
