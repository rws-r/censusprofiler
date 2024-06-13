test_that("tableID_variable_preflight works", {
  expect_identical(names(tableID_variable_preflight(tableID="B02001",
                                              variables=c("B02001_001","B02001_002"),
                                              censusVars = CV,
                                              return = "all")),
                   c("table_id","variable","tableID.join"))
  
  expect_type(tableID_variable_preflight(tableID="B02001",
                                                    variables=c("B02001_001","B02001_002"),
                                                    censusVars = CV,
                                                    return = "tableID"),
                   "character")
  
  expect_gt(length(tableID_variable_preflight(tableID="B02001",
                                                    variables=c("B02001_001","B02001_002"),
                                                    censusVars = CV,
                                                    return = "variable")),
                   1)
  
  expect_identical(names(tableID_variable_preflight(tableID="B02001",
                                                    variables=c("B02001_001","B02001_002"),
                                                    censusVars = CV,
                                                    return = "raw")),
                   c("table_id","variable"))
})
