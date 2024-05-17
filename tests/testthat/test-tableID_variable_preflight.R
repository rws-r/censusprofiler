test_that("tableID_variable_preflight works", {
  expect_identical(names(tableID_variable_preflight(tableID="B02001",
                                              variables=c("B02001_001","B02001_002"),
                                              censusVars = CV)),
                   c("tableID","variables"))
})
