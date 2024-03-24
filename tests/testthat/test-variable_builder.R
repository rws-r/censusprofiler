test_that("variable_bulider produces a list of variables", {
  
  expect_type(variable_builder(c("B01001","B01002"),
                               censusVars=CV,
                               varStartNum = c(1,1),
                               varEndNum=c(3,3),
                               varArray=NULL,
                               varSummaryNum = c(1,1),
                               verbose=verbose),
              "list")
})
