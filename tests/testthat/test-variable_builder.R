test_that("variable_bulider produces a list of variables", {
  
  expect_type(variable_builder(c("B01001","B01002"),
                               censusVars=ACS[[1]],
                               varStartNum = c(1,1),
                               varEndNum=c(3,3),
                               varArray=NULL,
                               varSummaryNum = c(1,1)),
              "list")
})