test_that("variable_formatter() works", {
  tipc <- tableID_pre_check("B02001",cvMatch=TRUE,censusVars=CV)
  x <- variable_formatter("B02001_002",tipc=tipc,tableID="B02001",censusVars=CV)
  expect_contains(x,c("B02001_002E","B02001_002M"))

})
