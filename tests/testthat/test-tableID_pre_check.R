test_that("tableID_pre_check() works", {

  tipc <- tableID_pre_check("B01001_001",censusVars = CV)
  tipc_novar <- tableID_pre_check()
  tipc_badvar <- tableID_pre_check("BLK33333",censusVars = CV)
  
  expect_contains(tipc,"B")
  expect_contains(tipc_novar,"NO_TABLE_ID")
  expect_contains(tipc_badvar,"NOTFOUND")

})