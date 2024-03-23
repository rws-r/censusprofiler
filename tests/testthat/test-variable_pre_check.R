test_that("variable_pre_check() works", {
  tipc <- tableID_pre_check("B01001_001")
  expect_identical(variable_pre_check("B01001_001",tipc),1)
  
})

test_that("variable_pre_check() throws error", {
  tipc_novar <- tableID_pre_check()
  expect_identical(variable_pre_check("B01001_001",tipc_novar),1)
  
})

test_that("variable_pre_check() throws error", {
  tipc_badvar <- tableID_pre_check("BLK33333")
  expect_error(variable_pre_check("B01001_001",tipc_badvar))
  
})