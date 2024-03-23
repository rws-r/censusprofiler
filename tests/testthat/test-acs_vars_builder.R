test_that("ACS Vars Builder returns ACS variables, if not existing", {
  ACS <- acs_vars_builder(year=2022,return=TRUE)
  expect_type(ACS,"list")
  expect_s3_class(ACS[[1]],"data.frame")
  expect_s3_class(ACS[[2]],"data.frame")
})