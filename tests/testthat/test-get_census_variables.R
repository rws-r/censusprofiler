test_that("get_census_variables returns census variables", {
  CV <- get_census_variables(year=2022,
                             dataset_main = "acs",
                             dataset_sub = "acs5")
  expect_type(CV,"list")
  expect_s3_class(CV[[1]],"data.frame")
  expect_s3_class(CV[[2]],"data.frame")
})