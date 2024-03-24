test_that("dateInputCheck() works", {
  cvYear <- as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-2
  decyear <- floor(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))/10)*10
  
  x <- dateInputCheck(dataset_sub ="acs5")
  y <- dateInputCheck(dataset_sub ="ddhca")
  z <- dateInputCheck(2022,"acs1")
  
  expect_identical(x,cvYear)
  expect_identical(y,decyear)
  expect_identical(z,2022)
})
