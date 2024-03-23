test_that("pseudo_tableID() works", {
  x <- pseudo_tableID(c("B02001_001","B01001_001"),fast=TRUE,test=TRUE)
  expect_identical(x,c("B02001","B01001"))
})
