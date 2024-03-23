test_that("varInputCheck() works", {
  x <- varInputCheck(tableID="B02001",variables=c("B02001_001","B02001_002"))
  expect_type(x,"list")
  expect_contains(names(x),c("tableID","variables","variableSummary"))
})
