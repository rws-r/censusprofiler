test_that("profile_helper() works", {
  profileHelperObject <- profile_helper(tableID = c("B01001","B02001"),
                                        year=year,
                                        test=TRUE)
  
  expect_contains(profileHelperObject,c("B01001_001","B02001_002"))
})
