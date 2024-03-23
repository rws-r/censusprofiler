test_that("get_vre_table() works", {
  x <- get_vre_table(data=data$data$type1data,
                     year=2021,
                     geography="tract",
                     tableID = "B02001",
                     variableList = "B02001_003",
                     state=17,
                     county=043)
  
  expect_contains(names(x),c("variable","variance","se","moe"))
})
