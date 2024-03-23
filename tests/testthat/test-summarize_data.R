test_that("summarize_data() works", {
  x <- summarize_data(data$data$type1data,
                      geography="tract",
                      filterRadius = 1)
  y <- summarize_data(data$data$type2data,
                      geography="tract",
                      filterRadius = 1)
  
  expect_s3_class(x,"data.frame")
  expect_s3_class(y,"data.frame")
})
