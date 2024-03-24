test_that("stat_table_builder() works with master_list and summary_list", {
  x <- stat_table_builder(summary_table = FALSE,
                          master_list = TRUE,
                          tableID="B02001",
                          variables = "B02001_003",
                          censusVars = CV,
                          test=TRUE,
                          stateStart = 56)
 
   expect_identical(type_data(x),1)
  
  y <- stat_table_builder(data=x,
                          summary_table = TRUE,
                          master_list = FALSE,
                          tableID="B02001",
                          variables = "B02001_003",
                          censusVars = CV,
                          test=TRUE,
                          stateStart = 56)
  
  expect_contains(names(y),c("variable","sd_pct","sd_est","mean_est","mean_pct","median_est"))
})