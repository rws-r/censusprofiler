test_that("Utility: Create Comparison Data does comparison calls (state / US) on variable lists.", {
  
  tableID <- "B01001"
  variables <- c("B01001_001","B01001_002","B01001_026")
  
  states <-  create_comparison_data(geography="state",
                                    year=2022,
                                    tableID=tableID,
                                    variables=variables,
                                    geosObject=geos,
                                    censusVars=CV,
                                    test=TRUE,
                                    verbose=verbose)
  
  us <-  create_comparison_data(geography="us",
                                year=2022,
                                tableID=tableID,
                                variables=variables,
                                geosObject=geos,
                                censusVars=CV,
                                test=TRUE,
                                verbose=verbose)
  
  expect_s3_class(states,"data.frame")
  expect_contains(names(states),c('table_id','year','variable','concept','labels','estimate','moe','subtotals','pct','tot_pop','tot_pop_pct','moe_pct','subtotals_by_type','pct_by_type','moe_pct_by_type','name','state','geoid','type_base','geography','calculation','type','varID','dt'))
  expect_equal(length(unique(states$geoid)),1)
  expect_s3_class(us,"data.frame")
  expect_contains(names(us),c('table_id','year','variable','concept','labels','estimate','moe','subtotals','pct','tot_pop','tot_pop_pct','moe_pct','subtotals_by_type','pct_by_type','moe_pct_by_type','name','us','geoid','type_base','geography','calculation','type','varID','dt'))
  expect_equal(length(unique(us$geoid)),1)
  
})

