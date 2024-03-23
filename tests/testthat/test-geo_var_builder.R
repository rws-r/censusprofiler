test_that("geo_var_builder() works", {
  
  x <- geo_var_builder(geography=c("all"),
                  try="local",
                  state=NULL,
                  county=NULL,
                  geosObject=NULL,
                  verbose=FALSE,
                  test=FALSE)
  
  expect_type(x,"list")
  expect_length(x,4)
  expect_identical(names(x),c("geo_states","geo_counties","geo_tracts","geo_blocks"))
  expect_s3_class(x$geo_states,c("sf","data.frame"))
  expect_s3_class(x$geo_counties,c("sf","data.frame"))
  expect_s3_class(x$geo_tracts,c("sf","data.frame"))
  expect_s3_class(x$geo_blocks,c("sf","data.frame"))
})

test_that("geo_var_builder() sends message on badly formed geography: states", {
  
  expect_warning(geo_var_builder(geography="states",
                  try="local",
                  state=NULL,
                  county=NULL,
                  geosObject=NULL,
                  verbose=FALSE,
                  test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: counties", {
  
  expect_warning(geo_var_builder(geography="counties",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: tracts", {
  
  expect_warning(geo_var_builder(geography="tracts",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: blocks", {
  
  expect_warning(geo_var_builder(geography="blocks",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: `block groups`", {
  
  expect_warning(geo_var_builder(geography="block groups",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: `block_groups`", {
  
  expect_warning(geo_var_builder(geography="block_groups",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: `block_group`", {
  
  expect_warning(geo_var_builder(geography="block_group",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"Geography param should be")
})

test_that("geo_var_builder() sends message on badly formed geography: `block groups`", {
  
  expect_error(geo_var_builder(geography="zinger",
                 try="local",
                 state=NULL,
                 county=NULL,
                 geosObject=NULL,
                 verbose=FALSE,
                 test=FALSE),"valid entry")
})