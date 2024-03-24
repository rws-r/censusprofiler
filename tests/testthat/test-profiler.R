
test_that("profiler() constructs a single tableID call properly", {
  ## Single tableID call 
  p1 <- profiler(name="TestProfile",
                 year=year,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=CV,   
                 tableID="B02001",   
                 variables=NULL,
                 geography="tract",
                 filterAddress=address,
                 filterRadius=1,
                 filterSummaryLevels="root",
                 state=NULL,
                 county=NULL,
                 tract=NULL,
                 block_group=NULL,
                 ggr=NULL,
                 geosObject=geos,
                 verbose=FALSE,
                 st=NULL)
  
  expect_type(p1,"list")
  expect_type(p1$info,"list")
  expect_type(p1$data,"list")
  expect_equal(names(p1$data),c("type1data","type2data","type3data","type4data"))
  expect_s3_class(p1$data$type1data,"data.frame")
  expect_s3_class(p1$data$type2data,"data.frame")
  expect_s3_class(p1$data$type3data,"data.frame")
  expect_s3_class(p1$data$type4data,"data.frame")
  
})

test_that("profiler() constructs a multi-tableID call properly", {
  p2 <- profiler(name="TestProfile",
                 year=year,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=CV,  
                 tableID=c("B01001","B02001"),   
                 variables=NULL,
                 geography="tract",
                 filterAddress=address,
                 filterRadius=1,
                 filterSummaryLevels="root",
                 state=NULL,
                 county=NULL,
                 tract=NULL,
                 block_group=NULL,
                 ggr=NULL,
                 geosObject=geos,
                 verbose=FALSE,
                 st=NULL)
  
  expect_type(p2,"list")
  expect_type(p2$info,"list")
  expect_type(p2$data,"list")
  expect_equal(names(p2$data),c("type1data","type2data","type3data","type4data"))
  expect_s3_class(p2$data$type1data,"data.frame")
  expect_s3_class(p2$data$type2data,"data.frame")
  expect_s3_class(p2$data$type3data,"data.frame")
  expect_s3_class(p2$data$type4data,"data.frame")
})

test_that("profiler() constructs an area (non radius) call properly", {
  p3 <- profiler(name="TestProfile",
                 year=year,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=CV,   
                 tableID="B02001",   
                 variables=NULL,
                 geography="state",
                 filterAddress=NULL,
                 filterRadius=NULL,
                 filterSummaryLevels="root",
                 state=17,
                 county=NULL,
                 tract=NULL,
                 block_group=NULL,
                 ggr=NULL,
                 geosObject=geos,
                 verbose=FALSE,
                 st=NULL)
  
  expect_type(p3,"list")
  expect_type(p3$info,"list")
  expect_type(p3$data,"list")
  expect_equal(names(p3$data),c("type1data","type2data","type3data","type4data"))
  expect_s3_class(p3$data$type1data,"data.frame")
  expect_s3_class(p3$data$type2data,"data.frame")
  expect_s3_class(p3$data$type3data,"data.frame")
  expect_s3_class(p3$data$type4data,"data.frame")
})

test_that("profiler() constructs an area (non radius) call properly (county)", {
  p4 <- profiler(name="TestProfile",
                 year=year,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=CV,   
                 tableID="B02001",   
                 variables=NULL,
                 geography="county",
                 filterAddress=NULL,
                 filterRadius=NULL,
                 filterSummaryLevels="root",
                 state=17,
                 county=043,
                 tract=NULL,
                 block_group=NULL,
                 ggr=NULL,
                 geosObject=geos,
                 verbose=FALSE,
                 st=NULL)
  
  expect_type(p4,"list")
  expect_type(p4$info,"list")
  expect_type(p4$data,"list")
  expect_equal(names(p4$data),c("type1data","type2data","type3data","type4data"))
  expect_s3_class(p4$data$type1data,"data.frame")
  expect_s3_class(p4$data$type2data,"data.frame")
  expect_s3_class(p4$data$type3data,"data.frame")
  expect_s3_class(p4$data$type4data,"data.frame")
})

test_that("profiler() constructs an area (non radius) call properly (tract)", {
  p5 <- profiler(name="TestProfile",
                 year=year,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=CV,  
                 tableID="B02001",   
                 variables=NULL,
                 geography="tract",
                 filterAddress=NULL,
                 filterRadius=NULL,
                 filterSummaryLevels="root",
                 state=17,
                 county=043,
                 tract=846509,
                 block_group=NULL,
                 ggr=NULL,
                 geosObject=geos,
                 verbose=FALSE,
                 st=NULL)
  
  expect_type(p5,"list")
  expect_type(p5$info,"list")
  expect_type(p5$data,"list")
  expect_equal(names(p5$data),c("type1data","type2data","type3data","type4data"))
  expect_s3_class(p5$data$type1data,"data.frame")
  expect_s3_class(p5$data$type2data,"data.frame")
  expect_s3_class(p5$data$type3data,"data.frame")
  expect_s3_class(p5$data$type4data,"data.frame")
})


