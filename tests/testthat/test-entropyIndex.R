test_that("entropyIndex() works with a no data call", {
  
  EI <- entropyIndex(data=NULL,
                     dataType="long", 
                     geography="tract",
                     wideCols=NULL,
                     longCol="pct",
                     tableID="B02001",
                     variables=NULL,
                     filterAddress=address,
                     filterRadius=1,
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     year=2022,
                     return=FALSE,
                     censusVars=CV,
                     verbose=verbose)
  
  expect_identical(names(EI),c("UnitEntropyScore","MaxEntropy","AreaSummary","AreaEntropyScore","AreaStandardizedEntropyScore","AreaEntropyIndex"))
  
})

test_that("entropyIndex() works with supplied data", {
  
  EI <- entropyIndex(data=data,
                     dataType="long", 
                     geography="tract",
                     wideCols=NULL,
                     longCol="pct",
                     tableID="B02001",
                     variables=NULL,
                     filterAddress=NULL,
                     filterRadius=NULL,
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     year=2022,
                     return=FALSE,
                     censusVars=CV,
                     verbose=verbose)
  
  expect_identical(names(EI),c("UnitEntropyScore","MaxEntropy","AreaSummary","AreaEntropyScore","AreaStandardizedEntropyScore","AreaEntropyIndex"))
})

test_that("entropyIndex() works with supplied data, dissimilarity index", {
  
  EI <- entropyIndex(data=data,
                     dataType="long", 
                     geography="tract",
                     wideCols=NULL,
                     longCol="pct",
                     tableID="B02001",
                     dissimilarityValue = 3,
                     variables=NULL,
                     filterAddress=NULL,
                     filterRadius=NULL,
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     year=2022,
                     return=FALSE,
                     censusVars=CV,
                     verbose=verbose)
  
  expect_identical(names(EI),c("UnitEntropyScore","MaxEntropy","AreaSummary","AreaEntropyScore","AreaStandardizedEntropyScore","AreaEntropyIndex","DissimilarityIndex"))
})

test_that("entropyIndex() works with supplied data, dissimilarity index + isolation/exposure indices", {
  
  EI <- entropyIndex(data=data,
                     dataType="long", 
                     geography="tract",
                     wideCols=NULL,
                     longCol="pct",
                     tableID="B02001",
                     dissimilarityValue = 3,
                     dissimilarityValueB = 1,
                     variables=NULL,
                     filterAddress=NULL,
                     filterRadius=NULL,
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     year=2022,
                     return=FALSE,
                     censusVars=CV,
                     verbose=verbose)
  
  expect_identical(names(EI),c("UnitEntropyScore","MaxEntropy","AreaSummary","AreaEntropyScore","AreaStandardizedEntropyScore","AreaEntropyIndex","DissimilarityIndex","ExposureIndex","IsolationIndex"))
})
