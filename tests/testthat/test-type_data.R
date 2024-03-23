test_that("type_data() works", {
  
    expect_identical(type_data(data$data$type1data),1)
    expect_identical(type_data(data$data$type2data),2)
    expect_identical(type_data(data$data$type3data),3)
    expect_identical(type_data(data$data$type4data),4)
    expect_identical(type_data(data),5)
    
  })

