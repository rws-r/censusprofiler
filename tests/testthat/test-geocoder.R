test_that("Geocoder geocodes.", {
  
  expect_s3_class(geocoder("350 Fifth Avenue New York, NY 10118",
                           year=2020,
                           service="OSM"),"sf")
  expect_s3_class(geocoder("350 Fifth Avenue New York, NY 10118",
                           year=2020,
                           service="census"),"sf")
  expect_s3_class(geocoder("350 Fifth Avenue New York, NY 10118",
                           year=2020,
                           service="tryall"),"sf")
  expect_error(geocoder("350 Fifth Avenue New York, NY 10118",
                        year=2020,
                        service="something"))
  
})
