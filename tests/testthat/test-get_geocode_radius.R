test_that("get_geocode_radius() works", {
    #Get full ACS dataset
    full_call <- get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
                                    filterRadius=1,geography="tract",geosObject = geos, test=TRUE)
    
    #Get the radius only.
    radius_only_call <- get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
                                           filterRadius=1,radiusOnly=TRUE,geosObject = geos,test=TRUE,geography="tract")
    
    #Get a simple geocode of an address.
    geocode_only_call <- get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
                                            filterRadius=1,geocodeOnly=TRUE,geosObject = geos,test=TRUE, geography="tract")
    
    expect_type(full_call,"list")
    expect_s3_class(full_call$df,c("sf","data.frame"))
    expect_s3_class(full_call$buffer,c("sf","data.frame"))
    expect_s3_class(full_call$coordinates,c("sf","data.frame"))
    
    expect_s3_class(radius_only_call,c("sf","data.frame"))
    
    expect_s3_class(geocode_only_call,c("sf","data.frame"))
    
  })