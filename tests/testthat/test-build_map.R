
# Similuate mapper() data preparation
    spatial_mapDF <- spatial_mapDF %>% mutate(value = formattable::percent(as.numeric(tot_pop_pct)),digits=2)
    spatial_mapDF <- spatial_mapDF %>% mutate(value_print = formattable::percent(value,digits=2))


test_that("build_map() works with supplied mapDF", {
  expect_identical(class(build_map(mapDF=spatial_mapDF,
            filterAddress=NULL,
            ggrObject=ggrObject,
            markers=markers,
            dispRoads=TRUE,
            dispWater=TRUE,
            dispPlaces=TRUE,
            dispRails=FALSE, 
            LegendTitle="Census Tracts",
            MapTitle="Selected Census Tracts",
            i=FALSE,
            radiusOnly=FALSE,
            alpha=0.2,
            areaOnly=FALSE,
            geography="tract",
            verbose=verbose,
            year = NULL,
            st = NULL)),"tmap")
})
