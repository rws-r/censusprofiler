
#' Map Locations from Address List
#' 
#' This function takes a vector or dataframe containing addresses and 
#' geocodes them, and plots them. 
#'
#' @param addressList Either a vector with addresses listed, or a dataframe needing formatting.
#' @param state Parameter to filter by state.
#' @param county Parameter to filter by county.
#' @param geosObject Optional geosObject to speed up processing time.
#' @param verbose Logical param to provide feedback.
#'
#' @return TMAP map plot
#' @export
#'
#' @import tmap
#'
#' @examples \dontrun{
#' map_locations(addressList)
#' map_locations(addressList,state="IL",county=43)
#' }
map_locations <- function(addressList,
                          state = NULL,
                          county = NULL,
                          geosObject = NULL,
                          verbose = FALSE){
  
  ## Deal with "no visible binding for global variable" error 
  NAME <- STUSPS <- STATEFP <- NAMELSAD <- COUNTYFP <- NULL

if(class(addressList)[1]!="sf"){
  message("Data is not a valid sf object. Run geocoder_batch() to save coordaintes.")
  stop()
}
  
  # Secure geosObjects
  if(is.null(geosObject)){
    geosObject <- geo_var_builder(
      geography=c("state","county"),
      try="local",
      geosObject=geosObject,
      verbose=verbose
    )}
  
  geo_states <- geosObject$geo_states
  geo_counties <- geosObject$geo_counties
 
  if(!is.null(state)){
    stf <- geo_states %>% filter(NAME==state | STUSPS==state | STATEFP==state)
    addressList <- st_filter(addressList,stf)
  }

  if(!is.null(county)){
    stf <- geo_counties %>% filter(NAME==county | NAMELSAD==county | COUNTYFP==county)
    addressList <- st_filter(addressList,stf)
  }
  
  geo_states <- geo_states %>% filter(STATEFP <= 56)
  geo_states <- st_transform(geo_states,2163)

  ##TODO make insets for alaska and Hawaii
  tm_basemap(server=providers$Stadia.StamenTonerLite) +
    tm_shape(geo_states) +
     tm_polygons() + 
    tm_shape(addressList) +
    tm_dots() 
}
