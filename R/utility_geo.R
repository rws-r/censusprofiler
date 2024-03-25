# TODO: Go through and figure out if I can use "..." in place of internal params in ALL FUNCs.
# TODO: Eliminate unnecesary params in ALL FUNCs.

# get_geocode_radius-----------------------------------
#' Get a Geocoded Radius
#'
#' A function to geocode a supplied address, and then return a specified list
#' with various coordinate information.
#'
#' Designed to work with censusprofiler functions, and using OpenStreetMap /
#' US Census data, supplied addresses are geocoded. Resulting coordinates are
#' processed, including radius buffers. Parameters are returned to the
#' originating function, where capi() is called. Alternatively, it can be used
#' to generate simply a geocoded address, or a geospatial radius.
#'
#' @param filterAddress Address for geocoding. Should be in "Street, City, State Zip"
#' format if possible. Names are also optional, but consistency of lookup is not
#' guaranteed.
#' @param filterRadius An integer describing the width of the radius in miles.
#' @param filterByGeoType An irregular geo type to get a smaller overlapping set
#'   of tracts, block_groups or other geography from. Options are currently
#'   "metro", "place","combined_statistical_areas". E.g., Find all tracts in Chicago (place).
#' @param filterByGeoValue A value to find object for filtering. Either NAME or GEOID.   
#' @param geography Typically passed by other functions, used for capi()
#' request to specify geography. May be either "us","state", "county","tract", or "block group".
#' @param radiusOnly Set to TRUE to return geocoded radius only, and not
#' provide any additional ACS data requests.
#' @param geocodeOnly Set to TRUE to return the geocode of the address only
#' as an object.
#' @param coords Internal: passing already found coordinates to function.
#' @param intersectOverlap A proportion of minimum required overlap from radius
#' and unit area.
#' @param test Internal: Parameter to allow testing passthroughs [depreciated]
#' @param verbose Internal: Parameter for printing feedback.
#' @param geosObject Optional geosObject object to speed up geo processing.
#' @param fipsOnly Logical param to return only FIPS from radius.
#' @param state Input (abb. or FIPS) of state for search.
#' @param county Input (abb. or FIPS) of county for search.
#' @param year Year for data call
#' @param geoidLookup Lookup geography by GEOID.
#' @param profile A profile data object.
#'
#' @return Returns either 1) a filtered sf object from capi() call; 2) an sf
#' object with point coordinates; 3) an sf object with radius coordinates;
#' or 4) a tmap map with radius overlay.
#'
#' @importFrom tmaptools geocode_OSM
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tigris states
#' @importFrom tigris counties
#' @importFrom tigris tracts
#' @importFrom sf st_buffer
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_filter
#' @importFrom sf st_intersects
#' @importFrom sf st_intersection
#' @importFrom sf st_transform
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_centroid
#' @importFrom sf st_coordinates
#' @importFrom sf st_area
#' @importFrom units set_units
#' @importFrom stringr str_trim
#'
#' @examples
#' \dontrun{
#' #Get full ACS dataset
#' get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
#' filterRadius=1,geometry=TRUE)
#'
#' #Get the radius only.
#' get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
#' filterRadius=1,radiusOnly=TRUE)
#'
#' #Get a simple geocode of an address.
#' get_geocode_radius(filterAddress="350 Fifth Avenue New York, NY 10118",
#' filterRadius=1,geocodeOnly=TRUE)
#' }
get_geocode_radius <- function(filterAddress=NULL,
                               filterRadius=NULL,
                               filterByGeoType=NULL,
                               filterByGeoValue=NULL,
                               state=NULL,
                               county=NULL,
                               geoidLookup=NULL,
                               geography=NULL,
                               radiusOnly=FALSE,
                               geocodeOnly=FALSE,
                               fipsOnly=FALSE,
                               profile=FALSE,
                               coords=NULL,
                               intersectOverlap=0.1,
                               year=NULL,
                               test=FALSE,
                               verbose=FALSE,
                               geosObject = NULL
){
  
  ## Deal with "no visible binding for global variable" error 
  feet <- STATEFP <- STUSPS <- NAME <- NAME10 <- COUNTYFP <- GEOID <- 
    GEOID10 <- intersectedArea <- unitArea <- intersectionProp <- 
    NAMELSAD <- geometry <- NULL
  
  if(is.null(geography)){
    ##ADDTEST
    stop("!> get_geocode_radius() requires a geography parameter.")
  }
  
  if(!(geography %in% c("us","state","county","tract","block group"))){
    if(str_detect(geography,"us")){
      rightgeo <- "us"
    }else if(str_detect(geography,"state")){
      rightgeo <- "state"
    }else if(str_detect(geography,"county")){
      rightgeo <- "county"
    }else if(str_detect(geography,"tract")){
      rightgeo <- "tract"
    }else if(str_detect(geography,"block")){
      rightgeo <- "block group"
    }else{
      rightgeo <- NA
    }
    if(is.na(rightgeo)){
      stop(paste("!> Malformed geography. Do not recognize '",geography,"'.",sep=""))
    }else{
      stop(paste("!> Malformed geography. Entered '",geography,"'. Did you mean '",rightgeo,"'?",sep=""))
    }
  }
  
  if(geography=="state" && !is.null(county)){
    stop("!> A state-level geography does not require a county. Remove and try again.")
  }
  if(geography=="us"){
    stop("!> get_geocode_radius() does not work on US-level geographies currently.")
  }
  if(geography=="us" && (!is.null(county) | !is.null(state))){
    stop("!> A US-level geography does not require a state or county. Remove and try again.")
  }
  
  st <- Sys.time()
  if(!is.null(filterAddress) && is.null(filterRadius)){
    stop("WHOOPS. You forgot to include the radius value. Please submit again.")
  }
  
  ## Did I pass geos? If not, find them.
  if(is.null(geosObject)){
    geosObject <- geo_var_builder(geography=c("state","county",geography),
                                  try="local",
                                  state=state,
                                  county=county,
                                  geosObject=geosObject,
                                  verbose=verbose,
                                  test=test)}

  if(!is.null(geosObject$geo_states))geo_states <- geosObject$geo_states
  if(!is.null(geosObject$geo_counties))geo_counties <- geosObject$geo_counties
  if(!is.null(geosObject$geo_tracts))geo_tracts <- geosObject$geo_tracts
  if(!is.null(geosObject$geo_blocks))geo_blocks <- geosObject$geo_blocks    
  
  ## Check for class mismatches.
  if(!is.null(state)){
    if(inherits(geo_states$STATEFP,"character")==TRUE || 
       inherits(geo_counties$STATEFP,"character")==TRUE || 
       inherits(geo_tracts$STATEFP,"character")==TRUE || 
       inherits(geo_blocks$STATEFP,"character")==TRUE && 
       is.numeric(state)){
      state <- as.character(state)
    }
  }
  if(!is.null(county)){
    ctymerge <- NULL
    for(i in 1:length(county)){
      print(is.numeric(county))
      if((inherits(geo_counties$COUNTYFP,"character")==TRUE || 
         inherits(geo_tracts$COUNTYFP,"character")==TRUE ||   
         inherits(geo_blocks$COUNTYFP,"character")==TRUE) && 
         is.numeric(county[i])){
           ctymerge <- c(ctymerge,sprintf("%03d",county[i]))
      }
    }
    county <- ctymerge
  }

  if(verbose==TRUE)message(paste("    -ggr--Fetching geos took ",round(difftime(Sys.time(),st,units = "sec"),2),"  speed up by saving to env."))
  st <- Sys.time()
  
  
  # if(verbose==TRUE)message(paste("    -ggr--Stacking geos took ",round(difftime(Sys.time(),st,units = "sec"),2)))
  # 
  # st <- Sys.time()
  
  if(!is.null(filterAddress) | !is.null(coords)){ 
    # Set the radius around the filtered address.
    if(is.null(coords)){
      coords <- geocoder(filterAddress,service="tryall")
      #  coords <- data.frame(long=coords$coords[[1]],lat=coords$coords[[2]])
      
      if(verbose==TRUE)message(paste("    -ggr--Fetching geocode for filter address took ",round(difftime(Sys.time(),st,units = "sec"),2)))
      st <- Sys.time()
      
    }else{
      if("sf" %in% class(coords)==TRUE){ # Account for sf
        coords <- sf::st_coordinates(coords)
      }
      coords <- data.frame(long=coords[[1]],lat=coords[[2]])
      coords <- sf::st_as_sf(coords,coords=c("long","lat"))
      coords <- coords %>% sf::st_set_crs(4326)
      
      if(verbose==TRUE)message(paste("    -ggr--Processing coords took ",round(difftime(Sys.time(),st,units = "sec"),2)))
      st <- Sys.time()
    }
    
    buffer <- sf::st_buffer(coords,units::set_units(filterRadius*5280,feet))
    
    if(verbose==TRUE)message(paste("    -ggr--Setting radius took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    
    st <- Sys.time()
    ## Get state from filterAddress / coords
    #statefp <- sf::st_filter(geo_states, coords, .predicate = sf::st_contains)$STATEFP
    statefp <- sf::st_filter(geo_states, buffer, .predicate = sf::st_intersects)$STATEFP
    #state <- as.numeric(state)
    if(geography=="state"){
      countyfp <- NULL
    }else{
      geo_counties <- geo_counties %>% filter(STATEFP %in% statefp)
      #countyfp <- sf::st_filter(geo_counties, coords, .predicate = sf::st_contains)$COUNTYFP
      countyfp <- sf::st_filter(geo_counties, buffer, .predicate = sf::st_intersects)$COUNTYFP
      #county <- as.numeric(county)
    }
  }else if(!is.null(filterByGeoType)){
    message(paste(dur(st),"Finding geos by supplied geo object..."))
    if(is.null(filterByGeoValue)){
      stop("Error in filterByGeoType: must supply value for filtering.")
    }
    if(filterByGeoType %in% c("urban","urban_areas","urban areas")){
      geofilterset <- tigris::urban_areas(year=year)
      gname <- geofilterset %>% filter(NAME10==filterByGeoValue)
      # If first match fails, try fuzzy matching. This may result in multiple matches.
      if(nrow(gname)==0)gname <- geofilterset %>% filter(grepl(filterByGeoValue,NAME10,ignore.case=TRUE))
      if(nrow(gname)==0)ggeoid <- geofilterset %>% filter(grepl(filterByGeoValue,GEOID10,ignore.case=TRUE))
    }else if(filterByGeoType %in% c("place","places")){
      if(is.null(state)){
        stop("Sorry, to select places designation, you have to provide a state value.")
      }
      geofilterset <- tigris::places(year=year,state=state)
      gname <- geofilterset %>% filter(NAME==filterByGeoValue)
      # If first match fails, try fuzzy matching. This may result in multiple matches.
     # if(nrow(gname)==0)gname <- geofilterset %>% filter(grepl(filterByGeoValue,NAME,ignore.case=TRUE))
      if(nrow(gname)==0)ggeoid <- geofilterset %>% filter(grepl(filterByGeoValue,GEOID,ignore.case=TRUE))
    }else if(filterByGeoType %in% c("combined statistical areas","combined_statistical_areas","csa")){
      ## There's a problem with tigris::combined... For year 2022. Hence the manual adj.
      geofilterset <- tigris::combined_statistical_areas(year=2021)
      gname <- geofilterset %>% filter(NAME==filterByGeoValue)
      # If first match fails, try fuzzy matching. This may result in multiple matches.
      if(nrow(gname)==0)gname <- geofilterset %>% filter(grepl(filterByGeoValue,NAME,ignore.case=TRUE))
      if(nrow(gname)==0)ggeoid <- geofilterset %>% filter(grepl(filterByGeoValue,GEOID,ignore.case=TRUE))
    }else{
      stop("Invalid filterByGeoType selection. Currently accepting 'metro', 'place','combined_statistical_areas'.")
    }
    
    if(nrow(gname)>0){
      if(nrow(gname)>1){
        stop("Multiple matches. Please try again with GEOID for greater specificity.")
      }
      geofiltered <- gname
    }else if(nrow(ggeoid)>0){
      geofiltered <- ggeoid
    }else{
      stop("No matches for filterByGeo.")
    }
    
    buffer <- geofiltered$geometry
    buffer <- buffer %>% sf::st_transform('+proj=longlat +datum=WGS84')

    if(verbose==TRUE)message(paste("    -ggr--Setting radius took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    
    st <- Sys.time()
    ## Get state from filterAddress / coords
    statefp <- sf::st_filter(geo_states, buffer, .predicate = sf::st_intersects)$STATEFP
    if(geography=="state"){
      countyfp <- NULL
    }else{
      geo_counties <- geo_counties %>% filter(STATEFP %in% statefp)
      countyfp <- sf::st_filter(geo_counties, buffer, .predicate = sf::st_intersects)$COUNTYFP
    }
    
  }else{
    buffer <- NULL
    if(!is.null(state)){
      states <- geo_states %>% filter(STATEFP==state | STUSPS==state | NAME==state)
      statefp <- states$STATEFP
    }else{
      statefp <- NULL
    }
    if(!is.null(county)){
      if(!is.null(statefp)){
        #geo_counties <- geo_counties %>% filter(STATEFP==statefp)
        geo_counties <- geo_counties %>% 
          filter(STATEFP==statefp & 
                   (COUNTYFP %in% county | NAME %in% county | NAMELSAD %in% county))
        countyfp <- geo_counties$COUNTYFP
      }else{
        countyfp <- NULL
        stop("!> No state supplied. County will therefore be ambiguous.")
      }
    }else{
      countyfp <- NULL
    }

    if(!is.null(geoidLookup)){
      glength <- nchar(as.character(geoidLookup))
    
      if(glength==2){
        G <- geo_states
      }else if(glength>2 && glength<=5){
        G <- geo_counties
      }else if(glength>=10 && glength<=11){
        G <- geo_tracts
      }else if(glength>11){
        G <- geo_blocks
      }else{
        stop("!> Invalid geoidLookup designation. NB: Geoid lookups require the full geoid (state+county+tract, etc)")
      }
      G <- G %>% filter(GEOID==geoidLookup)
      if(nrow(G)==0){
        stop("!> Invalid geoidLookup designation. Is there a typo? NB: Geoid lookups require the full geoid (state+county+tract, etc)")
      }

    }
    
    if(is.null(state) && is.null(county) && is.null(geoidLookup))stop("No geographic specifications given.")
  }

  if(verbose==TRUE)message(paste("    -ggr--Parsing addy took ",round(difftime(Sys.time(),st,units = "sec"),2)))
  
  st <- Sys.time()
  
  if(geocodeOnly==TRUE){
    return(coords)
  }else if(radiusOnly==TRUE){
    return(buffer)
  }else if(!is.null(geoidLookup)){
    if(is.data.frame(G)){
      df <- list(df = G,
                 geoid = G$GEOID,
                 states = G$STATEFP)
      if(geography %in% c("county","tract","block group")){
        df <- append(df,list(counties = G$COUNTYFP))
      }
      return(df)
    }else{
      stop("!> There was an error in geoidLookup.")
    }
  }else{
    
    if(verbose==TRUE)message(paste("    -ggr--Filtering by state and county took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    st <- Sys.time()
    
    # Filter tract list to make intersection less laborious.
    if(geography=="tract"){
      CT <- geo_tracts 
    }else if(geography=="block group"){
      CT <- geo_blocks
    }else if(geography=="county"){
      CT <- geo_counties
    }else{
      CT <- geo_states
    }
    if(!is.null(statefp)){
      CT <- CT %>% filter(STATEFP == statefp)
    }
    if(!is.null(countyfp)){
      CT <- CT %>% filter(COUNTYFP %in% countyfp)
    }

    if(verbose==TRUE)message(paste("    -ggr--Filtering tracts took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    st <- Sys.time()
    CT <- CT %>% sf::st_transform('+proj=longlat +datum=WGS84')
    if(!is.null(filterAddress) | !is.null(coords) | !is.null(filterByGeoType)){
      CT <- sf::st_filter(CT,buffer, .predicate = sf::st_intersects)
      
      if(!is.null(filterAddress)){
        ## For this section, I want to keep overlap of units that meet a certain
        ## criteria—encoded in the parameter intersectOverlap.
        # Find area of intersection
        CT <- CT %>% mutate(unitArea = sf::st_area(geometry))
        suppressWarnings( # Fussy about telling me about spatial constants.
          CT <- CT %>% mutate(intersectedArea = sf::st_area(sf::st_intersection(CT,buffer)))
        )
        CT <- CT %>% mutate(intersectionProp = as.numeric(intersectedArea/unitArea))
        
        if(verbose==TRUE)message(paste("    -ggr--Mutating intersect overlap took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    
    st <- Sys.time()
    
    # Filter by minimum overlap, but account for instances when census tract
    # is very large, beyond the radius, or when all overlaps are small, below
    # the threshhold.
    if(nrow(CT)>1){
      CTfilt <- CT %>% filter(intersectionProp>=intersectOverlap)
      if(nrow(CTfilt)>0){
        CT <- CTfilt
      }
    }
    
    if(verbose==TRUE)message(paste("    -ggr--Filtering minimum overlap took ",round(difftime(Sys.time(),st,units = "sec"),2)))
      }
    }else{
      CT <- CT %>% mutate(unitArea = sf::st_area(geometry))
    }
    
    st <- Sys.time()
    
    # Get a list of counties and states in filtered list.
    CTng <- sf::st_drop_geometry(CT)
    states <- unique(CTng$STATEFP)
    if(geography!="state"){
      counties <- unique(CTng$COUNTYFP)
    }else{
      counties <- NULL
    }
    
    if(fipsOnly==TRUE){
      df <- list(
        geoid = CT$GEOID,
        states = states,
        counties = counties
      )
      if(geography=="tract"){
        df <- append(df,list(tracts = CT$TRACTCE))
      }else if(geography=="block group"){
        df <- append(df,list(tracts = CT$TRACTCE,block_groups = CT$BLKGRPCE))
      }else{
        #df <- append(df,list(geoid = CT$GEOID))
      }
    }else if(profile==TRUE){
      df <- list(geoid = CT$GEOID,
                 states = states,
                 counties = counties)
      if(geography=="tract"){
        df <- append(df,list(tracts = CT$TRACTCE))
      }else if(geography=="block group"){
        df <- append(df,list(tracts = CT$TRACTCE,block_groups = CT$BLKGRPCE))
      }else{
      }
      df <- append(df,list(buffer = buffer))
      df <- append(df,list(coordinates = coords))
    }else if(is.null(filterAddress)){
      df <- list(df = CT,
                 geoid = CT$GEOID,
                 states = states,
                 counties = counties,
                 coordinates = coords)      
    }else{
      df <- list(df = CT,
                 geoid = CT$GEOID,
                 states = states,
                 counties = counties,
                 buffer = buffer,
                 coordinates = coords)
    }
    
    if(verbose==TRUE)message(paste("    -ggr--Df construction took ",round(difftime(Sys.time(),st,units = "sec"),2)))
    st <- Sys.time()
    
    return(df)
    
  }
}

# geo_road_helper-----------------------------------

#' Geo: Road Helper
#' 
#' A geo utility to assist filtering roads which do not meet certain criteria, 
#' to avoid bogging down tmap "plot" maps with too much text. 
#' 
#' First, strips out all directional pre/suffixes. Then, returns a modified
#' dataframe with added suffix column. Can use this to filter later.
#' 
#' @param df The road sf object.
#' @param verbose A logical parameter to specify verbose output.
#'
#' @return A dataframe with "suffix" column added, for filtering use later.
#' 
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @examples
#' \dontrun{
#'   roads <- tigris::roads(state=geo$states,county = geo$counties,year=2021,filter_by=bbox)
#'   roads <- geo_road_helper(roads)
#'   }
geo_road_helper <- function(df=NULL,
                            verbose=FALSE){

  if(verbose==TRUE)pb <- txtProgressBar(min = 1, max = nrow(df), style = 3)
  for(i in 1:nrow(df)){
    ## Strip away directional pre/suffix. Then capture the street suffix.
    strName <- df[i,]$FULLNAME
    strName <- strsplit(strName," ")
    strName <- unlist(strName)
    if(strName[max(length(strName))] %in% c("S","SW","W","NW","N","NE","E","SE")==TRUE){
      sfx <- strName[max(length(strName))-1]
    }else{
      sfx <- strName[max(length(strName))]
    }
    df[i,"suffix"]=sfx
    if(verbose==TRUE)setTxtProgressBar(pb, i)
  }
  if(verbose==TRUE)close(pb)
  return(df)
}

# geo_marker_builder -------------------------------

## Logic to create markers and test for passthrough objects.
geo_marker_builder <- function(obj=NULL,
                               dispRoads=TRUE,
                               dispWater=TRUE,
                               dispPlaces=TRUE,
                               dispRails=TRUE,
                               geo_resolution=2,
                               ggrObject=NULL,
                               year=NULL,
                               verbose=FALSE,
                               st=NULL){
  
  bbox <- RTTYP <- suffix <- NULL

  
  if(is.null(ggrObject)){
    stop("A ggr object is required. If not supplied, you should be able to find it in a profile object (profile$info).")
  }

  ## Perform checks.
  if(!is.null(obj)){
    if(dispRoads==TRUE){
      if(!("roads" %in% names(obj))){
        stop("There appears to be a mismatch in your marker object and show parameters: roads not present.")
      }
    }
    if(dispWater==TRUE){
      if(!("water" %in% names(obj))){
        stop("There appears to be a mismatch in your marker object and show parameters: water not present.")
      }
    }
    if(dispPlaces==TRUE){
      if(!("places" %in% names(obj))){
        stop("There appears to be a mismatch in your marker object and show parameters: places not present.")
      }
    }
    if(dispRails==TRUE){
      if(!("rails" %in% names(obj))){
        stop("There appears to be a mismatch in your marker object and show parameters: rails not present.")
      }
    }
  }
  
  if(is.null(obj)){
    if(verbose==TRUE)message("Creating bounding box...")
    polys <- st_union(ggrObject$df$geometry)
    bbox <- st_bbox(polys)
    markerObject <- list()
    
    if(dispRoads==TRUE && geo_resolution>=1){
      if(verbose==TRUE)message(paste("Fetching tigris::roads via geo_road_helper()..."))
      if(geo_resolution==1){
        roads <- tigris::primary_secondary_roads(state=ggrObject$states,filter_by = bbox,refresh=TRUE)
      }else{
        roads <- tigris::roads(state=ggrObject$states,county = ggrObject$counties,year=year,filter_by=bbox,refresh=TRUE)
        roads <- geo_road_helper(roads)
        roads <- roads %>% filter(RTTYP %in% c("C","I","O","S","U") |
                                    suffix %in% c("Ave","Dr","Blvd","Pkwy","Expy"))
      }
      markerObject <- append(markerObject,list(roads = roads))
    }
    if(dispWater==TRUE && geo_resolution>=1){
      if(verbose==TRUE)message(paste("Fetching tigris::water..."))
      water <- tigris::area_water(state=ggrObject$states,county = ggrObject$counties,year=2021,filter_by=bbox,refresh=TRUE)
      markerObject <- append(markerObject,list(water = water))
    }
    if(dispPlaces==TRUE && geo_resolution>=1){
      if(verbose==TRUE)message(paste("Fetching tigris::places..."))
      places <- tigris::places(state=ggrObject$states,year=year,filter_by=bbox,refresh=TRUE)
      markerObject <- append(markerObject,list(places = places))
    }
    if(dispRails==TRUE && geo_resolution>=1){
      if(verbose==TRUE)message(paste("Fetching tigris::rails..."))
      rails <- tigris::rails(year=year,filter_by=bbox,refresh=TRUE)
      markerObject <- append(markerObject,list(rails = rails))
    }
  }else{
    prms <- c()
    if(dispRoads==TRUE)prms <- c(prms,"roads")
    if(dispWater==TRUE)prms <- c(prms,"water")
    if(dispPlaces==TRUE)prms <- c(prms,"places")
    if(dispRails==TRUE)prms <- c(prms,"rails")
    markerObject <- obj[prms]
  }
  return(markerObject)
}

# geo_var_builder-----------------------------------

#' Geography Variable Builder
#'
#' Checks if geospatial objects exist for referencing census data. If not, get them.
#'
#' @param geography Specifies the kind of geography to call.
#' Options: "state","county","tract","block". Defaults to "all".
#' @param try Parameter to specify whether to try local file read first, before downloading.
#' @param state Filter by county name or COUNTYFP
#' @param county Filter by state name or STATEFP
#' @param geosObject Optional geosObject object to speed up geo processing.
#' @param verbose Logical param to provide feedback.
#' @param test Logical param to bypass feedback and messages during testing.
#'
#' @return Returns global objects if does not exist.
#' 
#' @importFrom tigris states
#' @importFrom tigris counties
#' @importFrom tigris tracts
#' @importFrom tigris block_groups
#' @import tibble
#'
#' @examples
#' \dontrun{
#' geo_var_builder()
#' geo_var_builder(geography="state")
#' geo_var_builder(geography="county")
#' geo_var_builder(geography="tract")
#' geo_var_builder(geography="block")
#' }
#' 
geo_var_builder <- function(geography=c("all"),
                            try="local",
                            state=NULL,
                            county=NULL,
                            geosObject=NULL,
                            verbose=FALSE,
                            test=FALSE){
  
  ## Make all geo vars NULL
  geo_states <- geo_counties <- geo_tracts <- geo_blocks <- 
    STATEFP <- STUSPS <- COUNTYFP <- NAME <- NAMELSAD <- 
    NAMELSADCO <- NULL
  
  ## Clean up potential geography errors
  acceptable_geos <- c("us","state","county","tract","block group","all")
  for(i in 1:length(geography)){
    if(!(geography[i] %in% acceptable_geos)){
      if(geography[i]=="states"){
        warning("    -gvb--NB: Geography param should be `state` not `states`...automatically corrected.")
        geography[i] <- "state"
      }else if(geography[i]=="counties"){
        warning("    -gvb--NB: Geography param should be `county` not `counties`...automatically corrected.")
        geography[i] <- "county"
      }else if(geography[i]=="tracts"){
        warning("    -gvb--NB: Geography param should be `tract` not `tracts`...automatically corrected.")
        geography[i] <- "tract"
      }else if(geography[i]=="blocks"){
        warning("    -gvb--NB: Geography param should be `block group` not `blocks`...automatically corrected.")
        geography[i] <- "block group"
      }else if(geography[i]=="block groups"){
        warning("    -gvb--NB: Geography param should be `block group` not `block groups`...automatically corrected.")
        geography[i] <- "block group"
      }else if(geography[i]=="block_groups"){
        warning("    -gvb--NB: Geography param should be `block group` not `block_groups`...automatically corrected.")
        geography[i] <- "block group"
      }else if(geography[i]=="block_group"){
        warning("    -gvb--NB: Geography param should be `block group` not `block_group`...automatically corrected.")
        geography[i] <- "block group"
      }else if(geography[i]=="metro"){
        geography[i] <- "metro"
      }else{
        stop(paste("There is an error in your geography param. `",geography[i],"` is not a valid entry.",sep=""))
      }
    }
  }
    
  if(verbose==TRUE)message("    -gvb--Initiating geo_var_builder()...")
  
  # Check if geospatial data for various geographies exists.
  ## Did I pass geos? If not, find them.
  if(!is.null(geosObject) || exists("geos",inherits=TRUE)){
    if(!is.null(geosObject)){
      if(verbose==TRUE)message("    -gvb--Found geos passthrough....")
      geoDF <- geosObject
      local <- TRUE
    }else{
      if(verbose==TRUE)message("    -gvb--Found geos in global env....")
      geoDF <- get("geos", envir = .GlobalEnv)
      # geoDF <- "USEENV" ## Flag to read directly from memory
      local <- TRUE
    }
    # Perform filtering for a call that just needs specific data.
    #TODO Need to adapt this to capture FIPS codes only. 
    if(!is.null(state)){
      if(!is.null(geoDF$geo_states))geoDF$geo_states <- geoDF$geo_states %>% filter(if_any(c(STATEFP,STUSPS), ~ . == state)) 
      if(!is.null(geoDF$geo_counties))geoDF$geo_counties <- geoDF$geo_counties %>% filter(if_any(c(STATEFP,STUSPS), ~ . == state)) 
      if(!is.null(geoDF$geo_tracts))geoDF$geo_tracts <- geoDF$geo_tracts %>% filter(if_any(c(STATEFP,STUSPS), ~ . == state)) 
      if(!is.null(geoDF$geo_blocks))geoDF$geo_blocks <- geoDF$geo_blocks %>% filter(if_any(c(STATEFP), ~ . == state)) 
    }
    if(!is.null(county)){
      if(is.numeric(county)==TRUE){
        county <- as.character(sprintf("%03d",county))
      }
      if(!is.null(geoDF$geo_counties))geoDF$geo_counties <- geoDF$geo_counties %>% filter(if_any(c(COUNTYFP,NAME,NAMELSAD), ~ . %in% county)) 
      if(!is.null(geoDF$geo_tracts))geoDF$geo_tracts <- geoDF$geo_tracts %>% filter(if_any(c(COUNTYFP,NAMELSADCO), ~ . %in% county)) 
      if(!is.null(geoDF$geo_blocks))geoDF$geo_blocks <- geoDF$geo_blocks %>% filter(if_any(c(COUNTYFP), ~ . %in% county)) 
    }
  }else{
    if(verbose==TRUE)message("    -gvb--No geos passthrough found, starting fresh...")
    geoDF <- NULL
    local <- FALSE
    
    ## Work through list, looking locally (if flagged) then downloading.
    # Start with looking for a major file.
    # if(file.exists(file.path(getwd(),"data","geos.RDS",fsep="/"))){
    #   geoDF <- readRDS(file.path(getwd(),"data","geos.RDS",fsep="/"))
    #   if(verbose==TRUE)message("    -gvb--Found complete geos file...")
    #   return(geoDF)
    #  }else{
    
    s <- FALSE
    
    if("state" %in% geography | "all" %in% geography){
      geo_states <- NULL
      if(try=="local"){
        if(file.exists(file.path(getwd(),"data","geos.RDS",fsep="/"))){
          gs <- readRDS(file.path(getwd(),"data","geos.RDS",fsep="/"))
          geo_states <- gs$geo_states
          s <- TRUE
          if(verbose==TRUE)message("    -gvb--Found geo_states file.")
        }else{
          s <- FALSE
        }
      }
      if(s==FALSE){
        if(try=="local" && s==FALSE && verbose==TRUE)message("    -gvb--No file found for geo_states, moving to download...")
        geo_states <- states(cb=TRUE)
        geo_states <- geo_states %>% sf::st_transform('+proj=longlat +datum=WGS84')
        if(!is.null(state)){
          geo_states <- geo_states %>% filter(if_any(c(STATEFP,STUSPS), ~ . == state))
        }
        #geo_states <- tibble(geo_states = list(geo_states))
      }
      # if(!is.null(geo_states)){
      #   if(!is.null(geoDF)){
      #     geoDF <- cbind(geoDF,geo_states)
      #   }else{
      #     geoDF <- geo_states
      #   }
      # }
    }
    
    if("county" %in% geography | "all" %in% geography){
      geo_counties <- NULL
      if(try=="local"){
        if(file.exists(file.path(getwd(),"data","geos.RDS",fsep="/"))){
          gs <- readRDS(file.path(getwd(),"data","geos.RDS",fsep="/"))
          geo_counties <- gs$geo_counties
          s <- TRUE
          if(verbose==TRUE)message("    -gvb--Found geo_counties file.")
        }else{
          s <- FALSE
        }
      }
      if(s==FALSE){
        if(try=="local" && s==FALSE && verbose==TRUE)message("No file found for geo_counties, moving to download...")
        geo_counties <- counties(cb=TRUE,state=state)
        geo_counties <- geo_counties %>% sf::st_transform('+proj=longlat +datum=WGS84')
        #geo_counties <- tibble(geo_counties = list(geo_counties))
      }
      # if(!is.null(geo_counties)){
      #   if(!is.null(geoDF)){
      #     geoDF <- cbind(geoDF,geo_counties)
      #   }else{
      #     geoDF <- geo_counties
      #   }
      # }
    }
    
    if("tract" %in% geography | "all" %in% geography){
      ## Set var to NULL for testing later.
      geo_tracts <- NULL
      if(try=="local"){
        if(file.exists(file.path(getwd(),"data","geos.RDS",fsep="/"))){
          gs <- readRDS(file.path(getwd(),"data","geos.RDS",fsep="/"))
          geo_tracts <- gs$geo_tracts
          s <- TRUE
          if(verbose==TRUE)message("    -gvb--Found geo_tracts file.")
        }else{
          s <- FALSE
        }
      }
      ## If either success was a failure (local) or skipped (remains false), download.
      if(s==FALSE){
        if(try=="local" && s==FALSE && verbose==TRUE)message("No file found for geo_tracts, moving to download...")
        geo_tracts <- tracts(cb=TRUE,state=state,county=county)
        geo_tracts <- geo_tracts %>% sf::st_transform('+proj=longlat +datum=WGS84')
      }
      # if(!is.null(geo_tracts)){
      #   if(!is.null(geoDF)){
      #     geoDF <- cbind(geoDF,geo_tracts)
      #   }else{
      #     geoDF <- geo_tracts
      #   }
      # }else{
      #   stop("There was an error creating geo_tracts.")
      # }
      ## Return success flag to FALSE
      s <- FALSE
    }
    
    if("block group" %in% geography | "all" %in% geography){
      ## Set var to NULL for testing later.
      geo_blocks <- NULL
      if(try=="local"){
        if(file.exists(file.path(getwd(),"data","geos.RDS",fsep="/"))){
          gs <- readRDS(file.path(getwd(),"data","geos.RDS",fsep="/"))
          geo_blocks <- gs$geo_blocks
          s <- TRUE
          if(verbose==TRUE)message("    -gvb--Found geo_blocks file.")
        }else{
          s <- FALSE
        }
      }
      ## If either success was a failure (local) or skipped (remains false), download.
      if(s==FALSE){
        if(try=="local" && s==FALSE && verbose==TRUE)message("No file found for geo_blocks, moving to download...")
        geo_blocks <- block_groups(cb=TRUE,state = state,county = county)
        geo_blocks <- geo_blocks %>% sf::st_transform('+proj=longlat +datum=WGS84')
      }
      # if(!is.null(geo_blocks)){
      #   if(!is.null(geoDF)){
      #     geoDF <- cbind(geoDF,geo_blocks)
      #   }else{
      #     geoDF <- geo_blocks
      #   }
      # }else{
      #   stop("There was an error creating geo_blocks.")
      # }
      ## Return success flag to FALSE
      s <- FALSE
    }
    # }
  }
  
  if(verbose==TRUE)message("    -gvb--Closing geo_var_builder.")
  
  if(local==FALSE){
    geoDF <- list()
    if(!is.null(geo_states))geoDF[["geo_states"]] <- geo_states
    if(!is.null(geo_counties))geoDF[["geo_counties"]] <- geo_counties
    if(!is.null(geo_tracts))geoDF[["geo_tracts"]] <- geo_tracts
    if(!is.null(geo_blocks))geoDF[["geo_blocks"]] <- geo_blocks
  }
  
  return(geoDF)
  #on.exit(rm(geoDF))
}

# geo_census----------------------------
geo_census <- function(address,year){
  address <- str_replace(address," ","+")
  address <- URLencode(address)
  call <- paste("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=",
                address,
                "&benchmark=",
                year,
                "&format=json",sep="")
  df <- jsonlite::fromJSON(call)
  df <- df$result$addressMatches$coordinates
  if(is.null(df)){
    return(NA)
  }else{
    coords <- data.frame(long=df$x,lat=df$y)
    coords <- sf::st_as_sf(coords,coords=c("long","lat"))
    coords <- coords %>% sf::st_set_crs(4326)
    if(nrow(coords)>1)coords <- coords[1,] ## Catching a problem with returning 2 per.
    return(coords)
  }
}

# geo_OSM----------------------------
geo_OSM <- function(address,year){
  coords <- geocode_OSM(address)
  if(is.null(coords)){
    return(NA)
  }else{
    coords <- data.frame(long=coords$coords[[1]],lat=coords$coords[[2]])
    coords <- sf::st_as_sf(coords,coords=c("long","lat"))
    coords <- coords %>% sf::st_set_crs(4326)
    if(nrow(coords)>1)coords <- coords[1,] ## Catching a problem with returning 2 per.
    return(coords)
  }
}

# geocoder-----------------------------------
#' Geocoder
#'
#' Using Census Geocoder
#'
#' @param address Address entry in STREET, CITY, STATE ZIP format.
#' @param year Year to get census geocoding. See census info for year specs.
#' @param service Whether to use census geocoder or OSM.
#' @param verbose Logical param to provide feedback.
#'
#' @importFrom stringr str_replace
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @return Dataframe with coordinates.
#'
#' @examples
#' \dontrun{
#' #Using census data
#' geocoder(address="350 Fifth Avenue New York, NY 10118",year=2020,service='census')
#'
#' #Using OSM data
#' geocoder(address="350 Fifth Avenue New York, NY 10118",service='OSM')
#' }
#'
geocoder <- function(address,
                     year=2020,
                     service="census",
                     verbose=FALSE){
  
  geo <- NULL
  if(service=="census"){
    geo <- geo_census(address,year)
  }else if(service=="OSM"){
    geo <- geo_OSM(address,year)
  }else if(service=="tryall"){
    tryCatch(
      {
        geo <- geo_census(address,year)
        tryCatch(
          {
            if(is.na(geo)){
              if(verbose==TRUE)message("      No census--trying OSM geocode...")
              geo <- geo_OSM(address,year)
            }
          },
          error = function(e){
            message("Error in is.na:")
            print(e)
            stop()
          },
          warning = function(w){
            message("Warning")
            print(w)
          }
        )
      },
      error = function(e){
        message("Error in call.")
        print(e)
        tryCatch(
          {
            if(verbose==TRUE)message("      No census--trying OSM geocode...")
            geo <- geo_OSM(address,year)
          },
          error = function(e){
            message("Error in is.na:")
            print(e)
            stop()
          },
          warning = function(w){
            message("Warning")
            print(w)
          }
        )
      },
      warning = function(w){
        message("Warning:")
        print(w)
      }
    )
  }else{
    stop("You did not provide the right value for service. Either census or OSM.")
  }
  if(is.null(geo)){
    stop("Error in obtaining geocode. Check services.")
  }else{
    return(geo)
  }
}

# geocoder Batch ----------------------------- 
#' Geocoder Batch
#'
#' @param addressList List of addresses to geocode.
#' @param addressCol Name or number of address column.
#' @param start If needed, a starting ID to begin batch processing.
#' @param limit If needed, an ending ID to end batch processing.
#' @param verbose Logical param to provide feedback.
#' 
#' @importFrom stringr str_to_lower
#' 
#' @return A modified dataframe including coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' geocoder_batch(addressList)
#' geocoder_batch(addressList,start=10,limit=50)
#' }
#'
geocoder_batch <- function(addressList,
                           addressCol = "address",
                           start=NULL,
                           limit=NULL,
                           verbose=FALSE){
  
  ## Deal with "no visible binding for global variables"
  addressListID <- NULL
  
  ## Check if vector or dataframe. Modify if vector.
  if(!is.data.frame(addressList)){
   addressList <- data.frame(address = addressList)
  }
  ## Check for valid address column
  if(!(addressCol %in% names(addressList))){
    stop(paste("`",addressCol,"` does not exist in addressList. Please specify correct address column.",sep=""))
  }
  
  ## Test for existing coord columns.
  crdnms <- c("coords","coord","coordinates","coord.x","coord.y","coords.x","coords.y")
  vl <- FALSE
  for(z in 1:length(crdnms)){
    if(crdnms[z] %in% names(addressList)==TRUE){
      vl <- TRUE
    }
  }
  if("sf" %in% class(addressList)==TRUE){
    vl <- TRUE
  }
  
  if(vl==TRUE){
    user_input <- readline("You have a coordinates column already in this dataframe.
Do you want to continue, and potentially overwrite? (Y/N)")
    if(stringr::str_to_lower(user_input)!="y"){
      stop("OK, stopping.")
    }
  }
  
  if(!is.null(limit)){
    limit <- limit
  }else{
    limit <- nrow(addressList)
  }
  
  if(!is.null(start)){
    start <- start
  }else{
    start <- 1
  }
  
  # Set up temporary ID column
  addressList <- addressList %>% mutate(addressListID=1:nrow(addressList))
  df <- NULL
  counter <- 0
  counterAll <- 1
  
  # Filter primary dataset.
  if(!is.null(start)){
    addressList <- addressList[addressList$addressListID>=start,]
  }
  if(!is.null(limit)){
    addressList <- addressList[addressList$addressListID<=limit,]
  }
  
  # TODO I need to fix the limit call here. Do I want it to return a truncated list? 
  # Or is it correct to include non-geography items?
  for(i in start:limit){
    if(verbose==TRUE)message(paste("[",counterAll,"/",(limit-start+1)," -- df row ",i,"] Trying census geocode...",sep=""))
    geo <- geocoder(addressList[i,addressCol],2020,service="tryall")
    if(!is.na(geo)){
      if(verbose==TRUE)message("      Saving data...")
      geo <- geo %>% mutate(addressListID=i)
      # dfi <- data.frame(addressListID=addressList[i,"addressListID"])
      # dfi <- merge(geo,dfi)
      if(verbose==TRUE)message(paste("      Success: Updated geocodes. ",round(counterAll/(limit-start+1),3)*100,"% complete.",sep=""))
      counter <- counter + 1
      df <- rbind(df,geo)
    }else{
      if(verbose==TRUE)message("     Unavailable--")
      #dfi <- data.frame(addressListID=addressList[i,"addressListID"])
      #dfi <- merge(geo,dfi)
      if(verbose==TRUE)message(paste("     Skipping; can't get geocodes. ",round(counterAll/(limit-start+1),3)*100,"% complete.",sep=""))
    }
    counterAll <- counterAll+1
  }
  
  # Merge address list with geospatial data, and remove temporary column.
  addressList <- merge(df,addressList,all=TRUE) %>% select(-addressListID)
  
  # Set projection.
  addressList <- addressList %>% sf::st_set_crs(4326)
  if(verbose==TRUE)message(paste("[Done] Ran ",limit-start+1," queries.",
                                 counter," were successful; ",(limit-start+1)-counter," not found. ",
                                 (counter/(limit-start+1))*100,"% geocode rate.",sep=""))
  return(addressList)
}

# spatial_helper-----------------------------------

#' Spatial Helper
#'
#' Internal helper function to create spatial objects by merging geoid with geo_tracts data.
#'
#' @param df Dataobject with GEOID column.
#' @param geography Param specifying the geographical designation for census information.
#' @param state Filter geo_var_builder by state to speed things up.
#' @param county Filter geo_var_builder by county to speed things up.
#' @param geosObject Optional geosObject to speed up processing time.
#' @param test Internal: Logical param to bypass feedback and provide limited data for testing.
#' @param verbose Logical param to provide feedback.
#'
#' @return An sf object
#'
#' @examples \dontrun{
#' spatial_helper(data)
#' }
spatial_helper <- function(df,
                           geography="tract",
                           state=NULL,
                           county=NULL,
                           geosObject=NULL,
                           test=FALSE,
                           verbose=FALSE
){
  
  # if(data_test(df) %in% c(2,4,5)){
  #   stop("spatial_helper() requires type 3 data.")
  # }

  if(type_data(df,return = FALSE) != 1){
    stop("spatial_helper() requires type 1 data.")
  }

  ## TODO Clean this up to cohere with geo_var_builder logic
  if(verbose==TRUE)message("--sp- Checking geosObject...")
  if(is.null(geosObject))geosObject <- geo_var_builder(geography=c("state","county",geography),
                                                       try="local",
                                                       geosObject=geosObject,
                                                       test=test,
                                                       verbose=verbose
  )

  if(!is.null(geosObject$geo_states))geo_states <- geosObject$geo_states
  if(!is.null(geosObject$geo_counties))geo_counties <- geosObject$geo_counties
  if(!is.null(geosObject$geo_tracts))geo_tracts <- geosObject$geo_tracts
  if(!is.null(geosObject$geo_blocks))geo_blocks <- geosObject$geo_blocks    
  
  if(verbose==TRUE)message("--sp- Checking for geography errors...")
  if(geography=="block group"){
    #geos <- geo_var_builder(geography="block groups",try="local")
    geo <- geo_blocks
  }else if(geography=="tract"){
    #geos <- geo_var_builder(geography="tract",try="local")
    geo <- geo_tracts
  }else if(geography=="county"){
    #geos <- geo_var_builder(geography="county",try="local")
    geo <- geo_counties
  }else if(geography=="state"){
    #geos <- geo_var_builder(geography="state",try="local")
    geo <- geo_states
  }else{
    stop("Error: geography not specified")
  }

  if(verbose==TRUE)message("--sp- Merging geography with df...")
  if(geography=="block group" || geography=="tract"){
   df <- merge(geo,df,by.x="GEOID",by.y="geoid",x.all=FALSE,y.all=FALSE)
  }
  else if(geography=="county"){
    df <- merge(geo,df,by.x=c("STATEFP","COUNTYFP"),by.y=c("state","county"))
  }
  else{
    df <- merge(geo,df,by.x="STATEFP",by.y="state")
  }
  if(verbose==TRUE)message("--sp- Returning df...")
  return(df)
}

# build_map--------------------------
build_map <- function(mapDF=NULL,
                      filterAddress=NULL,
                      ggrObject=NULL,
                      markers=NULL,
                      dispRoads=TRUE,
                      dispWater=TRUE,
                      dispPlaces=TRUE,
                      dispRails=FALSE, 
                      state=NULL,
                      county=NULL,
                      LegendTitle="Census Tracts",
                      MapTitle="Selected Census Tracts",
                      i=FALSE,
                      radiusOnly=FALSE,
                      alpha=0.2,
                      areaOnly=FALSE,
                      geography=NULL,
                      verbose=FALSE,
                      year = NULL,
                      st = NULL){
  
  if(radiusOnly==FALSE & areaOnly==FALSE){
    value <- "value"
    pal <- paletteer_c("ggthemes::Blue-Teal", 30)
  }else{
    value <- "name_clean"
    pal <- paletteer_d("ggpomological::pomological_palette")
  }
  
  if(i==FALSE){
    ## Set parameter for geo elements resolution. That is, only if block group, tract, or county
    ## with single county specified. 
    if(geography %in% c("tract","block group") || (geography=="county" && !is.null(county))){
      if(geography %in% c("tract","block group")){
        geo_resolution <- 2
      }else{
        geo_resolution <- 1 
      }
    }else{
      geo_resolution <- 0
    }
    
    ## Fetches road, water, place, etc markers, unless passthrough is provided.
    if(is.null(markers)){
      markers <- geo_marker_builder(obj=markers,
                                    dispRoads = dispRoads,
                                    dispWater = dispWater,
                                    dispPlaces = dispPlaces,
                                    dispRails = dispRails,
                                    geo_resolution = geo_resolution,
                                    ggrObject = ggrObject,
                                    year = year,
                                    verbose = verbose,
                                    st=NULL)
    }
    ## Extract from markers object. TODO link this to dispRoads, etc as well.
    if(!is.null(names(markers))){
      if("roads" %in% names(markers)){
        roads <- markers$roads
      }
      if("water" %in% names(markers)){
        water <- markers$water
      }
      if("places" %in% names(markers)){
        places <- markers$places
      }
      if("rails" %in% names(markers)){
        rails <- markers$rails
      }
    }
    
    tmode <- "plot"
  }else{
    tmode <- "view"
  }
  
  if(verbose==TRUE)message(paste(dur(st),"Crafting map..."))
  
  map <- tm_basemap(server=providers$Stadia.StamenTonerLite) +
    tm_shape(mapDF,unit="mi") +
    tm_polygons(value,
                title=LegendTitle,
                style="pretty",
                id="name_clean",
                alpha=alpha) +
    {if(areaOnly==FALSE && !is.null(filterAddress)){
      tm_shape(ggrObject$buffer) +
        tm_borders("darkred",lwd=1) +
        tm_shape(ggrObject$coordinates) +
        tm_dots("darkred",size = .25) 
    }} + 
    {if(i==FALSE){
      {if(dispRoads==TRUE && geo_resolution>=1){
        {if(nrow(roads)>0){
          tm_shape(roads) +
            tm_lines(col="#6F6F6F") }}}} +
        {if(dispRails==TRUE && geo_resolution>=1){
          {if(nrow(rails)>0){
            tm_shape(rails) +
              tm_lines(col="darkblue") }}}} +
        {if(dispRoads==TRUE && geo_resolution>=1){
          {if(nrow(roads)>0){
            tm_shape(roads) +
              tm_text("FULLNAME",size=1/2,remove.overlap = TRUE) }}}} +
        {if(dispPlaces==TRUE && geo_resolution>=1){
          {if(nrow(places)>0){
            tm_shape(places) +
              tm_text("NAME") }}}} +
        {if(dispWater==TRUE && geo_resolution>=1){
          {if(nrow(water)>0){
            tm_shape(water) +
              tm_polygons(col="#BFE7E1") }}}}
    }} +
    tmap_mode(tmode)
  
  if(verbose==TRUE)message(paste(dur(st),"Returning map..."))
  return(map)
}  

# map_locations--------------

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
