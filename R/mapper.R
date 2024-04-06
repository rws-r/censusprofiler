#' mappeR
#'
#' mappeR takes ACS data pulled from capi() and creates a map displaying
#' estimates, either as simple counts, or percentages based on summary
#' variables.
#'
#' Logics include: 1) a call with variables + filterAddress (capi() call)
#' [variable,variableSummary,filterAddress,filterRadius]; 2) a call with
#' variables + filterAddress + df included
#' [mapDF,variable,variableSummary,filterAddress,filterRadius]; 3) a call
#' without variables + filterAddress displaying area by geography
#' [filterAddress,filterRadius,areaOnly]; 4) a call without variables +
#' geography specified (e.g.: state=17, geography="county")
#' [state/county/tract,areaOnly]; 5) a call without variables + filterAddress
#' and geography, but only radius displayed
#' [filterAddress,filterRadius,radiusOnly]
#'
#' @param mapDF A census profiler object.
#' @param tableID Variable base, prior to the underscore: i.e. "B01001"  ->>
#'   TODO Necessary???
#' @param variable Since maps are not ideal for displaying a range of variables,
#'   the single variable number to display.
#' @param variableSummary Summary variable against which to calculate the
#'   percentage.
#' @param year  Year for capi().
#' @param geography Either "us","state","county","tract", or "block group".
#' @param filterAddress Address for querying a set of geographies as a radius
#'   around a location.
#' @param filterRadius The radius in miles.
#' @param coords Parameter to pass coordinates to function for quicker
#'   geolocation.
#' @param tract A list of specific tracts to filter.
#' @param county County by name or FIPS code.
#' @param state State by name or FIPS code.
#' @param geoidLookup Lookup geography by GEOID.
#' @param dispPerc Set to TRUE to calculate percentage based on summary
#'   variable.
#' @param LegendTitle Set for custom title.
#' @param MapTitle Text value to set map title.
#' @param radiusOnly To display the tracts/counties/states and the radius on a
#'   map.
#' @param areaOnly To display the tracts/counties/states within the area
#'   specified by geography.
#' @param alpha Set alpha value of map.
#' @param interactive Parameter for setting tmap_mode to either `interactive` or
#'   `plot.`
#' @param geosObject Optional geosObject to speed up processing time.
#' @param markers A geos object containing sf files for roads, water, places or
#'   rails. Intended to speed up repeated uses of mapper, as in RMD files.
#' @param dispRoads Logical: whether to display roads on non-interactive map.
#' @param dispWater Logical: whether to display water bodies on non-interactive
#'   map.
#' @param dispPlaces Logical: whether to display places on non-interactive map.
#' @param dispRails Logical: whether to display railroads on non-interactive
#'   map.
#' @param road_resolution Possible values: c(1,2,3,4). Value of 1 displays all roads
#' and names; value of 2 shows all roads, but only potentially major road names; 
#' value of 3 shows all roads and no names; value of 4 shows and names only 
#' potentially major roads; value of 5 shows only potential major roads and no names.
#' @param dataset_main Selection parameters for get_census_variables (e.g. "acs")
#' @param dataset_sub Selection parameters for get_census_variables (e.g. "acs5")
#' @param dataset_last Selection parameters for get_census_variables (e.g. "cprofile")
#' @param censusVars Passthrough object to bypass get_census_variables 
#' @param verbose Logical param to provide feedback.
#' @param test Internal: Logical param to bypass feedback and provide limited
#'   data for testing.
#' @param st Internal variable passed from other functions to provide
#'   consistency on timestamp.
#'
#' @return Returns a filtered tmap map, detailing either estimates or
#'   percentages of variables requested.
#'
#' @export
#'
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_set_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_centroid
#' @importFrom sf st_coordinates
#' @importFrom sf st_union
#' @importFrom sf st_bbox
#' @import tmap
#' @import paletteer
#'
#' @examples
#' \dontrun{
#' Create a map of a selected region based on a variable.
#' mapper(mapDF = NULL,tableID = "B02001",variable = "B02001_002",variableSummary =
#' "B02001_001",geography = "tract",year = 2022,dispPerc = FALSE,
#' MapTitle = "Selected Census Tracts",LegendTitle =
#' "Census Tracts",filterAddress = address,filterRadius = 1,alpha =
#' 0.1,interactive = FALSE,geosObject = NULL,verbose = FALSE)
#'
#' Create a map of an entire geography, using a variable.
#' mapper(mapDF = NULL,tableID = "B02001",variable = "B02001_002",variableSummary =
#' "B02001_001",geography = "county",year = 2022,state = 17,county
#' = 043,dispPerc = FALSE,MapTitle = "Selected Census Tracts",LegendTitle =
#' "Census Tracts",alpha = 0.1,interactive = FALSE,geosObject = NULL,
#' verbose = FALSE)
#' }
#'
mapper <- function(mapDF=NULL,
                   tableID=NULL,
                   variable=NULL,
                   variableSummary=NULL,
                   year=NULL,
                   geography=NULL,
                   filterAddress=NULL,
                   filterRadius=NULL,
                   coords=NULL,
                   tract=NULL,
                   county=NULL,
                   state=NULL,
                   geoidLookup=NULL,
                   dispPerc=FALSE,
                   LegendTitle=NULL,
                   MapTitle=NULL,
                   radiusOnly=FALSE,
                   areaOnly=FALSE,
                   alpha=0.5,
                   interactive=FALSE,
                   geosObject=NULL, 
                   markers=NULL,
                   dispRoads=TRUE,
                   dispWater=TRUE,
                   dispPlaces=TRUE,
                   road_resolution=1,
                   dispRails=FALSE,
                   dataset_main="acs",
                   dataset_sub="acs5",
                   dataset_last=NULL,
                   censusVars=NULL,
                   verbose=FALSE,
                   test=FALSE,
                   st=NULL
){

  ## Deal with "no visible binding for global variable" error 
  estimate  <- pct <- value <- concept <- block_group <- GEOID <- NULL
  
  # Internal functions ------------------------------------------------------

  ifelse(is.null(st),st <- Sys.time(),st <- st)

# Initial error checking (parameters) -------------------------------------

  if(!is.null(filterAddress) & (!is.null(county) | !is.null(state))){
    warning("!> You don't need to include specific geographies if you're including a filter address. 
    They are ignored here in preference of filterAddress.")
  }

  if(is.null(geography)){
    stop("ERROR: You need to include a geography designation.")
  }
  
  if(areaOnly==TRUE && radiusOnly==TRUE){
    warning("Both areaOnly and radiusOnly are set to true. mapper doesn't know which to prioritize. Only area will be shown. Please deselect one.")
  }
  
  if((areaOnly==TRUE | radiusOnly==TRUE) && (!is.null(variable) | !is.null(tableID))){
    stop("!> You cannot include an area/radius only parmeter, and a variable lookup call. Please pick one or the other.")
  }
  
  if(is.null(tableID) && is.null(variable) && is.null(filterAddress) && !is.null(geoidLookup) && areaOnly==FALSE){
    areaOnly <- TRUE
    warning("!> This appears to be an areaOnly==TRUE call. Automatically adjusted.")
  }

# Data init ---------------------------------------------------------------
  
  if(verbose==TRUE)message("Starting mapper.")
  
  ## Check and format variables
  # if(radiusOnly==FALSE && areaOnly==FALSE && is.null(geoidLookup)){
  #   viC <- varInputCheck(tableID,variable,variableSummary)
  #   variable <- viC$variable
  #   variableSummary <- viC$variableSummary
  # }
  
  ## Check and format date
  year <- dateInputCheck(year,dataset_sub,verbose=verbose)

  # Secure geosObjects
  if(is.null(geosObject)){
    geosObject <- geo_var_builder(
      geography=c("state","county",geography),
      try="local",
      geosObject=geosObject,
      test=test,
      verbose=verbose
    )}
  
  ## See what elements are in the geos, and simplify variables.
  if(verbose==TRUE)message("- mapper | Checking geos elements...")
  if(!is.null(geosObject$geo_states)){
    geo_states <- geosObject$geo_states
    if(verbose==TRUE)message("- mapper |   + geo_states")
  }
  if(!is.null(geosObject$geo_counties)){
    geo_counties <- geosObject$geo_counties
    if(verbose==TRUE)message("- mapper |   + geo_counties")
  }
  if(!is.null(geosObject$geo_tracts)){
    geo_tracts <- geosObject$geo_tracts
    if(verbose==TRUE)message("- mapper |   + geo_tracts")
  }
  if(!is.null(geosObject$geo_blocks)){
    geo_blocks <- geosObject$geo_blocks    
    if(verbose==TRUE)message("- mapper |   + geo_blocks")
  }

  ## Get census variables
  if(verbose==TRUE)message("Getting CV...")
  if(is.null(censusVars)){ 
    CV <- get_census_variables(year=year, dataset_main = dataset_main, dataset_sub = dataset_sub, dataset_last = dataset_last)
  }else{
    CV <- censusVars
  }
  CV.VARS <- CV[[1]]
  CV.GROUPS <- CV[[2]]
  
  ## If mapDF, process.
  if(!is.null(mapDF)){
    if(verbose==TRUE)message("- mapper | MapDF included. Processing mapDF...")
      ## Get GGR or set GGR variables for regions.
    if(type_data(mapDF,return = FALSE) == 5){
      # If profile object is given, supply correct data.
      ggr <- get_geocode_radius(filterAddress=mapDF$info$address,
                                filterRadius=mapDF$info$radius,
                                geography = geography,
                                coords = mapDF$info$coordinates,
                                geosObject = geosObject,
                                verbose = verbose)
      mapDF <- mapDF$data$type1data
    }else{
      if(type_data(mapDF,return = FALSE) != 1){
        stop("mapper() requires type 1 data.")
      }
    }

    ## Filter out all but relevant variable.
    if(!is.null(variable)){
      if(is.numeric(variable)){
        var <- varInputCheck(tableID = tableID, variables = variable)
        variable <- var$variables
      }
      mapDF <- mapDF[mapDF$variable==variable,]
    }
  }else if(!is.null(filterAddress) | !is.null(coords)){
    if(verbose==TRUE)message("- mapper | No mapDF. Processing filterAddress or coords...")
    ggr <- get_geocode_radius(filterAddress=filterAddress,
                              filterRadius=filterRadius,
                              geography = geography,
                              coords = coords,
                              geosObject = geosObject,
                              verbose = verbose)
  }else if(!is.null(state) | !is.null(county)){
    if(verbose==TRUE)message("- mapper | No mapDF. Processing geographic area...")
    ggr <- get_geocode_radius(filterAddress=NULL,
                              filterRadius=NULL,
                              state=state,
                              county=county,
                              geography = geography,
                              coords = coords,
                              geosObject = geosObject,
                              verbose = verbose)
  }else if(!is.null(geoidLookup)){
    if(verbose==TRUE)message("- mapper | No mapDF. Processing geographic area by geoid...")
    ggr <- get_geocode_radius(filterAddress=NULL,
                              filterRadius=NULL,
                              state=NULL,
                              county=NULL,
                              geoidLookup=geoidLookup,
                              geography = geography,
                              coords = coords,
                              geosObject = geosObject,
                              verbose = verbose)

  }else{
    ## At this point, if there's no mapDF, no filterAddress / coords, and no state/county, 
    ## we have nothing to work with. Throw error.
    if(is.null(state) && is.null(county)){
      stop("!> No geo reference supplied. Please include either valid mapDF, filterAddress, coords, or state/county (name / FIPS).")
    }
    ggr <- list(states = state,
                counties = county)
  }
  
  if(isFALSE(radiusOnly) & isFALSE(areaOnly)){
    ## Obviously, if mapDF is included, ignore capi().
    if(is.null(mapDF)){
      if(verbose==TRUE)message("- mapper | Getting data from capi()...")
      mapDF <- capi(year=year,
                    censusVars=CV,
                    tableID=tableID,
                    variables=c(variableSummary,variable),
                    geography=geography,
                    filterAddress=filterAddress,
                    filterRadius=filterRadius,
                    ggr=ggr,
                    state=state,
                    county=county,
                    tract=tract,
                    block_group = block_group,
                    verbose=verbose,
                    st=st)
    }
    ## Run mapDF (whether from capi() or supplied) through spatial_helper.
    if(verbose==TRUE)message("- mapper | Getting and setting mapDF via spatial_helper()...")
    mapDF <- spatial_helper(df=mapDF,
                            geography=geography,
                            state=state,
                            county=county,
                            geosObject = geosObject,
                            verbose=verbose)
    
    ## See if mapDF is fully-formed with ggr. If not, append ggr.
    if(!("df" %in% names(mapDF))){
      ggr <- append(ggr,list(df = mapDF))
    }
  }else{
    ## If our mapDF is for an areaOnly or radiusOnly call, use the supplied
    ## ggr$df object, rather than mapDF.
    mapDF <- ggr$df
  }
# Prepare data ------------------------------------------------------------
    if(radiusOnly==FALSE & areaOnly==FALSE){ 
    if(verbose==TRUE)message(paste(dur(st),"Setting up mapDF with modifications..."))
    
    if(dispPerc==TRUE){
      mapDF <- mapDF %>% mutate(value = as.numeric(pct))
      mapDF <- mapDF %>% mutate(value_print = formattable::percent(value,digits=2))
    }else{
      mapDF <- mapDF %>% mutate(value = estimate)
      mapDF <- mapDF %>% mutate(value_print = format(value,big.mark = ","))
    }
  }

  # Strip out county/state info from `name`
  if(radiusOnly==FALSE && areaOnly==FALSE){
    if(verbose==TRUE)message(paste(dur(st),"Update name col to strip out extraneous geo data..."))
    md <- st_drop_geometry(mapDF)
    for(i in 1:nrow(md)){
      mapDF[i,'name_clean'] <- (unlist(stringr::str_split(md[i,'name'],";"))[1])
    }
  }else{
    ## If we are passing only filter address, etc. but no mapDF, use ggr object.
    ## Otherwise, use mapDF.
    if(is.null(mapDF)){
      mapDF <- ggr$df
      #mapDF <- mapDF %>% mutate(name_clean = GEOID)
      mapDF <- mapDF %>% mutate(name_clean = NAMELSAD)
    }else{
      #md <- sf::st_drop_geometry(mapDF)
      mapDF <- mapDF %>% mutate(name_clean = NAMELSAD)
      # for(i in 1:nrow(md)){
      #   #mapDF <- mapDF %>% mutate(name_clean = GEOID)
      #   mapDF[i,'name_clean'] <- (unlist(stringr::str_split(md[i,'name'],";"))[1])
      #   
      # }
    }
  }

  if(verbose==TRUE)message(paste(dur(st),"Preparing centroids..."))
  mapDF <- mapDF %>% mutate(centroid = st_centroid(mapDF$geometry))
  dfC <- mapDF %>% mutate(coord=st_coordinates(mapDF$centroid))
  dfC <- st_as_sf(dfC)
  
  if(radiusOnly==FALSE & areaOnly==FALSE){
    if(verbose==TRUE)message(paste(dur(st),"Generating map label..."))
    if(is.null(LegendTitle)){
      dfLab <- unique(sf::st_drop_geometry(mapDF) %>% filter(variable==variable) %>% select(labels))
      dfLab <- dfLab$labels
    }else{
      dfLab <- LegendTitle
    }
    if(is.null(MapTitle)){
      dfTitle <- unique(sf::st_drop_geometry(mapDF) %>% filter(tableID==tableID) %>% select(concept))
      dfTitle <- dfTitle$concept
    }else{
      dfTitle <- MapTitle
    }
    
    if(verbose==TRUE)message(paste(dur(st),"Clearing out NA values..."))
    mapDF <- mapDF %>% filter(!is.na(value))
    
  }else{
    dfl <- case_when(
      geography=="state" ~ "State",
      geography=="county" ~ "County",
      geography=="tract" ~ "Tract",
      geography=="block group" ~ "Block Group",
      .default = "Geographical Unit"
    )
    dfLab <- dfl
    dfTitle <- "Selected Map Area"
  }
  

    if(radiusOnly==FALSE & areaOnly==FALSE){
    ## Calculations to get custom breaks for legend.
    if(verbose==TRUE)message(paste(dur(st),"Calculating label breaks..."))
    maxVal <- max(mapDF$value)
    minVal <- min(mapDF$value)
    breaks <- case_when(
      maxVal < 1 ~ 0.05,
      maxVal < 10^1 ~ 1,
      maxVal < 10^2 ~ 10^1,
      maxVal < 10^3 ~ 10^2,
      maxVal < 10^4 ~ 10^3,
      maxVal < 10^5 ~ 10^4,
      .default = 10^5
    )
    
    ## If breaks are too big (distance between min/max is smaller), halve.
    if(((maxVal-minVal)/breaks) < 2){
      breaks <- breaks/2
    }
    ## Find a reasonable starting place.
    floorVal <- floor(minVal/breaks)*breaks
  }

# Create map --------------------------------------------------------------

  ## Display selected area, without individual data.
  if(radiusOnly==TRUE){
    if(verbose==TRUE)message(paste(dur(st),"Working with radiusOnly..."))
    x <- build_map(mapDF=mapDF,
                   filterAddress=filterAddress,
                   ggrObject=ggr,
                   LegendTitle=dfLab,
                   MapTitle=dfTitle,
                   i=interactive,
                   state=state,
                   county=county,
                   radiusOnly=TRUE,
                   markers=NULL,
                   dispRoads=dispRoads,
                   dispWater=dispWater,
                   dispPlaces=dispPlaces,
                   dispRails=dispRails,
                   road_resolution=road_resolution,
                   alpha=alpha,
                   areaOnly=areaOnly,
                   geography=geography,
                   year = year,
                   st = st,
                   verbose=verbose)
    return(x)
  }else{
    if(verbose==TRUE)message(paste(dur(st),"Working with data map..."))
    x <- build_map(mapDF=mapDF,
                   filterAddress=filterAddress,
                   ggrObject=ggr,
                   LegendTitle=dfLab,
                   MapTitle=dfTitle,                 
                   i=interactive,
                   state=state,
                   county=county,
                   radiusOnly=FALSE,
                   markers=NULL,
                   dispRoads=dispRoads,
                   dispWater=dispWater,
                   dispPlaces=dispPlaces,
                   dispRails=dispRails,
                   road_resolution=road_resolution,
                   alpha=alpha,
                   areaOnly=areaOnly,
                   geography=geography,
                   year = year,
                   st = st,
                   verbose=verbose)
    return(x)
  }
}
