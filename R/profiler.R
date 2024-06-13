#' ProfileR
#' 
#' A wrapper for capi() with additional parameters for profile creation.
#'
#' @param name User-supplied name for profile.
#' @param year Year for data selection.
#' @param dataset_main Selection parameters for get_census_variables (e.g. "acs")
#' @param dataset_sub Selection parameters for get_census_variables (e.g. "acs5")
#' @param dataset_last Selection parameters for get_census_variables (e.g. "cprofile")
#' @param censusVars Passthrough object to bypass get_census_variables 
#' @param variables A vector of all variables requested.
#' @param geography Geography specification: e.g.: tract, county, state.
#' @param filterAddress An address input used to generate a radius around, for filtering data.
#' @param filterRadius A numeric value specifying the radius in miles around the address.
#' @param coords An sf coordinate object for get_geocode_radius.
#' @param filterByGeoType An irregular geo type to get a smaller overlapping set
#'   of tracts, block_groups or other geography from. Options are currently
#'   "metro", "place","combined_statistical_areas". E.g., Find all tracts in Chicago (place).
#' @param filterByGeoValue A value to find object for filtering. Either NAME or GEOID. 
#' @param filterSummary Logical parameter to specify whether to filter out summary levels (typically _001 and therefore "root").
#' @param filterSummaryLevels Explicit description of lowest type denoting summary level. Also excludes lower levels.#' @param state Input (abb. or FIPS) of state for search.
#' @param state Input (abb. or FIPS) of state for search.
#' @param county Input (abb. or FIPS) of county for search.
#' @param tract Input (abb. or FIPS) of tract for search.
#' @param block_group Input (abb. or FIPS) of block group for search.
#' @param metro Input (abb. or FIPS) of metropolitan statistical area for search.
#' @param place Input (abb. or FIPS) of place for search.
#' @param ggr Internal: to pass a get_geocode_radius() object to function.
#' @param geosObject Optional, attach geos object to simplify geo processes.
#' @param verbose Logical parameter to specify whether to produce verbose output.
#' @param st Internal parameter to provide timestamp consistency.
#' @param fast Internal parameter for stat table (capi())
#' @param test Internal parameter for testing suite.
#' @param simpleReturn Param to return raw data, not formatted.
#' @param tableID Specification for concept, or group: e.g., "B01001"
#'
#' @return data.frame
#' @export
#'
#' @examples \dontrun{
#' make_profile(x)
#' }
profiler <- function(name=NULL,
                     year=NULL,
                     dataset_main="acs",
                     dataset_sub="acs5",
                     dataset_last=NULL,
                     censusVars=NULL,  
                     tableID=NULL,   
                     variables=NULL, # Only need to include variable list.
                     geography=NULL,
                     filterAddress=NULL,
                     filterRadius=NULL,
                     coords=NULL,
                     filterByGeoType=NULL,
                     filterByGeoValue=NULL,
                     filterSummary=FALSE,
                     filterSummaryLevels="root",
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     metro=NULL,
                     place=NULL,
                     ggr=NULL,
                     geosObject=NULL,
                     simpleReturn=FALSE,
                     test=FALSE,
                     fast=FALSE, 
                     verbose=FALSE,
                     st=NULL){
  

# Internal functions ------------------------------------------------------

  if(is.null(st)){
    st <- Sys.time()
  }
  dur <- function(st){
    x <- paste(format(round(difftime(Sys.time(),st,units = "sec"),4),scientific=FALSE)," | ")
    return(x)
  }
  
  if(verbose==TRUE)message("profiler() | Getting CV...")
  if(is.null(censusVars)){ 
    CV <- get_census_variables(year=year, dataset_main = dataset_main, dataset_sub = dataset_sub, dataset_last = dataset_last)
  }else{
    CV <- censusVars
  }
  CV.VARS <- CV[[1]]
  CV.GROUPS <- CV[[2]]

# Error checking ----------------------------------------------------------

  if(is.null(geography)){
    ##ADDTEST
    stop("!> profiler() |  You need to include a geography parameter.")
  }
  
  if(is.null(year)){
    stop("!> profiler() | Year value is required to make a profile.")
  }
  
  if((!is.null(filterAddress) | !is.null(coords)) & is.null(filterRadius)){
    stop("!> profiler() | You must include a filterRadius value if using filterAddress or coords.")
  }
  
# Get geography -----------------------------------------------------------

  ## Get ggr object to pass to capi()
  if(!is.null(filterAddress) || !is.null(filterByGeoType) || !is.null(coords)){
    if(!is.null(filterAddress) | !is.null(coords)){ 
      if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Filtering geos by address and radius..."))
      
      if(is.null(ggr)){
        if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Finding geo area by radius..."))
        ggr <- get_geocode_radius(filterAddress = filterAddress,
                                  filterRadius = filterRadius,
                                  coords = coords,
                                  geography = geography,
                                  year = year,
                                  profile = TRUE,
                                  geosObject = geosObject,
                                  verbose = verbose)
      }
    }else{
      if(is.null(ggr)){
        if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Finding geo area by radius (filterByGeoType)..."))
        ggr <- get_geocode_radius(filterByGeoType = filterByGeoType,
                                  filterByGeoValue = filterByGeoValue,
                                  geography = geography,
                                  state = state,
                                  year = year,
                                  profile = TRUE,
                                  geosObject = geosObject,
                                  verbose = verbose)
      }
    }
  }else if(geography=="place"){
    if(is.null(ggr)){
      if(verbose==TRUE)message(paste(dur(st),"Finding geo area by radius..."))
      ggr <- get_geocode_radius(place = place,
                                coords = coords,
                                geography = geography,
                                state = state,
                                geosObject = geosObject,
                                year = year,
                                fipsOnly = TRUE)
    }
  }else{
    if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Getting geo var list..."))
    if(is.null(geosObject)){
      geosObject <- geo_var_builder(geography=c("state","county",geography),
                                    try="local",
                                    state=state,
                                    county=county,
                                    #  geosObject=geosObject,
                                    verbose=verbose,
                                    test=FALSE) 
    }
    if(is.null(ggr) & geography!="us"){
      if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Getting ggr..."))
      ggr <- get_geocode_radius(state=state,
                                county=county,
                                geography=geography,
                                geosObject = geosObject,
                                verbose=verbose)
    }
  }
  
  ## If failure in geocode or coord lookup (or other geography parameters), stop
  ## implementation, as we cannot proceed.
  if(is.null(ggr) & geography!="us"){
    if(verbose==TRUE)message("profiler() | No geo found for supplied parameters. Cannot complete profile.")
    return(NULL)
  }else{
    
    
    if(verbose==TRUE)message(paste("profiler() | ",dur(st),"Building data profile..."))
    ## Execute capi() with supplied parameters.
    
    data <- capi(year=year,
                 tableID=tableID,
                 variables=variables,
                 geography=geography,
                 filterAddress=filterAddress,
                 filterRadius=filterRadius,
                 coords=coords,
                 filterByGeoType = filterByGeoType,
                 filterByGeoValue = filterByGeoValue,
                 filterSummaryLevels=filterSummaryLevels,
                 ggr=ggr,
                 state=state,
                 county=county,
                 metro=metro,
                 tract=tract,
                 block_group=block_group,
                 censusVars=CV,
                 dataset_main = dataset_main,
                 dataset_sub = dataset_sub,
                 dataset_last = dataset_last,
                 fast=fast,
                 verbose=verbose,
                 profile=TRUE,
                 simpleReturn = simpleReturn,
                 test=test,
                 st=st)
    if(simpleReturn==FALSE){
      ti <- CV.GROUPS[CV.GROUPS$table_id %in% tableID,]
      info <- list(name = name,
                   address = filterAddress,
                   radius = filterRadius,
                   year = year,
                   tableids = paste(tableID,collapse=", "),
                   tableid.concepts = ti,
                   coordinates = ggr$coordinates,
                   buffer = ggr$buffer,
                   geoid = ggr$geoid,
                   states = ggr$states,
                   counties = ggr$counties)
      if(geography=="tract"){
        info <- append(info,list(tracts = ggr$tracts))
      }
      if(geography=="block group"){
        info <- append(info,list(tracts=ggr$tracts,block_groups=ggr$block_groups))
      }
      if(geography=="place"){
        info <- append(info,list(places=ggr$places))
      }
      
      df <- list(info = info,
                 data = data)
      attr(df,"dataType") <- 5
    }else{
      df <- data
    }
    
    return(df)
  }
}