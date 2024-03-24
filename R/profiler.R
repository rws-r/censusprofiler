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
#' @param ggr Internal: to pass a get_geocode_radius() object to function.
#' @param geosObject Optional, attach geos object to simplify geo processes.
#' @param verbose Logical parameter to specify whether to produce verbose output.
#' @param st Internal parameter to provide timestamp consistency.
#' @param fast Internal parameter for pseudo_tableID and stat table (capi())
#' @param test Internal parameter for testing suite.
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
                     filterByGeoType=NULL,
                     filterByGeoValue=NULL,
                     filterSummary=FALSE,
                     filterSummaryLevels="root",
                     state=NULL,
                     county=NULL,
                     tract=NULL,
                     block_group=NULL,
                     metro=NULL,
                     ggr=NULL,
                     geosObject=NULL,
                     test=FALSE,
                     fast=FALSE, # For pseudo_tableID + stat table
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
  
  if(verbose==TRUE)message("Getting CV...")
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
    stop("!> You need to include a geography parameter.")
  }
  
  if(is.null(year)){
    stop("!> Year value is required to make a profile.")
  }
  
# Get geography -----------------------------------------------------------

  ## Get ggr object to pass to capi()
  if(!is.null(filterAddress) | !is.null(filterByGeoType)){
    if(!is.null(filterAddress)){ 
      if(verbose==TRUE)message(paste(dur(st),"Filtering geos by address and radius..."))
      
      if(is.null(ggr)){
        if(verbose==TRUE)message(paste(dur(st),"Finding geo area by radius..."))
        ggr <- get_geocode_radius(filterAddress = filterAddress,
                                  filterRadius = filterRadius,
                                  geography = geography,
                                  year = year,
                                  fipsOnly = TRUE)
      }
    }else{
      if(is.null(ggr)){
        if(verbose==TRUE)message(paste(dur(st),"Finding geo area by radius..."))
        ggr <- get_geocode_radius(filterByGeoType = filterByGeoType,
                                  filterByGeoValue = filterByGeoValue,
                                  geography = geography,
                                  state = state,
                                  year = year,
                                  fipsOnly = TRUE)
      }
    }
  }else{
    if(verbose==TRUE)message(paste(dur(st),"Getting geo var list..."))
    if(is.null(geosObject)){
      geosObject <- geo_var_builder(geography=c("state","county",geography),
                                    try="local",
                                    state=state,
                                    county=county,
                                    #  geosObject=geosObject,
                                    verbose=verbose,
                                    test=FALSE)  
    }
  }
  
  if(verbose==TRUE)message(paste(dur(st),"Building data profile..."))
  ## Execute capi() with supplied parameters.
    
    data <- capi(year=year,
                 tableID=tableID,
                 variables=variables,
                 geography=geography,
                 filterAddress=filterAddress,
                 filterRadius=filterRadius,
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
                 fast=fast,
                 verbose=verbose,
                 profile=TRUE,
                 test=test,
                 st=st)
  
  info <- list(name = name,
               address = filterAddress,
               radius = filterRadius,
               year = year,
               coordinates = ggr$coordinates,
               buffer = ggr$buffer,
               states = ggr$states,
               counties = ggr$counties)
  
  df <- list(info = info,
             data = data)
  
  return(df)
}