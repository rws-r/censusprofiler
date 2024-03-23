#' Load Data
#' 
#' A convenience function that loads functional data, including ACS variables, 
#' a geos object, stats object, and/or geo profile comparison object. These are 
#' created and then if selected, loaded into the global environment. 
#'
#' @param load_acs Logical param to capture ACS variables and concepts.
#' @param load_geos Logical param to capture geos object.
#' @param load_stats Logical param to create stats_object.
#' @param load_profile_compare Logical param to create geo profile comparison object.
#' @param geography Default geography option for stat_table_builder.
#' @param geo_data Default geography options for load_geos.
#' @param loadToGlobal Logical param to save to global environment.
#' @param year Default year for data calls.
#' @param tableID ProfileList object for stat_table_builder.
#' @param variables Variable list for stat_table_builder.
#' @param verbose Logical param to provide feedback.
#' @param geosObject Optional geosObject to speed up processing time.
#' @param test Internal logical parameter to specify testing envir.
#'
#' @return data.frame objects loaded into global environment.
#' 
#' @export 
#'
#' @examples
#' \dontrun{
#' load_data(load_acs=TRUE, load_geos=TRUE,load_stats=TRUE,
#' variables=profile_variables,tableID=profile_tableID)
#' }
load_data <- function(load_acs = FALSE,
                      load_geos = FALSE,
                      load_stats = FALSE,
                      load_profile_compare = FALSE,
                      geography = "tract",
                      geo_data = c("state","county","tract"),
                      loadToGlobal = TRUE,
                      year = 2021,
                      tableID = NULL,
                      variables = NULL,
                      geosObject=NULL,
                      test=FALSE,
                      verbose=FALSE){
  
  ## Set all promised variables to NULL
  ACS <- ACS.VARS <- ACS.GROUPS <- NULL
  
  if(load_acs==TRUE){
    x <- acs_vars_builder(year=year,return=TRUE,verbose=verbose)
    if(loadToGlobal==TRUE){
      ACS.VARS <<- x[[1]]
      ACS.GROUPS <<- x[[2]]
    }else{
      ACS <- x
    }
  }
  if(load_geos==TRUE){
    y <- geo_var_builder(geography=geo_data,try="local",verbose=verbose)
    if(loadToGlobal==TRUE){
      geos <<- y
    }else{
      geos <- y
    }
  }
  if(load_stats==TRUE){
    ifelse(test==TRUE,ss <- 55,NULL)
    z <- stat_table_builder(data = NULL,
                            master_list = TRUE,
                            summary_table = TRUE,
                            tableID = tableID,
                            variables = variables,
                            geography = "tract",
                            geosObject = geosObject,
                            stateStart = ss,
                            test=TRUE,
                            verbose=verbose
    )
    
    if(loadToGlobal==TRUE){
      profile_stats <<- z
    }else{
      profile_stats <- z
    }
  }
    
  if(load_profile_compare==TRUE){
    s <- create_comparison_data(geography="state",
                                profileDataset = NULL,
                                year=2021,
                                variables = variables,
                                tableID = tableID, 
                                geosObject = geosObject,
                                test=TRUE,
                                verbose = verbose)
    
    u <- create_comparison_data(geography="us",
                                profileDataset = NULL,
                                year=2021,
                                variables = variables,
                                tableID = tableID, 
                                geosObject = geosObject,
                                test=TRUE,
                                verbose = verbose)
    
    
    if(loadToGlobal==TRUE){
      stateCompare <<- s
      usCompare <<- u
    }else{
      stateCompare <- s
      usCompare <- u
    }
  }
  
  if(loadToGlobal==FALSE){
    dt <- list()
    if(load_acs==TRUE)dt <- append(dt,list(ACS.VARS=ACS[[1]],ACS.GROUPS=ACS[[2]]))
    if(load_geos==TRUE)dt <- append(dt,list(geos=geos))
    if(load_stats==TRUE)dt <- append(dt,list(stats=profile_stats))
    if(load_profile_compare)dt <- append(dt,list(stateCompare=stateCompare,usCompare=usCompare))
    return(dt)
    on.exit(
      if(load_acs==TRUE)rm(ACS),
      if(load_geos==TRUE)rm(geos),
      if(load_stats==TRUE)rm(profile_stats))
      if(load_profile_compare==TRUE)rm(stateCompare,usCompare)
  }

} 
