#' Load Data
#' 
#' A convenience function that loads functional data, including ACS variables, 
#' a geos object, stats object, and/or geo profile comparison object. These are 
#' created and then if selected, loaded into the global environment. 
#'
#' @param load_censusVars Logical param to capture census variables and concepts.
#' @param load_geos Logical param to capture geos object.
#' @param load_stats Logical param to create stats_object.
#' @param load_profile_compare Logical param to create geo profile comparison object.
#' @param geography Default geography option for stat_table_builder.
#' @param geo_data Default geography options for load_geos.
#' @param dataset_main Selection parameters for get_census_variables (e.g. "acs")
#' @param dataset_sub Selection parameters for get_census_variables (e.g. "acs5")
#' @param dataset_last Selection parameters for get_census_variables (e.g. "cprofile")
#' @param censusVars Passthrough object to bypass get_census_variables 
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
load_data <- function(load_censusVars = FALSE,
                      load_geos = FALSE,
                      load_stats = FALSE,
                      load_profile_compare = FALSE,
                      geography = "tract",
                      geo_data = c("state","county","tract"),
                      dataset_main="acs",
                      dataset_sub="acs5",
                      dataset_last=NULL,
                      censusVars=NULL,
                      loadToGlobal = TRUE,
                      year = 2021,
                      tableID = NULL,
                      variables = NULL,
                      geosObject=NULL,
                      test=FALSE,
                      verbose=FALSE){
  
  ## Set all promised variables to NULL
  CV <- CV.VARS <- CV.GROUPS <- NULL
  
  if(load_censusVars==TRUE){
    if(verbose==TRUE)message("Getting CV...")
    if(is.null(censusVars)){ 
      CV <- get_census_variables(year=year, dataset_main = dataset_main, dataset_sub = dataset_sub, dataset_last = dataset_last)
    }else{
      CV <- censusVars
    }
    CV.VARS <- CV[[1]]
    CV.GROUPS <- CV[[2]]
    
    if(loadToGlobal==TRUE){
      CV <<- CV
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
    z <- stat_table_builder(year=year,
                            data = NULL,
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
    if(load_censusVars==TRUE)dt <- append(dt,list(CV=CV))
    if(load_geos==TRUE)dt <- append(dt,list(geos=geos))
    if(load_stats==TRUE)dt <- append(dt,list(stats=profile_stats))
    if(load_profile_compare)dt <- append(dt,list(stateCompare=stateCompare,usCompare=usCompare))
    return(dt)
    on.exit(
      if(load_acs==TRUE)rm(CV),
      if(load_geos==TRUE)rm(geos),
      if(load_stats==TRUE)rm(profile_stats))
      if(load_profile_compare==TRUE)rm(stateCompare,usCompare)
  }

} 
