#' Census API Data Call
#'
#' An API interface for capturing and formatting census data.
#'
#' @param year Year for data call.
#' @param datatype My main focus is on ACS data right now.
#' @param dataset Can select "acs1", "acsse" (supplemental est), "acs3", "acs5","flows" (migration flows) 
#' @param tableID Formerly known as varBase, or concept, or group: i.e., "B01001"
#' @param variables A vector of variables for the call. If multiple select variables per tableID are desired,
#' then variables should be constructed as a named list, with tableID as name, and sub list items as 
#' variables—either full ("B01001_001") or numeric (c(1:8)). 
#' @param geography Specifying geography: e.g., "tract", "county"
#' @param filterAddress An address input used to generate a radius around, for filtering data.
#' @param filterRadius A numeric value specifying the radius in miles around the address.
#' @param filterByGeoType An irregular geo type to get a smaller overlapping set
#'   of tracts, block_groups or other geography from. Options are currently
#'   "metro", "place","combined_statistical_areas". E.g., Find all tracts in Chicago (place).
#' @param filterByGeoValue A value to find object for filtering. Either NAME or GEOID.   
#' @param mode c("table","summarize","median")
#' @param state Input (abb. or FIPS) of state for search.
#' @param county Input (abb. or FIPS) of county for search.
#' @param tract Input (abb. or FIPS) of tract for search.
#' @param block_group Input (abb. or FIPS) of block group for search.
#' @param place Input (abb. or FIPS) of place for search.
#' @param metro Input (abb. or FIPS) of metropolitan statistical area for search.
#' @param consolidatedCity Input (abb. or FIPS) of consolidated city for search.
#' @param region Input (abb. or FIPS) of region for search.
#' @param division Input (abb. or FIPS) of division for search.
#' @param verbose Logical parameter to specify whether to produce verbose output.
#' @param profile Logical parameter to specify whether to build profile.
#' @param ggr Internal: to pass a get_geocode_radius() object to function.
#' @param filterSummary Logical parameter to specify whether to filter out summary levels (typically _001 and therefore "root").
#' @param filterSummaryLevels Explicit description of lowest type denoting summary level. Also excludes lower levels.
#' @param fast Internal parameter for pseudo_tableID and stat table
#' @param test Internal parameter for testing suite.
#' @param simpleReturn Param to return raw data, not formatted.
#' @param st Internal parameter to provide timestamp consistency.
#'
#' @return dataframe
#' 
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringr str_starts
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_i
#'
#' @examples \dontrun{
#' Basic call
#' capi(year=2022,datatype="acs", dataset="acs5", tableID="B01001",
#' variables=c("B01001_001","B01001_002"), geography="tract", filterAddress=v,
#' filterRadius=1, ggr=NULL, mode="table", filterSummary=FALSE,
#' filterSummaryLevels="root", state=NULL, county=NULL, tract=NULL,
#' block_group=NULL, verbose=TRUE, profile=FALSE, st=NULL)
#' 
#' capi(year=2022,datatype="acs", dataset="acs5", tableID="B01001",
#' variables=c("B01001_001","B01001_002"), geography="tract", filterAddress=v,
#' filterRadius=1, ggr=NULL, mode="summarize", filterSummary=FALSE,
#' filterSummaryLevels="root", state=NULL, county=NULL, tract=NULL,
#' block_group=NULL, verbose=TRUE, profile=FALSE, st=NULL)
#' }
#' 
capi <- function(year=NULL,
                 tableID=NULL,   
                 variables=NULL,
                 geography=NULL,
                 filterAddress=NULL,
                 filterRadius=NULL,
                 ggr=NULL,
                 mode="table",  
                 filterSummary=FALSE,
                 filterSummaryLevels="root",
                 filterByGeoType=NULL,
                 filterByGeoValue=NULL,
                 datatype="acs", 
                 dataset="acs5", 
                 state=NULL,
                 county=NULL,
                 tract=NULL,
                 block_group=NULL,
                 place=NULL,
                 metro=NULL,
                 consolidatedCity=NULL,
                 region=NULL,
                 division=NULL,
                 verbose=FALSE,
                 profile=FALSE,
                 fast=FALSE,
                 simpleReturn=FALSE,
                 test=FALSE, 
                 st=NULL){

  estimate <- moe <- vartype <- value <- name <- geoid <- NAME <-
    tot_pop <- tot_pop_pct <- `block group` <- 
    variable <- label <- table_id <- calculation <- concept <- 
    type <- subtotals <- varID <- pct <- moe_pct <- 
    data_type_1 <- data_type_2 <- data_type_3 <- data_type_4 <- 
    type_base <- subtotals_by_type <- pct_by_type <- moe_pct_by_type <-  
    TRACTCE <- COUNTYFP <- NULL

# Init --------------------------------------------------------------------
## Initial Error Checking --------------------------------------------------

  # Check for oddball inputs on TRUE/FALSE
  if(is.logical(filterSummary)==FALSE)stop("Invalid input for `filterSummary`. Must be TRUE/FALSE.")
  if(is.logical(profile)==FALSE)stop("Invalid input for `profile`. Must be TRUE/FALSE.")
  if(is.logical(verbose)==FALSE)stop("Invalid input for `verbose`. Must be TRUE/FALSE.")

  # if(is.null(state) && is.null(county) && is.null(filterAddress) && geography!="us" && geography!="state"){
  #   stop("!> No geography parameters provided. Try again.")
  # }
  if((!is.null(state) | !is.null(county)) && !is.null(filterAddress)){
    state <- NULL
    county <- NULL
    warning("!> Ambiguous parameters: state/county + filterAddress provided. Defaulting to filterAddress.")
  }
  if((!is.null(tract) | !is.null(block_group)) && is.null(state) && is.null(county)){
    stop("!> To select tract or block-level specifics, you need to include higher level geographical parameters (state/county) as well.")
  }
  if(!is.null(block_group) && is.null(tract)){
    stop("!> To select block-level specifics, you need to include tract number as well.")
  }
  if(geography=="us" && !is.null(filterAddress)){
    filterAddress <- NULL
    filterRadius <- NULL
    warning("!> Unnecessary use of filterAddress with geography=`us`. Ignoring.")
  }
  
  if((!is.null(filterByGeoType) | !is.null(filterByGeoValue)) & 
     (!is.null(filterAddress) | !is.null(filterRadius) | !is.null(county) | !is.null(tract))){
    stop("Too many competing geographic filters set: filterbyGeoType cannot be used in conjunction with filterAddress or county.")
  }

  if(is.null(geography)){
    stop("!> You must provide one of the following geography parameters: `us`,`state`,`county`,`tract`,`block group`,`place`,`region`,`division`,`subdivision`,`consolidated city`")
  }
  if(is.null(year)){
    stop("!> You must provide a year parameter.")
  }
  if(is.null(dataset)){
    stop("!> You must provide a dataset. Options are `acs1`, `acs3`, `acsse`, and `acs5`.")
  }
  if(geography %in% c("region","division","subminor civil division","place","consolidated city")){
    stop(paste("!> Unfortunately, we cannot provide results for `",geography,"` geographies at this time.",sep=""))
  }
  if(is.null(tableID) && is.null(variables)){
    stop("!> No variables or tableID included. You need to include either a tableID to capture all subvariables, a tableID with numeric vector, or a complete vector of variables. You can also include a named list with multiple tableIDs and variables.")
  }
  # Set default if null passed
  if(is.null(filterSummaryLevels)){
    filterSummaryLevels <- "root"
  }

## Data init ---------------------------------------------------------------

  # TODO Allow for caching of this data or not. Request permission.
  ACS <- acs_vars_builder(year=year,dataset=dataset)
  
  # Check for mode mismatch
  if(!is.null(tableID)){
    varcalccheck <- ACS[[2]] %>% filter(table_id %in% tableID)
    if(!is.na(match("median",varcalccheck$calculation))){
      stop("!> Summary tables do not work on median variables.")
    } 
  }

## Internal Functions ------------------------------------------------------
  
  ifelse(is.null(st),st <- Sys.time(),st <- st)

# Data Preparation -----------------------------------------------------------
   CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

 ## Run tableID check to grab prefix and path -------------------------------
   if(verbose==TRUE)message(paste(dur(st),"Running tableID check to grab prefix and path..."))
   
   if(!is.null(variables) && is.null(tableID)){
     tableID <- pseudo_tableID(variables,fast=TRUE,test=test,verbose=verbose)
   }
   
   tableID_pre.path <- tableID_pre_check(tableID)
   for(a in 1:length(tableID)){
     if(tableID_pre.path[[a]][1]=="NOTFOUND"){
       stop("!> There seems to be an error in your tableID. Check for typos.")
     }
   }
   if(tableID_pre.path[1]=="NO_TABLE_ID"){
     tableID <- pseudo_tableID(variables,test=test,verbose=verbose)
   }

    ## Build variable list with error checks -----------------------------------

   if(verbose==TRUE)message(paste(dur(st),"Building variable list with error checks..."))

   # Assign all variables logic if `all` is specified.
   if((is.null(variables) || length(variables)==1 && variables=="all")){ 
     if(is.null(tableID)){
       stop("!> To add all variables, you need to provide a tableID.")
     }
     variables <- (ACS[[1]] %>% filter(table_id %in% tableID))$name
     
   }else{
     if(verbose==TRUE)message(paste(dur(st),"Running varInputCheck()..."))
     viC <- varInputCheck(tableID=tableID,variables=variables)
     variables <- viC$variables
   }
   
   if(length(variables)==1 && variables!="all"){  ## Single entry
     varlist <- variable_formatter(var=variables,
                                   tipc=tableID_pre.path,
                                   tableID=tableID,
                                   ACS=ACS)
     varcount <- length(varlist)
     chunks <- 1 # Default chunk number (for sizing below)
     chunksize <- 48
     chunksizes <- varcount # Default chunksizes number
     varlist <- stringr::str_flatten(varlist,collapse=",")
    }else{  ## Multiple entry
     varlist <- NULL
    for(i in 1:length(variables)){
      varlist <- c(varlist,variable_formatter(var=variables[i],
                                              tipc=tableID_pre.path,
                                              ACS=ACS))
     }
    # For later reshaping, find total number of varlist, and break into chunks if > 50
    varcount <- length(varlist)

    # Census API has a limit of 50 variables at a time. We need to break the 
    # variable list down into serviceable chunks. 
    if(verbose==TRUE)message(paste(dur(st),"Checking variable length and trimming if needed..."))

    chunks <- 1 # Default chunk number (for sizing below)
    chunksize <- 48
    chunksizes <- varcount # Default chunksizes number
    
    if(varcount>chunksize){
      chunks <- ceiling(varcount/chunksize)
      vchunks <- NULL
      chunksizes <- NULL
      for(c in 1:chunks){
        s <- 1+chunksize*(c-1) # start parameter for chunk. Limiting to 48 for buffer.
        ifelse(chunks==c,e <- varcount,e <- chunksize*c) # end parameter for chunk
        vc <- varlist[s:e]
        vc <- stringr::str_flatten(vc,collapse=",")
        vchunks <- c(vchunks,vc)
        chunksizes[paste("chunk_",c,sep = "")] <- e-(chunksize*(c-1))
      }
      varlist <- vchunks
    }else{
      varlist <- stringr::str_flatten(varlist,collapse=",") 
     }
    }

## TableID checks ----------------------------------------------------------
   
   if(!is.null(variables) && is.null(tableID)){
     # We engage pseudo_tableID down in reshape section.
   }else{
     # Check both directions.
     ptid <- pseudo_tableID(variables,test=test,verbose=verbose)
     for(j in 1:length(tableID)){
       if(!(tableID[j] %in% ptid))stop(paste("!> It appears there is a variable/tableID mismatch. `",tableID[j],"` does not represent every variable given.",sep=""))
     }
     for(k in 1:length(ptid)){
       if(!(ptid[k] %in% tableID))stop("!> It appears there is a variable/tableID mismatch. More tableIDs in variables than listed explicitly in tableID parameter.")   
     }
    }
 
 ## Geography Functionality -------------------------------------------------

  fips <- tigris::fips_codes

  ### Check and format state as code ---------------------------------------
 
  if(!is.null(state)){
    if(verbose==TRUE)message(paste(dur(st),"Checking and formatting state as code..."))
    
    # Reclassify a character number 
    if(is.character(state)==TRUE){
      if(grepl("[0-9]",state)==TRUE){
        state <- as.numeric(state)
      }
    }
    
    if(is.numeric(state)){
      state <- sprintf("%02d",state)
      stateLookup <- match(state,unique(fips$state_code))
      if(is.na(stateLookup)){
        stop(paste("!> `",state,"` is not a valid FIPS code for state.",sep=""))
      }
    }else{
      if(nchar(state)==2){
        state <- stringr::str_to_upper(state)
        stateLookup <- match(state,unique(fips$state))
        if(is.na(stateLookup)){
          stop(paste("!> `",state,"` is not a valid abbreviation for state.",sep=""))
        }
        state <- unique(fips$state_code)[stateLookup]
      }else if(nchar(state)>2){
        #state <- stringr::str_to_title(state) 
        stateLookup <- fips[grep(state,fips$state_name,ignore.case = TRUE),]
        if(nrow(stateLookup)==0){
          stop(paste("!> `",state,"` is not a valid state name.",sep=""))
        }
        state <- unique(stateLookup$state_code)
        if(length(state)>1){
          stop(paste("!> `",state,"` is an ambiguous state name",sep=""))
        }
      }else{
        stop(paste("!> `",state,"` is not a valid state entry.",sep=""))
      }
    }
  }
  
  ### Check and format county as code ---------------------------------------
  if(!is.null(county)){
    if(verbose==TRUE)message(paste(dur(st),"Checking and formatting county as code..."))
    if(is.null(state)){
      stop("!> You must include a state, if filtering by county.")
    }

    # Begin checks and formatting 
    countymerge <- NULL
    for(i in 1:length(county)){ 
      # Account for character numeric versions
    if(grepl('[0-9]',(county[i]))==TRUE){
      countyformatted <- sprintf("%03d",as.numeric(county[i]))
      countyLookup <- fips[fips$state_code==state & 
                             fips$county_code==countyformatted,]
      countyLookup <- countyLookup$county_code
      if(is.na(countyLookup)){
        stop(paste("!> `",county[i],"` is not a valid FIPS code for county."))
      }
    }else{
      if(nchar(county[i])>2){
        #county <- stringr::str_to_title(county)
        countyLookup <- fips[grep(county[i],fips$county,ignore.case = TRUE),]
        if(nrow(countyLookup)==0){
          stop(paste("!> `",county[i],"` is not a valid county name."))
        }
        countyLookup <- unique(countyLookup$county_code)
        if(length(countyLookup)>1){
          stop(paste("!> `",county[i],"` is an ambiguous county name."))
        }
      }else{
        stop(paste("!> `",county[i],"` is not a valid county entry."))
      }
    }
      if(is.numeric(countyLookup)){
        countyLookup <- sprintf("%03d",countyLookup)
      }
      countymerge <- c(countymerge,countyLookup)
    }
    county <- paste(countymerge,collapse=",")
  }
   
  ### Get tracts and modify counties if cross-county tracts ------------------
   if(!is.null(tract)){
     if(verbose==TRUE)message(paste(dur(st),"Checking and formatting tract as code..."))
     geos <- geo_var_builder(geography="tract",
                             try="local",
                             state=state,
                             county=county,
                             geosObject=NULL,
                             verbose=FALSE,
                             test=FALSE)
     tracts_filt <- geos$geo_tracts %>% filter(TRACTCE %in% tract)
     new_counties <- unique(tracts_filt$COUNTYFP)
     tract_pack <- list()
     for(i in 1:length(new_counties)){
       tp <- tracts_filt %>% filter(COUNTYFP %in% new_counties[i])
       tract_pack[[new_counties[i]]] <- tp$TRACTCE
     }
     county <- new_counties
   }


  ### Get geography filters if address and radius or geo type is supplied. ---------------
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

    if(geography=="state"){
      state <- stringr::str_flatten(unique(ggr$states),collapse=",")
    }else if(geography=="county"){
      state <- stringr::str_flatten(unique(ggr$states),collapse=",")
      county <- stringr::str_flatten(unique(ggr$counties),collapse=",")
    }else if(geography=="tract"){
      state <- stringr::str_flatten(unique(ggr$states),collapse=",")
        # Keep unflattened for tract call, due to loops.
        county <- ggr$counties
        tract <- "*" # > Filtering happens after the data call to simplify the call itself.
        #tract <- stringr::str_flatten(unique(ggr$tracts),collapse=",")
      }else if(geography=="block group"){
      state <- stringr::str_flatten(unique(ggr$states),collapse=",")
      county <- stringr::str_flatten(unique(ggr$counties),collapse=",")
      tract <- stringr::str_flatten(unique(ggr$tracts),collapse=",")
      if(length(ggr$tracts>1)){
        block_group <- "*"
      }else{
        block_group <- stringr::str_flatten(ggr$block_groups,collapse=",")
      }
    }else if(geography=="metro" || geography=="msa"){
      metro <- metro
    }else{
      stop("!> No geography provided.")
    }
    
  }else{
  ## Assign "*" if geography is NULL and required. Otherwise, flatten to allow for multiple entries.
    ## TODO This isn't very elegant. I think I can clean this up.
    
      if(is.null(state)){
        state <- "*"
      }else{
        state <- stringr::str_flatten(unique(state),collapse=",")
      }
      if(is.null(county)){
        county <- "*"
      }else{
        if(geography=="tract" || geography=="block group"){
          # Keep unflattened for tract call, due to loops.
          county <- county
        }else{ 
          county <- stringr::str_flatten(unique(county),collapse=",")
        }
      }
      if(is.null(tract)){
        tract <- "*"
      }else{
        tract <- stringr::str_flatten(unique(tract),collapse=",")
      }
      if(is.null(block_group)){
        block_group <- "*"
      }else{
        block_group <- stringr::str_flatten(unique(block_group),collapse=",")
      }
     if(is.null(metro)){
       metro <- "*"
     }else{
       metro <- stringr::str_flatten(unique(metro),collapse=",")
     }
  }

# Prepare API Call --------------------------------------------------------
## Create GET URL ----------------------------------------------------------

  if(verbose==TRUE)message(paste(dur(st),"Building URL for GET call..."))
  url <- "http://api.census.gov/"
  path <- paste("data/",year,"/",datatype,"/",dataset,sep="")
  #varlist <- toString(varlist)

  ## Loops to handle multiple counties
  datac <- NULL
  for(i in 1:length(county)){ 
  
  ingeo <- NULL
  if(geography=="us"){
    forgeo <- paste("us",sep="")
  }else if(geography=="state"){
    forgeo <- paste("state:",state,sep="")
  }else if(geography=="county"){
    forgeo <- paste("county:",county[i],sep="")
    ingeo <- paste("state:",state,sep="")
  }else if(geography=="tract"){
    forgeo <- paste("tract:",tract,sep="")
    ingeo <- paste("state:",state,"&in=county:",county[i],sep="")
  }else if(geography=="block group"){
    forgeo <- paste("block group:",block_group,sep="")
    ingeo <- paste("state:",state,"&in=county:",county[i],"&in=tract:",tract,sep="")
  }else if(geography=="county subdivision"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
    stop("!> Unable to provide this geography at this time.")
    forgeo <- paste("county subdivision")
    ingeo <- paste("state:",state,"&in=county:",county[i],sep="")
  }else if(geography=="metro" || geography=="msl"){
    forgeo <- paste("metropolitan statistical area/micropolitan statistical area:",metro,sep="")
  }else if(geography=="subminor civil division"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
  }else if(geography=="place"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
    # forgeo <- paste("place")
    # ingeo <- paste("state:",state,"&in=place:",place,sep="")
  }else if(geography=="consolidated city"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
    # forgeo <- paste("consolidated city")
    # ingeo <- paste("state:",state,"&in=consolidated city:",consolidatedCity,sep="")
  }else if(geography=="region"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
    # if(is.null(region))region <- "*"
    # forgeo <- paste("region:",region,sep="")
  }else if(geography=="division"){
    ##TODO Update get_geo_radius() / geo_var_builder() to account for this geography.
    # if(is.null(division))division <- "*"
    # forgeo <- paste("division:",division,sep="")
  }else{
    stop("!> No geography provided.")
  }
  
## Make GET Call -----------------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"GET CALL: ",url,path,"?get=",paste(varlist,",NAME",sep=""),"&for=",forgeo,"&in=",ingeo,sep=""))
  
  if(verbose==TRUE)message(paste(dur(st),"Initiate GET call..."))
  
  # Run GET on number of chunks if large number of variables 
  data <- list()
  for(d in 1:chunks){
  if(verbose==TRUE)message(paste(dur(st)," GET call #",d,"...",sep=""))
   if(is.null(ingeo)){

     dt <- httr::GET(url,
                      path=path,
                      query=list(get = paste(varlist[d],",NAME",sep=""),
                                 "for" = forgeo,
                                 key = CENSUS_API_KEY)
                      )
   }else{
     dt <- httr::GET(url,
                      path=path,
                      query=list(get = paste(varlist[d],",NAME",sep=""),
                                 "for" = forgeo,
                                 "in" = ingeo,
                                 key = CENSUS_API_KEY)
                                 )
   }
   if(httr::status_code(dt)!=200){
     ##ADDTEST?
     stop(paste("!> There was an error in the GET call / json text. ERROR",httr::status_code(dt)))
   }

  if(verbose==TRUE)message(paste(dur(st),"Cleaning data and formatting to dataframe..."))
   dt <- httr::content(dt,as="text")
   
   dt <- jsonlite::fromJSON(dt)
   dt <- as.data.frame(dt)
   colnames(dt) <- dt[1,]
   dt <- dt[-1,]

   if(verbose==TRUE)message(paste(dur(st),"Reshaping data..."))
   dt <- tidyr::pivot_longer(dt,cols=1:chunksizes[d],names_to="variable")
   
   # Combine chunk loops
   if(chunks>1){
     data <- rbind(data,dt)
   }else{
     data <- dt
   }
  }
  # Combine county loops
  if(i>1){
    datac <- rbind(datac,data)
  }else{
    datac <- data
  }
  }
  data <- datac
  
  if(simpleReturn==TRUE)return(data)
# Reshape and clean combined data --------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Reshaping and cleaning combined data..."))
  
    ## NA Replacement
    nas <- c(-111111111,
             -222222222,
             -333333333,
             -444444444,
             -555555555,
             -666666666,
             -777777777,
             -888888888,
             -999999999)
    
    data$value <- as.numeric(data$value)
    data <- data %>% mutate(vartype = ifelse(stringr::str_sub(data$variable,-1,-1)=="E","estimate","moe"))
    data <- data %>% mutate(variable=stringr::str_sub(data$variable,1,-2))
    data <- data %>% tidyr::pivot_wider(names_from = vartype,values_from = value)
    data$estimate <- as.numeric(data$estimate)
    data$moe <- as.numeric(data$moe)
    data <- data %>% mutate(moe = ifelse(moe %in% nas,NA,moe))

    if(verbose==TRUE)message(paste(dur(st),"Adding geography..."))
    
    if(geography=="block group"){
      data <- data %>% rename(block_group = `block group`)
    }
    
    if(geography=="state"){
      data <- data %>% mutate(geoid = paste(state,sep=""))
    }else if(geography=="county"){
      data <- data %>% mutate(geoid = paste(state,county,sep=""))
    }else if(geography=="tract"){
      data <- data %>% mutate(geoid = paste(state,county,tract,sep=""))
    }else if(geography=="block group"){
      data <- data %>% mutate(geoid = paste(state,county,tract,block_group,sep=""))
    }else{
      data <- data %>% mutate(geoid = NA)
    }
    
    ## For filterAddress and filterByGeoType, perform data filtering now,
    if(verbose==TRUE)message("Filtering data...")
    if((!is.null(filterAddress) | !is.null(filterByGeoType)) && (geography=="tract") || geography=="block group"){
       data <- data %>% filter(data$geoid %in% ggr$geoid)  
    }
    
    if(verbose==TRUE)message(paste(dur(st),"Adding tableID..."))
    
#    if(!is.null(tableID)){
#      data <- data %>% dplyr::mutate(table_id = pseudo_tableID)
#    }else{
      tableID <- pseudo_tableID(data$variable,fast=fast,test=test,verbose=verbose)
      data <- data %>% dplyr::mutate(table_id = tableID)
#     }
    data <- data %>% dplyr::rename(name = NAME)
    
    data <- data %>% dplyr::arrange(name,variable)
    
    ## Label attachment --------------------------------------------------------
   
     if(verbose==TRUE)message(paste(dur(st),"Attaching variable/tableID labels..."))
    
    ACSV <- ACS[[1]] %>% mutate(labels = str_split_i(label,"!!",-1)) %>% 
      dplyr::select(name,concept,labels,calculation,type,type_base,varID)
    
    # Create columnn for labels, for easy plotting.
    data <- left_join(data,ACSV,by=c("variable"="name"))
    
    if(verbose==TRUE)message(paste(dur(st),"Adding proportions.."))
    
    # Add proportions relative to table_id, geoid, and type.
    data <- data %>% dplyr::mutate(geography=geography)
   data <- data %>% dplyr::group_by(table_id,geoid,type_base) %>% mutate(subtotals=sum(estimate))
    
    data <- data %>% dplyr::group_by(table_id,geoid,type_base) %>% mutate(subtotals=sum(estimate))
    data <- data %>% dplyr::mutate(pct = estimate/subtotals)
    data <- data %>% dplyr::mutate(moe_pct = moe/estimate)
    data <- data %>% dplyr::ungroup()
      data <- data %>% dplyr::group_by(table_id,geoid) %>% mutate(tot_pop=max(estimate))
    data <- data %>% dplyr::mutate(tot_pop_pct = estimate/tot_pop)
    data <- data %>% dplyr::ungroup()
    data <- data %>% dplyr::group_by(table_id,geoid,type) %>% mutate(subtotals_by_type=sum(estimate))
    data <- data %>% dplyr::mutate(pct_by_type = estimate/subtotals_by_type)
    data <- data %>% dplyr::mutate(moe_pct_by_type = moe/estimate)
    data <- data %>% dplyr::mutate(year=year)

    if(verbose==TRUE)message(paste(dur(st),"Relocating columns..."))

    # Rearrange columns to make it more readable.
    data <- data %>% dplyr::relocate(table_id) %>% 
      dplyr::relocate(year,.after=table_id) %>% 
      dplyr::relocate(variable,.after=year) %>% 
      dplyr::relocate(concept,.after=variable) %>% 
      dplyr::relocate(labels,.after=concept) %>% 
      dplyr::relocate(estimate,.after=labels) %>% 
      dplyr::relocate(geography,.after=estimate) %>% 
      dplyr::relocate(moe,.after=geography) %>% 
      dplyr::relocate(subtotals,.after=moe) %>% 
      dplyr::relocate(pct,.after=subtotals) %>% 
      dplyr::relocate(tot_pop,.after=pct) %>% 
      dplyr::relocate(tot_pop_pct,.after=tot_pop) %>% 
      dplyr::relocate(moe_pct,.after=tot_pop_pct) %>% 
      dplyr::relocate(subtotals_by_type,.after=moe_pct) %>% 
      dplyr::relocate(pct_by_type,.after=subtotals_by_type) %>% 
      dplyr::relocate(moe_pct_by_type,.after=pct_by_type)
    
    ##TODO Make this one function for all processing
    relocation_fun <- function(data){
      data <- data %>% dplyr::relocate(calculation,.after="geography") %>% 
        dplyr::relocate(type,.after=calculation) %>% 
        dplyr::relocate(varID,.after=type) 
    }
    
    data <- relocation_fun(data)
    
    if(verbose==TRUE)message(paste(dur(st),"Creating type 1 data..."))
    data_type_1 <- data
    data_type_1 <- data_type_1 %>% mutate(dt=1)

## Filter summaries --------------------------------------------------------
    if(verbose==TRUE)message(paste(dur(st),"Creating type 2 data..."))
  ## This is `type 2 data`
    if(filterSummary==TRUE || profile==TRUE){
      
      ## Remove bottom specified summary levels.
      levels <- c("root","summary","level_1","level_2","level_3","level_4")
      n <- 1
      # Start w/ baseline
      fsl <- levels[n]
      # Iteratively build filter list if not root
      while(levels[n]!=filterSummaryLevels){
        n <- n+1
        fsl <- levels[1:n]
      }
      
      data_type_2 <- data_type_1 %>% filter(!(type %in% fsl))
      data_type_2 <- data_type_2 %>% mutate(dt=2)
    }

 ## Up to this point ^^^ is `type 1/2` data.
  
## Mode adjustments --------------------------------------------------------

    if(verbose==TRUE)message(paste(dur(st),"Creating type 3/4 data..."))
      
      if(mode=="summarize" || profile==TRUE){
        if(filterSummary==FALSE || profile==TRUE){
          data_type_3 <- summarize_data(data_type_1,
                                        geography=geography,
                                        filterRadius = filterRadius,
                                        state=state,
                                        county=county,
                                        tract=tract,
                                        block_group=block_group)
          
        #  data_type_3 <- relocation_fun(data_type_3)
          data_type_3 <- data_type_3 %>% mutate(dt=3)
        }
        if(filterSummary==TRUE || profile==TRUE){
          data_type_4 <- summarize_data(data_type_2,
                                        geography=geography,
                                        filterRadius = filterRadius,
                                        state=state,
                                        county=county,
                                        tract=tract,
                                        block_group=block_group)
        #  data_type_4 <- relocation_fun(data_type_4)
          data_type_4 <- data_type_4 %>% mutate(dt=4)
        }
      }
  
# Finish and return -------------------------------------------------------

  if(verbose==TRUE)message(paste(dur(st),"Done."))
  
  if(profile==FALSE){
    if(mode=="table" && filterSummary==FALSE)data <- data_type_1
    if(mode=="table" && filterSummary==TRUE)data <- data_type_2
    if(mode=="summarize" && filterSummary==FALSE)data <- data_type_3
    if(mode=="summarize" && filterSummary==TRUE)data <- data_type_4
    
  }else{
    data <- list(type1data = data_type_1,
                 type2data = data_type_2,
                 type3data = data_type_3,
                 type4data = data_type_4
                 )
  }
  return(data)
}