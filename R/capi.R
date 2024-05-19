#' Census API Data Call
#'
#' An API interface for capturing and formatting census data.
#'
#' @param year Year for data call.
#' @param tableID Formerly known as varBase, or concept, or group: i.e., "B01001"
#' @param variables A vector of variables for the call. If multiple select variables per tableID are desired,
#' then variables should be constructed as a named list, with tableID as name, and sub list items as 
#' variables—either full ("B01001_001") or numeric (c(1:8)). 
#' @param geography Specifying geography: e.g., "tract", "county"
#' @param filterAddress An address input used to generate a radius around, for filtering data.
#' @param filterRadius A numeric value specifying the radius in miles around the address.
#' @param coords An sf coordinate object for get_geocode_radius.
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
#' @param puma Input (abb. or FIPS) of puma (public use microdata area) for search.
#' @param dataset_main Selection parameters for get_census_variables (e.g. "acs")
#' @param dataset_sub Selection parameters for get_census_variables (e.g. "acs5")
#' @param dataset_last Selection parameters for get_census_variables (e.g. "cprofile")
#' @param censusVars Passthrough object to bypass get_census_variables 
#' @param verbose Logical parameter to specify whether to produce verbose output.
#' @param profile Logical parameter to specify whether to build profile.
#' @param ggr Internal: to pass a get_geocode_radius() object to function.
#' @param geosObject Optional, attach geos object to simplify geo processes.
#' @param filterSummary Logical parameter to specify whether to filter out summary levels (typically _001 and therefore "root").
#' @param filterSummaryLevels Explicit description of lowest type denoting summary level. Also excludes lower levels.
#' @param fast Internal parameter for stat table
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
#' @importFrom stringr str_count
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
                 coords=NULL,
                 ggr=NULL,
                 geosObject=NULL,
                 mode="table",  
                 filterSummary=FALSE,
                 filterSummaryLevels="root",
                 filterByGeoType=NULL,
                 filterByGeoValue=NULL,
                 state=NULL,
                 county=NULL,
                 tract=NULL,
                 block_group=NULL,
                 place=NULL,
                 metro=NULL,
                 consolidatedCity=NULL,
                 region=NULL,
                 division=NULL,
                 puma=NULL,
                 dataset_main="acs",
                 dataset_sub="acs5",
                 dataset_last=NULL,
                 censusVars=NULL,
                 verbose=FALSE,
                 profile=FALSE,
                 fast=FALSE,
                 simpleReturn=FALSE,
                 test=FALSE, 
                 st=NULL){

  estimate <- moe <- vartype <- value <- name <- geoid <- NAME <-
    subtotal <- pct <- `block group` <- 
    variable <- label <- table_id <- calculation <- concept <- 
    type <- subtotals <- varID <- pct <- moe_pct <- 
    data_type_1 <- data_type_2 <- data_type_3 <- data_type_4 <- 
    type_base <- subtotal_by_type <- pct_by_type <- moe_pct_by_type <-  
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
  if(is.null(dataset_main)){
    stop("!> You must provide a dataset. Options are `acs` and subsidiaries.")
  }
  if(geography %in% c("region","division","subminor civil division","consolidated city")){
    stop(paste("!> Unfortunately, we cannot provide results for `",geography,"` geographies at this time.",sep=""))
  }
  if(is.null(tableID) && is.null(variables)){
    stop("!> No variables or tableID included. You need to include either a tableID to capture all subvariables, a tableID with numeric vector, or a complete vector of variables. You can also include a named list with multiple tableIDs and variables.")
  }
  if(geography=="place" && is.null(state)){
    stop("!> You must provide a state along with place geography designation.")
  }
  
  # Check for minimum year
  if(length(year)>1){
    if(min(year)<2009 & dataset_main=="acs"){
      stop("!> The ACS only provides data from 2009 on.")
    }
  }else{
    if(year<2009 & dataset_main=="acs"){
        stop("!> The ACS only provides data from 2009 on.")
      }
  }
  
  # Set default if null passed
  if(is.null(filterSummaryLevels)){
    filterSummaryLevels <- "root"
  }
  
  if(!is.null(dataset_last)){
    if(dataset_last=="pums"){
      if(!(geography %in% c("puma","region","division","state"))){
        stop(paste("!> ",geography," is not an acceptable geography for pums data. Use region, division, or state.",sep=""))
      }
      if(!is.null(puma) & geography!="puma"){
        stop("!> To select PUMs area by PUM id, please select geography=puma.")
      }
      if(geography=="puma" & dataset_sub!="acs1"){
        stop("!> PUMAs require ACS 1-year data.")
      }
    }
  }

## Data init ---------------------------------------------------------------

  # TODO Allow for caching of this data or not. Request permission.
  if(verbose==TRUE)message("Getting CV...")
  if(is.null(censusVars)){ 
    CV <- get_census_variables(year=year, 
                               dataset_main = dataset_main, 
                               dataset_sub = dataset_sub, 
                               dataset_last = dataset_last)
  }else{
    CV <- censusVars
  }

## Internal Functions ------------------------------------------------------
  
  ifelse(is.null(st),st <- Sys.time(),st <- st)

# Data Preparation -----------------------------------------------------------
   CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

 ## Run tableID_variable_preflight check to check variables and tableIDs, and 
 ## clean up lists -------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Running tableID_variable_preflight..."))
  

    preflight_check <- tableID_variable_preflight(tableID = tableID,
                                                  variables = variables,
                                                  censusVars = CV,
                                                  verbose = verbose)
    
    tableID <- preflight_check$tableID
    variables <- preflight_check$variables
   
  # Census API has a limit of 50 variables at a time. We need to break the 
  # variable list down into serviceable chunks. 
  if(verbose==TRUE)message(paste(dur(st),"Checking variable length and trimming if needed..."))
  # For later reshaping, find total number of variables, and break into chunks if > 50
  varcount <- length(variables)
  chunks <- 1 # Default chunk number (for sizing below)
  chunksize <- 48
  chunksizes <- varcount # Default chunksizes number
  
  if(varcount>chunksize){
    chunks <- ceiling(varcount/chunksize)
    vchunks <- NULL
    chunksizes <- NULL
    for(c in 1:chunks){
      s <- 1+chunksize*(c-1) # start parameter for chunk. Limiting to 48 for buffer.
      ifelse(chunks==c, e <- varcount, e <- chunksize*c) # end parameter for chunk
      vc <- variables[s:e]
      vc <- paste(vc,collapse=",")
      vchunks <- c(vchunks,vc)
      chunksizes[paste("chunk_",c,sep = "")] <- e-(chunksize*(c-1))
    }
    varlist <- vchunks
  }else{
    varlist <- paste(variables,collapse=",") 
  }

## TableID checks ----------------------------------------------------------
   
   # if(!is.null(variables) && is.null(tableID)){
   #   # We engage pseudo_tableID down in reshape section.
   # }else{
   #   # Check both directions.
   #   ptid <- pseudo_tableID(variables,test=test,verbose=verbose)
   #   for(j in 1:length(tableID)){
   #     if(!(tableID[j] %in% ptid))stop(paste("!> It appears there is a variable/tableID mismatch. `",tableID[j],"` does not represent every variable given.",sep=""))
   #   }
   #   for(k in 1:length(ptid)){
   #     if(!(ptid[k] %in% tableID))stop("!> It appears there is a variable/tableID mismatch. More tableIDs in variables than listed explicitly in tableID parameter.")   
   #   }
   #  }
 
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
     if(is.null(geosObject)){
       geosObject <- geo_var_builder(geography="tract",
                               try="local",
                               state=state,
                               county=county,
                               geosObject=NULL,
                               verbose=FALSE,
                               test=FALSE)
     }
     
     tracts_filt <- geosObject$geo_tracts %>% filter(TRACTCE %in% tract)
     new_counties <- unique(tracts_filt$COUNTYFP)
     tract_pack <- list()
     for(i in 1:length(new_counties)){
       tp <- tracts_filt %>% filter(COUNTYFP %in% new_counties[i])
       tract_pack[[new_counties[i]]] <- tp$TRACTCE
     }
     county <- new_counties
   }


  ### Get geography filters if address and radius or geo type is supplied. ---------------
   if(!is.null(filterAddress) | !is.null(filterByGeoType) | !is.null(coords)){
     if(!is.null(filterAddress) | !is.null(coords)){ 
       if(verbose==TRUE)message(paste(dur(st),"Filtering geosObject by address and radius..."))
       
       if(is.null(ggr)){
         if(verbose==TRUE)message(paste(dur(st),"Finding geo area by radius..."))
         ggr <- get_geocode_radius(filterAddress = filterAddress,
                                   filterRadius = filterRadius,
                                   coords = coords,
                                   geography = geography,
                                   geosObject = geosObject,
                                   year = year,
                                   fipsOnly = TRUE)
       }
     }else{
       if(is.null(ggr)){
         if(verbose==TRUE)message(paste(dur(st),"Finding geo area by radius..."))
         ggr <- get_geocode_radius(filterByGeoType = filterByGeoType,
                                   filterByGeoValue = filterByGeoValue,
                                   coords = coords,
                                   geography = geography,
                                   state = state,
                                   geosObject = geosObject,
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
      # state <- stringr::str_flatten(unique(ggr$states),collapse=",")
      # county <- stringr::str_flatten(unique(ggr$counties),collapse=",")
      # tract <- stringr::str_flatten(unique(ggr$tracts),collapse=",")
      # Keep unflattened for tract call, due to loops.
      state <- ggr$states
      county <- ggr$counties
      tract <- "*" # > Filtering happens after the data call to simplify the call itself.
      if(length(ggr$tracts>1)){
        block_group <- "*"
      }else{
        block_group <- stringr::str_flatten(ggr$block_groups,collapse=",")
      }
    }else if(geography=="metro" || geography=="msa"){
      metro <- metro
    }else if (geography=="place"){
      if(length(ggr$places>1)){
        place <- stringr::str_flatten(ggr$places,collapse=",")
      }else{
        place <- ggr$place
      }
    }else{
      stop("!> No geography provided.")
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
    if(length(ggr$places>1)){
      place <- stringr::str_flatten(ggr$places,collapse=",")
    }else{
      place <- ggr$place
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
    if(!is.null(puma)){
      if(length(puma)>1){
        puma <- paste(puma,collapse=",")
      }
    }
    
  }

# Prepare API Call --------------------------------------------------------
## Create GET URL ----------------------------------------------------------

   if(verbose==TRUE)message(paste(dur(st),"Building URL for GET call..."))
   
   ## Loops to handle multiple years
   for(t in 1:length(year)){
     
     url <- "http://api.census.gov/"
     pathElements <- c("data",year[t],dataset_main,dataset_sub,dataset_last,"variables")
     path <- paste(pathElements,collapse="/")
     
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
         forgeo <- paste("place:",place,sep="")
         ingeo <- paste("state:",state,sep="")
       }else if(geography=="puma"){
         forgeo <- "public use microdata area:*"
         ingeo <- paste("state:",state,sep="")
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
       if(verbose==TRUE){
         message(paste(dur(st),
                       "GET CALL: ",
                       url,
                       path,
                       "?get=",
                       paste(varlist,
                             ifelse(CV$type=="simple" | !is.null(puma),"",",NAME"),
                             sep=""),
                       "&for=",
                       forgeo,
                       ifelse(CV$type=="complex" | geography=="puma","&in=",""),
                       ifelse(CV$type=="complex" | geography=="puma",ingeo,""),
                       sep=""))}
       
       if(verbose==TRUE)message(paste(dur(st),"Initiate GET call..."))
       
       # Run GET on number of chunks if large number of variables 
       data <- list()
       for(d in 1:chunks){
         if(verbose==TRUE)message(paste(dur(st)," GET call #",d,"...",sep=""))
         if(is.null(ingeo)){
           
           dt <- httr::GET(url,
                           path=path,
                           query=list(get = paste(varlist[d],
                                                  ifelse(CV$type=="simple" | geography=="puma","",",NAME"),
                                                  sep=""),
                                      "for" = forgeo,
                                      key = CENSUS_API_KEY)
           )
         }else{
           dt <- httr::GET(url,
                           path=path,
                           query=list(get = paste(varlist[d],
                                                  ifelse(CV$type=="simple" | geography=="puma","",",NAME"),
                                                  sep=""),
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
         
         if(geography=="puma"){
           dt$index <- 1:nrow(dt)
           dt <- dt %>% relocate(index,.before = "state")
           names(dt)[names(dt)=="public use microdata area"] <- "puma"
         }
      
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
     
     # Add year column
     data <- data %>% mutate(year=year[t])
     # Combine year loops 
     if(t>1){
       datat <- rbind(datat,data)
     }else{
       datat <- data
     }
     data <- datat
   }  

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
    
    ### Initial data processing --------------------------
    if(verbose==TRUE)message(paste(dur(st),"Initial data processing..."))
    if(geography=="block group"){
      data <- data %>% dplyr::rename(block_group = `block group`)
    }
    if(geography=="state"){
      data <- data %>% dplyr::mutate(geoid = paste(state,sep=""))
    }else if(geography=="county"){
      data <- data %>% dplyr::mutate(geoid = paste(state,county,sep=""))
    }else if(geography=="tract"){
      data <- data %>% dplyr::mutate(geoid = paste(state,county,tract,sep=""))
    }else if(geography=="block group"){
      data <- data %>% dplyr::mutate(geoid = paste(state,county,tract,block_group,sep=""))
    }else if(geography=="puma"){
      data <- data %>% dplyr::mutate(geoid = paste(state,puma,sep=""))
    }else{
      data <- data %>% dplyr::mutate(geoid = NA)
    }

    ### If filterAddress and filterByGeoType, perform data filtering now-------
    if(verbose==TRUE)message("Filtering data...")
    if((!is.null(filterAddress) | !is.null(filterByGeoType)) & (geography=="tract") || geography=="block group"){
        if(!is.null(ggr$geoid)){
          data <- data[data$geoid %in% ggr$geoid,] 
        }
    }
    if(!is.null(puma)){
      puma <- sprintf("%05d",puma)
      if(!(puma %in% data$puma)){
        stop("!> That puma reference number does not exist in the dataset.")
      }else{
        data <- data[data$puma %in% puma,]
      }
    }

    data$value <- as.numeric(data$value)
    
    if(!is.null(dataset_last) && dataset_last=="pums"){
      data <- data %>% dplyr::mutate(value = ifelse(value %in% nas,NA,value))
    }else{
      
      data <- data %>% dplyr::mutate(vartype = ifelse(stringr::str_sub(data$variable,-1,-1)=="E","estimate","moe"))
      data <- data %>% dplyr::mutate(variable=stringr::str_sub(data$variable,1,-2))
      data <- data %>% tidyr::pivot_wider(names_from = vartype,values_from = value)
      data$estimate <- as.numeric(data$estimate)
      data$moe <- as.numeric(data$moe)
      data <- data %>% dplyr::mutate(estimate = ifelse(estimate %in% nas,NA,estimate),
                                     moe = ifelse(moe %in% nas,NA,moe))
    }
    
    ### Format and add helpful data--------------------------
    if(!is.null(dataset_last) && dataset_last=="pums"){
      ### Add Labels + Concept ---------------------------------
      if(verbose==TRUE)message(paste(dur(st),"Attaching variable/tableID labels..."))
      CVV <- CV$variables %>% dplyr::select(name,label,var_type)
      data <- left_join(data,CVV,by=c("variable"="name"))
      
      ## Create type_7_data (straight data)
      if(verbose==TRUE)message(paste(dur(st),"Creating type 7 data..."))
      data_type_7 <- data
      data_type_7 <- data_type_7 %>% mutate(dt=7)
      attr(data_type_7,"dataType") <- 7
      
      ## Create type_8_data (summary)
      if(verbose==TRUE)message(paste(dur(st),"Creating type 8 data..."))
      data_type_8 <- data %>% group_by(variable,value) %>% 
        summarize(subtotal = sum(value))
      data_type_8 <- data_type_8 %>% mutate(dt=8)
      attr(data_type_8,"dataType") <- 8
      
    }else{
      if(verbose==TRUE)message(paste(dur(st),"Adding tableID..."))
      tableID <- tableID_variable_preflight(variables=data$variable,
                                            censusVars = CV,
                                            verbose=verbose)
      tableID <- tableID$tableID
      data <- data %>% dplyr::mutate(table_id = tableID)
      
      ### Add Variable--------------------------
      if(verbose==TRUE)message(paste(dur(st),"Adding variable..."))
      data <- data %>% dplyr::rename(name = NAME)
      data <- data %>% dplyr::arrange(name,variable)
      
      ### Add Year + relocate variable-------------------------- 
      if(verbose==TRUE)message(paste(dur(st),"Adding year + relocating variable..."))
      if(length(year)==1)data <- data %>% dplyr::mutate(year=year)
      data <- data %>% dplyr::relocate(variable, .after = year)
      
      ### Add Labels + Concept ---------------------------------
      if(verbose==TRUE)message(paste(dur(st),"Attaching variable/tableID labels..."))
      CVV <- CV[[1]] %>% dplyr::mutate(labels = str_split_i(label,"!!",-1))
      CVV1 <- CVV %>% dplyr::select(name,concept,labels,calculation,type,type_base,varID)
      # CVV2 <- CVV %>% dplyr::select(name,calculation,type,type_base,varID)
      data <- left_join(data,CVV1,by=c("variable"="name"))
      
      ### Add Subtotals + Proportions ---------------------
      if(verbose==TRUE)message(paste(dur(st),"Adding subtotals + proportions..."))
      data <- data %>% dplyr::relocate(estimate,.after=labels)
      data <- data %>% dplyr::group_by(year,table_id,geoid) %>% 
        dplyr::mutate(subtotal=ifelse(calculation=="median" | calculation=="mean",NA,max(estimate)),
                      pct = ifelse(calculation=="median" | calculation=="mean",NA,estimate/subtotal)) %>% 
        dplyr::ungroup()
      
      data <- data %>% dplyr::group_by(year,table_id,geoid,type) %>% 
        dplyr::mutate(subtotal_by_type=ifelse(calculation=="median" | calculation=="mean",NA,sum(estimate)),
                      pct_by_type = ifelse(calculation=="median" | calculation=="mean",NA,estimate/subtotal_by_type),
                      moe_pct = ifelse(calculation=="median" | calculation=="median",NA,moe/subtotal_by_type)) %>% 
        dplyr::ungroup()
      
      data <- data %>% dplyr::relocate(moe,.after=pct_by_type)
      data <- data %>% dplyr::relocate(moe_pct,.after=moe)
      
      
      ### Add Geographical Data ----------------
      if(verbose==TRUE)message(paste(dur(st),"Adding geography..."))
      data <- data %>% dplyr::relocate(name,.after=moe_pct)
      data <- data %>% dplyr::mutate(geography=geography)
      if("state" %in% names(data))data <- data %>% dplyr::relocate(state,.after=geography)
      if("county" %in% names(data))data <- data %>% dplyr::relocate(county,.after=state)
      if("tract" %in% names(data))data <- data %>% dplyr::relocate(tract,.after=county)
      if("block_group" %in% names(data))data <- data %>% dplyr::relocate(block_group,.after=tract)
      if("block_group" %in% names(data)){
        data <- data %>% dplyr::relocate(geoid, .after=block_group)
      }else if("tract" %in% names(data)){
        data <- data %>% dplyr::relocate(geoid, .after=tract)
      }else if("county" %in% names(data)){
        data <- data %>% dplyr::relocate(geoid, .after=county)
      }else if("state" %in% names(data)){
        data <- data %>% dplyr::relocate(geoid, .after=state)
      }else{
        data <- data %>% dplyr::relocate(geoid, .after=geography)
      }
      
      data <- data %>% dplyr::relocate(calculation,type,type_base,varID,.after=geoid)
      
      if(verbose==TRUE)message(paste(dur(st),"Creating type 1 data..."))
      data_type_1 <- data
      data_type_1 <- data_type_1 %>% mutate(dt=1)
      attr(data_type_1,"dataType") <- 1
      
      ## Filter summaries --------------------------------------------------------
      if(verbose==TRUE)message(paste(dur(st),"Creating type 2 data..."))
      ## This is `type 2 data`
      if(filterSummary==TRUE || profile==TRUE){
        
        levels <- c("root","summary","level_1","level_2","level_3","level_4")
        fsl <- c()
        ## Remove bottom specified summary levels.
        if(is.numeric(filterSummaryLevels)==TRUE){
          for(i in 1:length(levels)){
            if(i %in% filterSummaryLevels){
              c(fsl,levels[i])
            }
          }
        }else{
          fsl <- filterSummaryLevels
        }
        
        data_type_2 <- data_type_1 %>% filter(!(type %in% fsl))
        data_type_2 <- data_type_2 %>% mutate(dt=2)
        attr(data_type_2,"dataType") <- 2
      }
      
      ## Up to this point ^^^ is `type 1/2` data.
      
      ## Mode adjustments --------------------------------------------------------
      
      if(verbose==TRUE)message(paste(dur(st),"Creating type 3/4 data..."))
      
      if(mode=="summarize" || profile==TRUE){
        ## Deal with medians
        data_type_3 <- data_type_1 %>% dplyr::mutate(estimate=ifelse(calculation=="median",NA,estimate))
        data_type_3 <- data_type_3 %>% group_by(table_id,year,variable,concept,labels,calculation,type,varID) %>% 
          summarize(estimate=if(any(calculation=="mean"))mean(estimate)
                    else(sum(estimate)),
                    subtotal_by_type=sum(subtotal_by_type)) %>% 
          ungroup()
        data_type_3 <- data_type_3 %>% 
          group_by(table_id) %>% 
          mutate(subtotal=if(any(calculation=="mean"))NA
                 else(max(estimate)),
                 pct = estimate/subtotal,
                 pct_by_type = estimate/subtotal_by_type,
                 dt=3) %>% 
          ungroup()
        
        data_type_3 <- data_type_3 %>% relocate(estimate,subtotal, pct,subtotal_by_type,pct_by_type, .after=labels)
        attr(data_type_3,"dataType") <- 3
        
        if(filterSummary==TRUE || profile==TRUE){
          data_type_4 <- data_type_3 %>% filter(!(type %in% fsl))
          attr(data_type_4,"dataType") <- 4
          # ## Deal with medians
          # data_type_4 <- data_type_2 %>% dplyr::mutate(estimate=ifelse(calculation=="median",NA,estimate))
          # data_type_4 <- data_type_4 %>% group_by(table_id,year,variable,concept,labels,calculation,type,varID) %>% 
          #   summarize(estimate=sum(estimate),
          #             subtotal_by_type=sum(subtotal_by_type))
          # data_type_4 <- data_type_4 %>% 
          #   group_by(table_id) %>% 
          #   mutate(subtotals=max(estimate),
          #          pct = estimate/subtotals,
          #          pct_by_type = estimate/subtotal_by_type,
          #          dt=4) %>% 
          #   ungroup()
          # 
          # data_type_4 <- data_type_4 %>% relocate(estimate,subtotals, pct,subtotal_by_type,pct_by_type, .after=labels)
          
        }
      }
    }
# Finish and return -------------------------------------------------------

  if(verbose==TRUE)message(paste(dur(st),"Done."))
  
  if(profile==FALSE){
    if(mode=="table" && filterSummary==FALSE)data <- data_type_1
    if(mode=="table" && filterSummary==TRUE)data <- data_type_2
    if(mode=="summarize" && filterSummary==FALSE)data <- data_type_3
    if(mode=="summarize" && filterSummary==TRUE)data <- data_type_4
    if(mode=="table" && (!is.null(dataset_last) && dataset_last=="pums"))data <- data_type_7
    if(mode=="summarize" && dataset_last=="pums")data <- data_type_8
    
  }else{
    if(dataset_last=="pums"){
      data <- list(type7data = data_type_7,
                   type8data = data_type_8
      )
      attr(data,"dataType") <- 9
    }else{
      data <- list(type1data = data_type_1,
                   type2data = data_type_2,
                   type3data = data_type_3,
                   type4data = data_type_4
      )
      attr(data,"dataType") <- 6
    }
  }
  return(data)
}
