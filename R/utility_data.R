# acs_vars_builder-----------------------------------

#' ACS Variable Builder
#'
#' A simple function to construct the variable lists returned from the ACS dataset.
#' Uses tidycensus::load_variables() and saves both the output as a global object,
#' and a filtered object as a summary file. Primarily internal use.
#'
#' @param year Year of ACS data return.
#' @param dataset Defaults to "acs5", but can use any valid ACS dataset. 
#' Currently, this and all my functions are tuned to the ACS dataset. 
#' Future functionality may include decennial data and PUMS.
#' @param return If FALSE, load variables into env., otherwise return to variable.
#' @param detailed_tagging Logical: TRUE provides column tagging kind of ACS var table.
#' @param verbose Logical parameter to specify whether to produce verbose output.
#'
#' @return Produces two objects in the global environment: ACS.VARS and ACS.TABLES.
#' ACS.VARS is a master list of all variables, ACS.TABLES is a filtered dataframe
#' of top-level variables.
#'
#'
#' @importFrom tidycensus load_variables
#' @importFrom stringr str_count
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' acs_vars_builder()
#' }
acs_vars_builder <- function(year,
                             dataset="acs5",
                             detailed_tagging=FALSE,
                             return=TRUE,
                             verbose=FALSE)
{
  
  # To prevent "no visible binding for global variable" error
  name <- concept <- map_chr <- geography <- calculation <- table_id <- NULL
  varName <- paste("ACS.VARS.",year,sep="")

  if(exists(varName,inherits = TRUE)==FALSE){
    if(!file.exists(file.path(getwd(),"data",paste("ACS.VARS.",year,".RDS",sep=""),fsep="/"))){
      if(verbose==TRUE)message("acs_vars_builder(): Trying to download ACS variables...")
      ACS.VARS <- load_variables(year,dataset=dataset)
      if(verbose==TRUE)message("      Formatting variables...")
      ACS.VARS <- ACS.VARS %>% mutate(table_id=gsub('(_*?)_.*','\\1',name))
      ACS.VARS <- ACS.VARS %>% mutate(calculation = ifelse(str_detect(concept,"(?i)MEDIAN"),"median","count"))
      ACS.VARS <- ACS.VARS %>% mutate(type = case_when(
        str_count(label,"!!") == 1 ~ "root",
        str_count(label,"!!") == 2 ~ "summary",
        str_count(label,"!!") == 3 ~ "level_1",
        str_count(label,"!!") == 4 ~ "level_2",
        str_count(label,"!!") == 5 ~ "level_3",
        str_count(label,"!!") == 6 ~ "level_4",
        .default = "other"
      ))
      ACS.VARS <- ACS.VARS %>% mutate(type_base = case_when(
        str_count(label,"!!") == 1 ~ "root",
        str_count(label,"!!") > 1 ~ "vars",
        .default = "other"
      ))
      #ACS.VARS <- ACS.VARS %>% mutate(varID=strsplit(name,"_") %>% map_chr(.,2))
      ACS.VARS <- ACS.VARS %>% mutate(varID=strsplit(name,"_")[[1]][[2]])
      
    if(detailed_tagging==TRUE){
      if(verbose==TRUE)message("     Creating detailed tagging...")
      pb <- txtProgressBar(min = 1, max = nrow(ACS.VARS), style = 3)
      for(i in 1:nrow(ACS.VARS)){
        x <- ACS.VARS[i,'name']
        pre <- case_when(
          stringr::str_starts(x,"B") ~ "B",   # Detailed Tables: Base Table
          stringr::str_starts(x,"CP") ~ "CP", # Comparison Profile
          stringr::str_starts(x,"C") ~ "C",   # Detailed Tables: Collapsed Table
          stringr::str_starts(x,"S") ~ "S",   # Subject Table
          stringr::str_starts(x,"DP") ~ "DP", # Data Profile
          stringr::str_starts(x,"S0201") ~ "S0201",# Selected Population Profile
          #stringr::str_starts(x,"R") ~ "",    # Ranking Table (TODO this and below exist, but can't find them rn)
          #stringr::str_starts(x,"GCT") ~ "",  # Geographic Comparison Table
          #stringr::str_starts(x,"K20") ~ "",  # Supplemental Table
          #stringr::str_starts(x,"XK") ~ "",   # Experimental Estimates
          #stringr::str_starts(x,"NP") ~ "",   # Narrative Profile
          .default = "UNKNOWN"
        )
        
        post <- str_sub(unlist(stringr::str_split(x,"_"))[1],-1,-1)
 
        ACS.VARS[i,'table_type'] <- pre
        ACS.VARS[i,'sub_table'] <- post
        
        setTxtProgressBar(pb, i)
      }
      close(pb)
    }
      
      ACS.TABLES <- ACS.VARS %>% select(table_id,concept,geography,calculation)
      ACS.TABLES <- unique(ACS.TABLES)
      if(verbose==TRUE)message("     Saving ACS variables...")
    }else{
      ACS.VARS <- readRDS(file.path(getwd(),"data",paste("ACS.VARS.",year,".RDS",sep=""),fsep="/"))    
      ACS.TABLES <- readRDS(file.path(getwd(),"data",paste("ACS.TABLES.",year,".RDS",sep=""),fsep="/")) 
    }
    # if(return==FALSE){
    #   if(verbose==TRUE)message("     Saving vars to global environment...")
    #   assign(paste("ACS.VARS.",year,sep=""),ACS.VARS,envir = .GlobalEnv)
    #   assign(paste("ACS.TABLES.",year,sep=""),ACS.TABLES,envir = .GlobalEnv)
    # }else{
      # Returning both dataframes for testing.
      acsLists <- list(ACS.VARS,ACS.TABLES)
      names(acsLists) <- list(paste("ACS.VARS.",year,sep=""),
                              paste("ACS.TABLES.",year,sep=""))
      
      return(acsLists)
    # }
    if(verbose==TRUE)message("     Done.")
  }else{
    if(verbose==TRUE)message("acs_vars_builder(): Variables exist in global environment...")
  }
  if(return==TRUE){
    av <- get(paste("ACS.VARS.",year,sep=""))
    at <- get(paste("ACS.TABLES.",year,sep=""))
    acsLists <- list(av,at)
    names(acsLists) <- list(paste("ACS.VARS.",year,sep=""),
                            paste("ACS.TABLES.",year,sep=""))
    return(acsLists)
    if(verbose==TRUE)message("     Returned.")
  }
}

# comparison_helper -------------------------------------------------------
## TODO The download doesn't work for states. I need to fix this.
#' Comparison Helper
#'
#' @param df A primary dataset to provide comparisons against.
#' @param comparisonDF A geography-level comparison dataset.
#' @param comp_type Explicitly call comparison type.
#' @param tableID The tableID to reference comparison.
#' @param stateFilter FIPS or abbreviation for filtering and selecting comparisons.
#' @param verbose Logical parameter to specify whether to produce verbose output.
#'
#' @return Dataframe
#' @export
#'
#' @examples \dontrun{
#' comparison_helper(data,comparisondata,tableID="B02001",stateFilter=17)
#' }
comparison_helper <- function(df=NULL,
                              comparisonDF=NULL,
                              comp_type = NULL,
                              tableID = NULL,
                              stateFilter = NULL,
                              verbose=FALSE){
  
  table_id <- estimate <- pct <- us_est <- us_pct <- state_est <- 
    pct <- variable <- state <-state_pct <- NULL
  
  if(verbose==TRUE)message(" - ch | Error checks...")
  if(!is.null(stateFilter)){
    if(stateFilter==99999){
      stop("!> Ambiguous state filter. No state supplied, and multiple states included in dataset.")
    }
  }
  
  # State type 3/4 data don't work; they just produce us data.
  if(comp_type=="us"){
    if(type_data(comparisonDF,FALSE)==5){
      comp <- comparisonDF$data$type1data
    }else if(type_data(comparisonDF)==1){
      comp <- comparisonDF
    }else{
      stop("You didn't include a valid data type in comparison_helper() for US compare.")
    }
  }else{ 
    if(type_data(comparisonDF,FALSE)==5){
      comp <- comparisonDF$data$type1data
    }else if(type_data(comparisonDF)==1){
      comp <- comparisonDF
    }else{
      stop("You didn't include a valid data type in comparison_helper() for state compare.")
    }
  }
  
  # Filter 
  if(verbose==TRUE)message(" - ch | Filter by tableID...")
  comp <- comp %>% dplyr::filter(table_id==tableID) 
  
  if(verbose==TRUE)message(" - ch | Set up tables and further filtering...")
  if(comp_type=="us"){
    comp <- comp %>% dplyr::rename(us_est=estimate, us_pct=pct) %>% dplyr::ungroup() %>% 
      dplyr::select(variable,us_est,us_pct)
  }else if(comp_type=="state"){
    comp <- comp %>% 
      dplyr::rename(state_est=estimate, state_pct=pct) %>% 
      dplyr::ungroup() 
    comp <- comp %>% dplyr::filter(state==as.vector(stateFilter)) %>% 
      dplyr::select(variable,state_est,state_pct)
  }else{
    stop("No comp_type supplied for comparison_helper().")
  }
  
  if(verbose==TRUE)message(" - ch | Merge dfs...")
  df <- merge(df,comp,by="variable")
  if(comp_type=="us"){
    df <- df %>% mutate(us_pct_diff = pct-us_pct)
  }else{
    df <- df %>% mutate(state_pct_diff = pct-state_pct)
  }
  
  if(verbose==TRUE)message(" - ch | Done, returning...")
  return(df)
}

# create_comparison_data-----------------------------------
#' Create Profile (Batch By Geographies)
#' 
#' This function relies on create_profile_batch for its internal logic, but 
#' is used primarily to create comparison profile objects for whole geographies.
#' This is useful when rendering displayTable() and a larger geography 
#' comparison is desired. 
#'
#' @param verbose Whether to provide verbose output.
#' @param coordColName Default, set to "sf", but can be changed if non-sf object.
#' @param showCall Whether get_acs() should have showCall set to TRUE
#' @param geography Either "us" or "state".
#' @param year Numeric value specifying year of ACS call.
#' @param variables A variables vector.
#' @param tableID A tableID vector.
#' @param censusDataset The census dataset for calls. Defaults to `acs5`.
#' @param profileDataset Optional dataset to determine state/county for filtering.
#' @param geosObject Optional geosObject to speed up processing time.
#' @param test Internal: for testing purposes.
#'
#' @importFrom sf st_is_empty
#'
#' @return A deep, nested list. Structure is list1 > regionInfo,data.
#' regionInfo is a tibble with identifiers, and data contains three tibbles:
#' df, dfCount, dfNoSummary. See create_profile for more information.
#' To access individual datasets, use this structure: object$data[[n]]$profileTable.
#'
#' @examples
#' \dontrun{
#' create_comparison_data(geography="state",
#' year=2021,variables = profile_variables,tableID = profile_tableID)
#' create_comparison_data(geography="us",year=2021,
#' variables = profile_variables,tableID = profile_tableID)
#' }
create_comparison_data <- function(
  #geo_list = NULL,
  geography=NULL,
  profileDataset=NULL,
  year=NULL,
  variables=NULL,
  tableID=NULL,
  coordColName="sf",
  verbose=FALSE,
  showCall=FALSE,
  geosObject=NULL,
  censusDataset="acs5",
  test=FALSE){
  
  # TODO It would be interesting to also run this with county geographies.
  
  ## Deal with "no visible binding for global variable" error 
  GEOID <- NULL
  
  ## Check for params and variables.
  
  if(is.null(year)){
    message("You need to supply a year for this to work.")
    stop()
  }
  
  if(!is.null(profileDataset)){
    if(type_data(profileDataset)<=3){
      stop("profileDataset must be type 4 or 5 data.")
    }
  }
  
 if(geography=="state"){
  # If a profile dataset is supplied, pull coords to filter state / county. 
  # Otherwise, just get states list.
  if(!is.null(profileDataset)){
    if(verbose==TRUE)message("Using profileDataset to get df from ggr...")
    df <- get_geocode_radius(filterRadius = 1,geography="tract",coords=profileDataset$info$coordinates,verbose=verbose)
    state <- df$states
  }
  # Filter geo_list for only the 50 states. Not territories.
   if(verbose==TRUE)message("Filtering down to 50 states...")
  geo_list <- geo_var_builder(geography="state", try="local",geosObject = geosObject,verbose=verbose)
  if(test==TRUE){
    df <- geo_list$geo_states %>% filter(GEOID==56) 
  }else{
    if(!is.null(profileDataset)){
      df <- geo_list$geo_states %>% filter(GEOID==state)  
    }else{
      df <- geo_list$geo_states %>% filter(GEOID<=56) 
    } 
  }
 }
  
  if(verbose==TRUE)message("Getting ACS...")
  ACS <- acs_vars_builder(year=year)
  ACS.VARS <- ACS[[1]]
  ACS.TABLES <- ACS[[2]]
  
  if(verbose==TRUE)message("> Beginning profile batch build.")
  
  # # Set default variables and starter variables.
  masterDF <- list()
  
  if(geography=="state"){
    # Begin the loop.
    for(i in 1:nrow(df)){
      if(verbose==TRUE){
        if(verbose==TRUE)message(paste("> > Iteration ",i,"/",nrow(df)))
      }
      
      # Set coord and address columns if needed.
      if(coordColName=="sf"){
        if(class(df)[1]!="sf"){
          stop("Error: You do not have an sf object. Reselect coordColName.")
        }
        if(coordColName!="sf"){
          coords <- c(df[i,coordColName[1]],df[i,coordColName[2]])
        }else if(class(df)[1]=="sf" & st_is_empty(df[i,])==FALSE){
          coords <- st_coordinates(df[i,])
        }else{
          coords <- NULL
        }
      }else{
        coords <- NULL
      }
      
      if(st_is_empty(df[i,])==FALSE){
        x <- profiler(name = paste("State Comparison Info: ",df[i,]$NAME,sep=""),
                        tableID = tableID,
                            variables = variables,
                            year=year,
                            state=df[i,]$STATEFP,
                            geography=geography,
                            geosObject = geosObject,
                            #coords=st_coordinates(df[i,]),
                            verbose=verbose)
      }else{
        x <- "MISSING ACCURATE GEODATA"
      }
      if(verbose==TRUE)message(paste("> > Compiling data for ",df[i,]$NAME,sep=""))
      
      ## TODO figure out how to include concepts, but only once in this list. Not every time, and not with every iteration. Perhaps get_profile_concepts()?
      masterDF <- rbind(masterDF,x$data$type1data)
    }
    return(masterDF)
  }else{
    x <- profiler(name = "United States Comparison",
                  tableID = tableID,
                  variables = variables,
                  year=2021,
                  geography=geography,
                  geosObject = geosObject,
                  #coords=st_coordinates(df[i,]),
                  verbose=verbose)
    
    if(verbose==TRUE)message("> > Compiling data for US vars.")
    
    ## TODO figure out how to include concepts, but only once in this list. Not every time, and not with every iteration. Perhaps get_profile_concepts()?
    #z <- tibble(regionInfo = data.frame(NAME = "United States"),
    #            data = list(x))
    #masterDF <- rbind(masterDF,z)
    
  }
  x <- x$data$type1data
  return(x)
  # if(geography=="state"){
  #   states_profile <<- masterDF
  # }else{
  #   us_profile <<- masterDF
  # }
  if(verbose==TRUE)message(paste("^ ^ Profile complete.",sep=""))
  
}

# Entropy Index -------------------------
#' Entropy Index 
#' 
#' A statistical function to estimate diversity / segregation in datasets.
#'
#'Logic developed from
#'https://www2.census.gov/programs-surveys/demo/about/housing-patterns/multigroup_entropy.pdf
#'and Scientific Study of Religion - 2016 - Dougherty - Congregational Diversity
#'and Attendance in a Mainline Protestant-2024-02-19-13-18.pdf OR
#'https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1468-5906.2008.00390.x."EI"
#'is built from the second, while MGEI is built from the former. Basically, the
#'EI signifies how much diversity exists in a community (e.g., Census Tract).
#'The Multigroup index of the whole metro area examines how much segregation
#'exist between areas in a larger area. It doesn't account for the diversity of
#'the whole, but the integration of the areas. So for example, Individual census
#'tracts may have a high entropy score (0.8), signifying lots of diversity. But
#'it may have a low metro score (.05) suggesting that diversity is evenly spread
#'throughout the larger area. If the metro MGEI was higher, it would signify
#'more segregation between diverse areas.
#'
#' @param data A data object for which to estimate entropy. 
#' @param dataType Can choose "long", "wide" or "vector" depending on data object type.
#' @param geography Defaults to "tract," but must match data object.
#' @param wideCols Specified columns for calculating entropy in wide data.
#' @param longCol Specified columns for calculating entropy in long data.
#' @param tableID TableID specification for calculating entropy.
#' @param variables Variable specification for calculating entropy.
#' @param dissimilarityValue For dissimilarity index, selecting minority group.
#'   Numeric value corresponding to variable.
#' @param dissimilarityValueB For exposure/isolation index, selecting minority
#'   group. Numeric value corresponding to variable.
#' @param filterAddress For data calls, a filtered area specification.
#' @param filterRadius For data calls, a filtered area specification.
#' @param state Input (abb. or FIPS) of state for search.
#' @param county Input (abb. or FIPS) of county for search.
#' @param tract Input (abb. or FIPS) of tract for search.
#' @param block_group Input (abb. or FIPS) of block group for search.
#' @param year Year for data call.
#' @param filterSummary Logical parameter to specify whether to filter out summary levels (typically _001 and therefore "root").
#' @param filterSummaryLevels Explicit description of lowest type denoting summary level. Also excludes lower levels.#' 
#' @param return Logical parameter. If TRUE, return value only. Otherwise return formatted string.
#' @param dataset Can select "acs1", "acsse" (supplemental est), "acs3", "acs5","flows" (migration flows) 
#' @param verbose Logical parameter to specify whether to produce verbose output.
#' 
#' @importFrom ggplot2 geom_point
#'
#' @return dataframe
#' @export
#' 
#'
#' @examples \dontrun{
#' entropyIndex(data=NULL,tableID = "B11012",variables =
#' c(1:4),year=2022,verbose=TRUE,filterAddress = v,filterRadius = 1)
#' }
entropyIndex <- function(data=NULL,
                         dataType="long", # c("long","wide","vector")
                         dissimilarityValue=NULL, # for dissimilarity index, selecting minority group
                         dissimilarityValueB=NULL, # for exposure/isolation index, selecting minority group
                         geography="tract",
                         wideCols=NULL,
                         longCol="tot_pop_pct",
                         filterSummary=FALSE,
                         filterSummaryLevels="root", # default
                         #entropyType="EI", # c("EI","MGEI")
                         tableID=NULL,
                         variables=NULL,
                         filterAddress=NULL,
                         filterRadius=NULL,
                         # filterSummaryLevels=NULL, Duplicate in typeFilter
                         state=NULL,
                         county=NULL,
                         tract=NULL,
                         block_group=NULL,
                         year=NULL,
                         return=FALSE,
                         #  plot=FALSE,
                         dataset="acs5",
                         verbose=FALSE){
  
  table_id <- variable <- concept <- estimate <- tot_pop_pct <- 
    pct_by_type <- tot_pop <- tot_pop_pct <- name <- geoid <- type <- 
    est_total <- sub_total <- NULL
  
  if(is.null(tableID)){
    stop("No tableID supplied. Please try again.")
  }
  ### Init 
  if(!is.null(tableID) || !is.null(variables)){
    viC <- varInputCheck(tableID=tableID,variables=variables)
    variables <- viC$variables
  }

  ## Create data object if does not exist
  if(is.null(data)){
    data <- profiler(year=year,
                     dataset=dataset,
                     tableID=tableID,
                     variables=variables,
                     geography=geography,
                     filterAddress=filterAddress,
                     filterRadius=filterRadius,
                     filterSummary=filterSummary,
                     filterSummaryLevels=filterSummaryLevels,
                     state=state,
                     county=county,
                     tract=tract,
                     block_group=block_group,
                     verbose=verbose)
  }
  
  ## Error Checking
  if(verbose==TRUE)message("   - EI: error checking...")
  if(dataType=="vector"){
    if(!is.vector(data)){
      stop("You have not supplied a vector, per specified dataType.")
    }
  }else{
    if(type_data(data)==5){
      data <- data$data$type2data
    }else{
      if(type_data(data)!=2){
        stop("You need to supply type_2_data.")
      }
    }
  }
  
  if(!(tableID %in% data$table_id)){
    stop("That tableID does not exist in the data provided.")
  }

  ## Data Cleaning and preparation
  if(verbose==TRUE)message("   - EI: cleaning data...")
  if(dataType!="vector"){
    data <- data %>% ungroup() %>% select(table_id,variable,year,concept,labels,estimate,tot_pop,tot_pop_pct,name,geoid,geography,type)
    data <- data %>% filter(table_id==tableID)

    if(!is.null(variables)){
      data <- data %>% filter(variable %in% variables) 
    }
    data <- data %>% filter(!is.na(tot_pop_pct))

    # Create empty entropy table to populate.
    entropyTableNames <- c("table_id","year","concept","name","geoid","geography","tot_pop_pct","entropyIndex","multiGroupEntropyIndex")
    entropyTable <- data.frame(matrix(ncol = length(names(entropyTableNames)),nrow = 0))
    colnames(entropyTable) <- names(entropyTableNames)
  }
  
  ## Get whole-area entropy estimates / entropy index
  # Combine populations
  area <- data %>% group_by(labels,variable) %>% summarize(est_total = sum(estimate),
                                                  sub_total = sum(tot_pop),
                                                  areaPct = est_total/sub_total)
  
  areaPopulation <- unique(area$sub_total)
  
  ### Processing Function with Vectorized Input
  processEntropy <- function(vector,dissimilarityValue=dissimilarityValue, return=FALSE,verbose=FALSE){
    # Filter out zeros
    if(verbose==TRUE)message("   - pE:         dealing with zeroes...")
    # for(i in 1:length(vector)){
    #   if(vector[i]<0.0001){
    #     vector[i] <- 0.0001
    #   }
    # }
    K <- length(vector)
    maxEntropy <- log(K)
    if(K==0){
      return(NA)
    }else{
      
      ## Convert to proportion if whole number
      if(verbose==TRUE)message("   - pE:         converting to proportion if whole number...")
      for(b in 1:K){
        if(vector[b]>1){
          vector[b] <- vector[b]/100
        }
      }
      
      H <- 0
      
      ## Calculate standardized entropy score.
      if(verbose==TRUE)message("   - pE:         calculating entropy (ES)...")
      for(j in 1:K){
        ifelse(vector[j]==0,
               s <- vector[j] * 0,
               s <- vector[j] * log(1/vector[j]))
        H <- s + H
      }
      ES <- round(H,4)
      SES <- round((ES/maxEntropy),4) ## Return standardized entropy score.
      
      if(return==FALSE){
        cat(paste("Standardized Entropy Index:",SES))
      }else{
        return(list(ES=ES,SES=SES))
      }
    }
  }
  
  ## Run Program 
  if(verbose==TRUE)message("   - EI: running processEntropy()...")
  if(dataType=="wide"){
    if(verbose==TRUE)message("   - EI: using wide data...")
    if(is.null(wideCols)){
      stop("Please include columns by number or name")
    }
    
  }else if(dataType=="long"){
    if(verbose==TRUE)message("   - EI: using long data...")
    # Get reference variables
    if(is.null(tableID)){
      if(verbose==TRUE)message("   - EI: generating unique tableID values...")
      tableID <- unique(data$table_id)
    }
    
    #Loop by table_id
    if(verbose==TRUE)message("   - EI: looping through tableIDs...")
    for(ti in 1:length(tableID)){
      if(verbose==TRUE)message(paste("   - EI:    it #",ti))
      if(verbose==TRUE)message(paste("   - EI:    filtering by table_id..."))
      df <- data %>% filter(table_id==tableID[ti])
      if(geography=="metro" || geography=="msa"){
        gid <- 1
      }else{
        gid <- unique(df$geoid)
        DI <- 0
        EXI <- 0
        II <- 0
        for(g in 1:length(gid)){
          if(verbose==TRUE)message(paste("   - EI:       looping through geoids; it #",g))
          if(verbose==TRUE)message(paste("   - EI:       filtering by geoid..."))
          d <- df %>% filter(geoid==gid[g])
        
        if(verbose==TRUE)message(paste("   - EI:       building vector..."))
        vector <- d[[longCol]]
        
        maxEntropy <- log(length(vector))
        
        if(verbose==TRUE)message(paste("   - EI:       running processEntropy()..."))
        ESLIST <- processEntropy(vector,return=TRUE,verbose=verbose)
        SES <- ESLIST$SES
        ES <- ESLIST$ES
        
        ## Dissimilarity Index
        if(!is.null(dissimilarityValue)){
          if(is.numeric(dissimilarityValue)){
            dv <- paste(tableID,"_",sprintf("%03d",dissimilarityValue),sep="")
          }else{
            dv <- dissimilarityValue
          }
          dvd <- d %>% filter(variable==dv)
          ti <- dvd$estimate
          pi <- dvd$tot_pop_pct
          To <- areaPopulation
          ad <- area %>% filter(variable==dv)
          P <- ad$areaPct
          
          dsi <- (ti * abs(pi-P)) / (2*To*P*(1-P))
          DI <- DI+dsi
        }else{
          DI <- NULL
        }
        
        ## Exposure / Isolation Index
        if(!is.null(dissimilarityValueB)){
          if(is.numeric(dissimilarityValueB)){
            dvb <- paste(tableID,"_",sprintf("%03d",dissimilarityValueB),sep="")
          }else{
            dvb <- dissimilarityValueB
          }
          eidA <- d %>% filter(variable==dv)
          eidB <- d %>% filter(variable==dvb)
          xi <- eidA$estimate
          yi <- eidB$estimate
          ti <- eidA$tot_pop
          To <- areaPopulation
          aeid <- area %>% filter(variable==dv)
          X <- aeid$est_total

          exv <- (xi/X)*(yi/ti)
          EXI <- EXI+exv
          iiv <- (xi/X)*(xi/ti)
          II <- II+iiv
          
        }else{
          EXI <- NULL
          II <- NULL
        }
  
        # Remove pct col and get summary row
        d <- d %>% select(table_id,year,concept,name,geoid,geography,tot_pop)
        d <- unique(d)
        d <- d %>% mutate(maxEntropy = maxEntropy, entropyScore = ES, standardizedEntropyScore = SES)
        entropyTable <- rbind(entropyTable,d)
        }
      }
    }
    
    areaVector <- area[["areaPct"]]
    AESLIST <- processEntropy(areaVector,return=TRUE,verbose=verbose)
    AES <- AESLIST$ES
    ASES <- AESLIST$SES

    ## Develop entropy index (weighted average deviation of each unit's entropy from the 
    ## metropolitan-wide entropy, expressed as a fraction of the metropolitan area's total
    ## entropy.) "The entropy index varies between 0, when all areas have the same
    ## composition as the entire metropolitan area (i.e., maximum integration), to a high of 1,
    ## when all areas contain one group only (maximum segregation). While the diversity score is 
    ## influenced by the relative size of the various groups in a metropolitan area, 
    ## the entropy index, being a measure of evenness, is not. Rather, it measures how evenly 
    ## groups are distributed across metropolitan area neighborhoods, regardless of the 
    ## size of each of the groups." (Iceland, p. 8)

    EI <- 0
    for(et in 1:nrow(entropyTable)){
      t <- entropyTable[[et,"tot_pop"]]
      Ei <- entropyTable[[et,"entropyScore"]]
      To <- areaPopulation
      #print(paste(t,"(",AES,"-",Ei,") / (",AES,"*",To,")"))
      h <- (t * (AES-Ei)) / (AES*To)
      EI <- EI+h
    }
    
    entropyBundle <- list(UnitEntropyScore = entropyTable,
                          MaxEntropy = maxEntropy,
                          AreaSummary = area,
                          AreaEntropyScore = AES,
                          AreaStandardizedEntropyScore = ASES,
                          AreaEntropyIndex = EI)
    
    if(!is.null(dissimilarityValue)){
      entropyBundle <- append(entropyBundle,list(DissimilarityIndex = DI))
    }
    if(!is.null(dissimilarityValueB)){
      entropyBundle <- append(entropyBundle,list(ExposureIndex = EXI,
                                                 IsolationIndex = II))
    }
    
    
    return(entropyBundle)
  }else{
    processEntropy(data,return=TRUE,verbose=verbose)
  }
}

# varInputCheck---------------------
varInputCheck <- function(tableID = NULL,
                          variables = NULL,
                          variableSummary = NULL,
                          verbose=FALSE){
  if(is.list(variables)){
    if(is.null(names(variables))){
      stop("Incorrect formatting. Variables must be formatted in a named list. E.g.: list(`B01001`=c(1:8)).")
    }
    tableID <- names(variables)
  }
  
  newVars <- newVarsSum <- NULL 
  if(is.null(tableID)){
    if(is.null(variables)){
      stop("Insufficient data: requires tableID and/or variables.")
    }else if (is.numeric(variables)){
      stop("Ambiguous variables supplied. Either provide whole variable names (`B01001_001`) or include a tableID.")
    }
  }else{
    if(is.list(variables)){
      for(t in 1:length(tableID)){
        for(i in 1:length(variables[[t]])){
          if(is.numeric(variables[[t]][i])==TRUE){
            newVars <- c(newVars,paste(tableID[t],"_",sprintf("%03d",variables[[t]][i]),sep=""))
          }
        }
      }
    }else{
      for(i in 1:length(variables)){
        if(is.numeric(variables[i])==TRUE){
          newVars <- c(newVars,paste(tableID,"_",sprintf("%03d",variables[i]),sep=""))
        }
      }
    }
  }
  
  if(is.null(tableID) && is.null(variableSummary)){ 
    stop("Summary variable must be supplied. Or include tableID.")
  }else{
    for(v in 1:length(tableID)){   
      if(is.null(variableSummary[v])){
        newVarsSum <- c(newVarsSum,paste(tableID[v],"_",sprintf("%03d",1),sep=""))
        if(verbose==TRUE)message(paste("No variableSummary supplied. Defaulting to ",tableID[v],"_",sprintf("%03d",1),sep=""))
      }else{
        if(is.numeric(variableSummary[v])){
          newVarsSum <- c(newVarsSum,paste(tableID[v],"_",sprintf("%03d",variableSummary[v]),sep=""))
        }
      }
    }
  }
  if(!is.null(newVars))variables <- newVars
  if(!is.null(newVarsSum))variableSummary <- newVarsSum
  return(list(tableID = tableID, variables = variables, variableSummary = variableSummary))
}

# dateInputCheck---------------------
dateInputCheck <- function(year = NULL,dataset = NULL,verbose=FALSE){
  if(is.null(year)){
    ## If datatype=="acs", set to year - 2, as this seems to be a safe bet for released data from ACS.
    ## If decennial, round down to nearest decade.
    acssets <- c("acs1","acs3","acs5")
    decsets <- c("ddhca","dhc","dp","pl","pes","dhcas","cd118")
    if(dataset %in% acssets){
      year <- as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-2
    }else if(dataset %in% decsets){
      year <- floor(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))/10)*10
    }else{
      stop("!> No year supplied, and no valid census dataset supplied.")
    }
    
   if(verbose==TRUE)message(paste("!> FYI: No year supplied...setting default to ",
                  year,". Change this by adding manually.",sep=""))
  }
  return(year)
}


# get_vre_table-----------------------------------

#' Get VRE Tables
#'
#' A function to calculate the margin of error based on aggregate ACS data.
#'
#' The basic logic of this function is derived from the Census Bureau 
#' (https://www.census.gov/data/academy/webinars/2020/calculating-margins-of-error-acs.html).
#' Information is stored in VRE tables on the CB FTP website in .csv files. 
#' This function finds, downloads, extracts, converts the .csv files, and then 
#' performs statistical calculations on the tables, for the aggregate data.
#'
#' @param data ACS data passed to function from capi().
#' @param year ACS year passed to function from capi().
#' @param geography ACS geography passed to function from capi().
#' @param tableID ACS variable base passed to function from capi().
#' @param variableList ACS variable number(s) passed to function from capi(). Integer or list.
#' @param variableAgg Future TODO: make this suitable for a list of variables.
#' @param state ACS state (descriptive or FIPS) passed to function from capi().
#' @param county ACS county (descriptive or FIPS) passed to function from capi().
#' @param savePath Local save path for VRE tables.
#' @param verbose Logical parameter to display output
#' @param censusDataset Can select "acs1", "acsse" (supplemental est), "acs3", "acs5","flows" (migration flows) 
#'
#' @return Returns a dataframe with variance, standard error, and margin of error for aggregate data.
#'
#'
#' @importFrom dplyr reframe
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom utils read.csv
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#'   VREMOE <- get_vre_table(data = df,year = 2021,geography = "tract",
#'   tableID = "B01001",variableList = c(2,3,3),variableAgg = NULL,state = "IL",
#'   county = 043)
#'   }
#'
get_vre_table <- function(
    data=NULL,
    year=NULL,
    geography=NULL,
    # geos=NULL,
    tableID, ## TODO: make this suitable for a list of variabvles
    variableList, ## A list of subvariables to include
    variableAgg=FALSE,
    state=NULL,
    county=NULL,
    censusDataset="acs5",
    verbose=FALSE,
    savePath=paste(getwd(),"/data/VRE/",sep="")
){
  
  ## Deal with "no visible binding for global variable" error 
  name <- NAME <- ORDER <- ESTIMATE <- Var_Rep1 <- 
    Var_Rep80 <- estimate <- geoid <- GEOID <- NULL
  
  if(inherits(state,"character")){
    stop("get_vre_table ERROR: You must supply a FIPS code for state.")
  }
  if(inherits(county,"character")){
    stop("get_vre_table ERROR: You must supply a FIPS code for county")
  }
  # # warning("[!!] >>> TODO: You've got it set up to do aggregates, but you need 
  #  to find a way to loop through the individual subvariables when you want to 
  #  display margins of error on individual bars. That is, you need to treat 
  #  individual bars in a loop, with a summary dataframe to draw from, otherwise 
  #  you are going to end up having the same MOE for each bar. It's correct to 
  #  aggregate the geographies, or to aggregate subvars if you're going to provide 
  #  summary data. But if side-by-side individual bars, they have to have separate 
  #  MOEs.")
  
  ACS <- acs_vars_builder(year)
  ACS.VARS <- ACS[[1]]
  ACS.TABLES <- ACS[[2]]
  
  #Convert variableList to simple IDs (VRE tables use simple ID).
  variableList <- ACS.VARS %>% filter(name %in% variableList)
  variableList <- as.numeric(sprintf("%01d",as.numeric(variableList$varID)))
  
  # If not present, grab the table of FIPS codes for simplicity.
  # if(exists("fips_codes")){
  #   fips_codes <- tigris::fips_codes
  #   states_codes <- fips_codes %>% select(state,state_code,state_name)
  #   states_codes <- unique(states_codes)
  #   #assign("fips_codes",fips_codes,envir = .GlobalEnv)
  #   #assign("fips_codes_state",states_codes,envir = .GlobalEnv)
  #   }
  # 
  # # Get FIPS codes
  #  if(!is.null(state)){
  #   stateCode <- fips_codes_state[fips_codes_state$state==state | fips_codes_state$state_name==state,]
  #   stateCode <- stateCode$state_code
  #  }
  # if(!is.null(county)){
  #   countyCode <- fips_codes %>% filter(grepl(county,county))
  #   countyCode <- countyCode$county_code
  #   }
  
  stateCode <- state
  countyCode <- county
  
  # Get a list of variables.
  VARS <- ACS.TABLES
  
  # Check to see if we have the VRE table saved/converted to speed things up.
  # If it doesn't yet exist, grab the VRE file and unpack, and save it as an
  # RDS file.
  
  # GeoSummaryLevels
  # see https://www.census.gov/newsroom/blogs/random-samplings/2014/08/understanding-geographic-relationships-geographic-summary-levels.html
  summaryLevelTable <- data.frame(
    geoComponent = c("us","state","county","county subdivision","tract","block"),
    sumLevel = c(010,040,050,060,140,150)
  )
  compression <- ifelse(year>=2019,".zip",".gz")
  summaryLevelCode <- summaryLevelTable[which(summaryLevelTable$geoComponent==geography),]$sumLevel
  if(geography=="us"){
    fileVar <- tableID
  }else{
    fileVar <- paste(tableID,"_",stateCode,sep="")
  }
  acsFTP <- paste("https://www2.census.gov/programs-surveys/acs/replicate_estimates/",year,"/data/5-year/",sep="")
  acsFTP <- paste(acsFTP,sprintf("%03d",summaryLevelCode),"/",fileVar,".csv",compression,sep="")
  acsSaveFile <- paste(savePath,fileVar,".csv",compression,sep="")
  
  saveFileRDS <- paste(savePath,fileVar,".RDS",sep="")
  
  if(file.exists(saveFileRDS)){
    if(verbose==TRUE)message("Hey-o, we've got a local file!")
    VREall <- readRDS(saveFileRDS)
  }else{
    if(verbose==TRUE)message("Attempting download of VRE files...")
    download.file(acsFTP,acsSaveFile)
    message("Parsing file and re-saving as RDS...")
    if(compression==".zip"){
      VREall <- read.csv(unzip(acsSaveFile))
    }else{
      VREall <- read.csv(gzfile(acsSaveFile))
    }
    saveRDS(VREall,saveFileRDS)
    if(verbose==TRUE)message("Removing download...")
    file.remove(acsSaveFile)
    message("Done.")
  }
  
  GEOID_Base <- "1400000US"
  TBLID <- tableID
  vNums <- NULL
  for(i in 1:length(variableList)){
    vNums <- c(vNums,sprintf("%03d",variableList[i]))
  }
  
  ## Match VRE to census tracts list
  
  # I need to account for multiple variables that *aren't* aggregated.
  # That is, creating MOE for each subvariable, separately, while aggregating
  # geographies. So creating a for() loop to accomplish this. If aggregate vars,
  # then we're just running a for() loop with n=1.
  
  ifelse(variableAgg==TRUE,vln <- 1,vln <- length(variableList))
  
  # make common column for matching
  data <- data %>% mutate(geoid_full = paste(GEOID_Base,geoid,sep=""))

  for(v in 1:vln){
    
    if(variableAgg==TRUE){
      VRE <- VREall %>% filter(GEOID %in% data$geoid_full,ORDER %in% variableList)
    }else{
      VRE <- VREall %>% filter(GEOID %in% data$geoid_full,ORDER == variableList[v])
    }
    
    if(year>=2019){
      VREsum <- VRE %>% reframe(across(c(ESTIMATE,Var_Rep1:Var_Rep80),sum))
    }else{
      VREsum <- VRE %>% reframe(across(c(estimate,Var_Rep1:Var_Rep80),sum))
    }
    
    VREsumVector <- NULL
    VREdiffVector <- NULL
    VREsqVector <- NULL
    
    for(j in 1:89){
      if(j==6){
        valueSum <- VREsum[[1]]
        valueDiff <- NA
        valueSq <- NA
      }else if(j>9){
        valueSum <- as.numeric(VREsum[[(j-8)]])
        valueDiff <- as.numeric(valueSum-VREsum[[1]])
        valueSq <- as.numeric(valueDiff^2)
      }else{
        valueSum <- NA
        valueDiff <- NA
        valueSq <- NA
      }
      VREsumVector <- c(VREsumVector,valueSum)
      VREdiffVector <- c(VREdiffVector,valueDiff)
      VREsqVector <- c(VREsqVector,valueSq)
      
    } # End for() loop
    
    VRECalc <- rbind(VRE,VREsumVector,VREdiffVector,VREsqVector)
    VRECalcSum <- VRECalc[nrow(VRECalc),] %>% select(10:89)
    VREvariable <- paste(TBLID,"_",sprintf('%03d',variableList[v]),sep="")
    VREvariance <- as.numeric(rowSums(VRECalcSum,na.rm=TRUE))*(4/80)
    VREse <- sqrt(VREvariance)
    VREmoe <- round(VREse*1.645,1)
    
    if(v>1){
      y <- data.frame(variable = VREvariable,variance = VREvariance,se=VREse,moe=VREmoe)
      x <- rbind(x,y)
    }else{
      x <- data.frame(variable = VREvariable,variance = VREvariance,se=VREse,moe=VREmoe)
    }
  } # End for() loop
  
  return(x)
  
}

# speedtest-----------------------------------

#' Speedtest
#'
#' @param x First comparison object
#' @param y Second comparison object
#'
#' @return data.frame with time comparisons.
#'
#' @examples \dontrun{
#' speedtest(x,y)
#' }
#' 
speedtest <- function(x,y){
  ## COMP 1
  st1 <- Sys.time()
  # >>>>>start<<<<<<
  
  tigris::fips_codes
  
  # >>>>>end<<<<<<
  a <- round(difftime(Sys.time(),st1,units="sec"),5)
  
  ## COMP 2
  st2 <- Sys.time()
  # >>>>>start<<<<<<
  
  
  
  # >>>>>end<<<<<<
  b <- round(difftime(Sys.time(),st2,units="sec"),5)
  
  print(data.frame(unit = c("capi","tidycensus"),time=c(a,b)))
}

# stat_table_builder-----------------------------------

#' Utility: Stat Table Builder
#' 
#' A utility function for generating a dataframe object of all census tracts in 
#' the United States, with variables from a variable list, Additionally, can calculate
#' distribution statistics.
#'
#' @param data A data object generated by "master_list" set to TRUE, to create summary_table.
#' @param summary_table Set to `TRUE` to create basic distribution statistics. 
#' @param master_list Set to `TRUE` to download census tracts with variable list provided. 
#' @param compiler Set to `TRUE` to stitch downloaded master list files into a single master file.
#' @param variables Variable list (vector) object to pass to ACS call.
#' @param geography Options to get "block group","tract","county" or "state".
#' @param test Testing variable. Uses a limited data call for internal testing.
#' @param tableID profileList object to pass to ACS call.
#' @param geosObject Optional geosObject to speed up processing time.
#' @param saveProgress Logical parameter to save individual downloaded files to
#'   /data/ folder to reduce runtime and memory load.
#' @param stateStart Related to saveProgress, if the process is interrupted, a 
#' state FIPS code to begin the process at. 
#' @param verbose Logical parameter to specify whether to produce verbose output.   
#'
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom data.table rbindlist
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#' # Run summary statistics.
#' stat_table_builder(data=profile_all_summary_stats,
#' summary_table=TRUE) 
#' # Download all census tracts.
#' stat_table_builder(master_list=TRUE) 
#' }
stat_table_builder <- function(data=NULL,
                               summary_table=FALSE,
                               master_list=FALSE,
                               compiler=FALSE,
                               tableID=NULL,
                               variables=NULL,
                               geography="tract",
                               test=FALSE,
                               geosObject=NULL,
                               saveProgress=FALSE,
                               stateStart=NULL,
                               verbose=FALSE){
  
  ## Deal with "no visible binding for global variable" error 
  GEOID <- STATEFP <- COUNTYFP <- NULL
  
  if(compiler==TRUE){
    relPath <- "data/statfiles/"
      message("Setting file path...")
      filepath <- relPath
      message("Getting file list...")
      filelist <- list.files(filepath)
      datalist <- list()
      message("Building file array...")
      for(i in 1:length(filelist)){
        message(paste(" > for",filelist[i]," [",round(i/length(filelist),2)*100,"%]..."))
        name <- filelist[i]
        dp <- list()
        dp[[name]] <- readRDS(paste(filepath,filelist[i],sep=""))
        datalist <- append(datalist,dp)
      }
      message("Binding data array into cumulative file...")
      datafull <- data.table::rbindlist(datalist)
      dffn <- paste(filepath,"stat_table_cumulative.RDS",sep="")
      saveRDS(datafull,dffn)
      if(exists(dffn))message("Saved cumulative data successfully.")
  }else{
    
  ## Initial error checking
  if(is.null(tableID)){
    stop("You're going to need to supply a vector of tableIDs.")
  }
  
    ## Since we're mainly interested in the summary df, we can do this in one shot, if summary_table==TRUE and data==NULL.
    ## Otherwise, just download master_list, or rerun with master_list df and create summary table.
    # Start by getting geos. 
    
    ##TODO clean this up to cohere with logic in geo_var_builder().
    geo <- geo_var_builder(geography=geography,try="local",geosObject = geosObject)
    
    if(geography=="block group"){
      #geos <- geo_var_builder(geography="block groups",try="local")
      geo <- geo$geo_blocks
    }else if(geography=="tract"){
      #geos <- geo_var_builder(geography="tract",try="local")
      geo <- geo$geo_tracts
    }else if(geography=="county"){
      #geos <- geo_var_builder(geography="county",try="local")
      geo <- geo$geo_counties
    }else if(geography=="state"){
      #geos <- geo_var_builder(geography="state",try="local")
      geo <- geo$geo_states
    }else{
      stop("Error: geography not specified")
    }
    
    ## Get a list of states.
    if(exists("geos",envir = .GlobalEnv)){
      geos <- geos
    }else{
      geos <- geo_var_builder(geography="state",try="local")  
    }
    states <- geos$geo_states
    states <- states %>% filter(GEOID<=56)
    if(!is.null(stateStart))states <- states %>% filter(GEOID>=stateStart)
    states <- as.vector(as.numeric(states$GEOID))
    states <- sort(states)
    ## Set params for testing suite. 
    # if(test==TRUE){
    #   states <- 17
    #   county <- 043
    #   if("STATEFP" %in% names(geo)){
    #     geo <- geo %>% filter(STATEFP==states)
    #   }
    #   if("COUNTYFP" %in% names(geo)){
    #     geo <- geo %>% filter(COUNTYFP==county)
    #   }
    # }else{
       county <- NULL
    # }
    
    if(master_list==TRUE & is.null(data)){
      # Skip messaging if test is true
      if(test==FALSE){
        sizeRows <- nrow(geo)*length(variables)
        sizeFile <- round((sizeRows*121)/10^9,3)
        
        if(summary_table==TRUE & is.null(data)){
          user_input <- readline(stringr::str_wrap(paste("HEADS UP: This will generate a large dataframe (",format(sizeRows,big.mark=","),"rows; approx.",sizeFile,"GB). In addition, `summary_table=TRUE`, this will return ONLY the summary table.  For `master_list`, rerun with `master_list` set to TRUE. Proceed? (y/n)"),width=90))
        }else{
          user_input <- readline(
            
            stringr::str_wrap(paste("HEADS UP: This will generate a large dataframe (",format(sizeRows,big.mark=","),"rows; approx.",sizeFile,"GB). In addition, `summary_table=TRUE`, this will return ONLY the summary table.  For `master_list`, rerun with `master_list` set to TRUE. Proceed? (y/n)"),width=90))
        }
      }else{
        user_input <- "y"
      }
      if(user_input!="y"){
        stop("OK, quitting.")
      }else{
        ## Loop through get_acs() requests by state and append to df.
        data <- NULL
        if(test==FALSE){
          pb <- txtProgressBar(min = 1, max = length(states), style = 3)
        }
        for(i in 1:length(states)){
          if(verbose==TRUE)message(paste("Iteration ",i,"/",length(states)," (",round(i/length(states),1)*100,"%)",sep=""))
          x <- profiler(tableID = tableID,
                        variables = variables,
                        year=2022,
                        state=states[i],
                        county=county,
                        geography=geography,
                        verbose = verbose,
                        test=test,
                        fast=TRUE) 
          x <- x$data$type1data
          # data <- data.table::rbindlist(list(data,x))
          #data <- rbind(data,x)
          if(saveProgress==TRUE){
            filename_i <- paste("~/DATA/censusprofileR/data/statfiles/stat_table_",sprintf("%02d",states[i]),".RDS",sep="")
            # filename_d <- "~/DATA/censusprofileR/data/statfiles/stat_table_cumulative.RDS"
            saveRDS(x,filename_i)
            # saveRDS(data,filename_d)
            #if(file.exists(filename_i) && file.exists(filename_d)){
            if(file.exists(filename_i)){
              message("Saved progress.")
            }else{
              stop("error saving progress.")
            }
          }else{
            data <- rbind(data,x)
          }
          if(test==FALSE)setTxtProgressBar(pb, i)
        }
        if(test==FALSE)close(pb)
        
        if(summary_table==FALSE){
          return(data)
        }
      }
    }

    ## Logics for returning a summary table of dataframe.
    if(summary_table==TRUE){
      if(is.null(data)){
        stop("Dataframe requred to parse and summarize.")
      }
      vars <- variables
      if(verbose==TRUE)message(paste("Calculating summary_table. This may take a while. Processing ",nrow(data)," rows, and ",length(vars)," variables.",sep=""))
      st <- Sys.time()
      z <- NULL
      
      for(i in 1:length(vars)){
        if(verbose==TRUE)message(paste(dur(st),"Iteration ",i),sep="")
        sti <- Sys.time()
        if(verbose==TRUE)message(paste(dur(st),"Filtering data",sep=""))
        #subdf = data %>% filter(variable==vars[i])
        subdf <- data[data$variable==vars[i],]
        if(verbose==TRUE)message(paste(dur(st),"Performing calculations...",sep=""))
        
        if(verbose==TRUE)message(paste(dur(st),"...calculating sd_pct",sep=""))
        sd_pct <- sd(subdf$pct_by_type,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating sd_est",sep=""))
        sd_est <- sd(subdf$estimate,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating mean_pct",sep=""))
        mean_pct <- mean(subdf$pct_by_type,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating mean_est",sep=""))
        mean_est <- mean(subdf$estimate,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating median_pct",sep=""))
        median_pct <- median(subdf$pct_by_type,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating median_est",sep=""))
        median_est <- median(subdf$estimate,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating max_pct",sep=""))
        max_pct <- max(subdf$pct_by_type,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating max_est",sep=""))
        max_est <- max(subdf$estimate,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating min_pct",sep=""))
        min_pct <- min(subdf$pct_by_type,na.rm=TRUE)
        if(verbose==TRUE)message(paste(dur(st),"...calculating min_est",sep=""))
        min_est <- min(subdf$estimate,na.rm=TRUE)
        
        d <- data.frame(
          variable = vars[i],
          sd_pct = sd_pct,
          sd_est = sd_est,
          mean_pct = mean_pct,
          mean_est = mean_est,
          median_pct = median_pct,
          median_est = median_est,
          max_pct = max_pct,
          max_est = max_est,
          min_pct = min_pct,
          min_est = min_est
        )
        if(verbose==TRUE)message(paste(dur(st),"Combining data",sep=""))
        if(i==1){
          df <- d
        }else{
          df <- rbind(df,d)
        }
        x <- round(difftime(Sys.time(),st,units = "sec"),2)
        y <- round(difftime(Sys.time(),sti,units = "sec"),2)
        z <- c(z,y)
        zm <- mean(z)
        zr <- ((length(vars)-i)*zm)/60
        if(i>1){
          if(verbose==TRUE)message(paste("Time elapsed: ",x," seconds | Iteration: ",y," seconds | Est. time remaining: ",zr," minutes."))
        }
      }
      
      return(df)
      
    }else{
      stop("No parameters specified.")
    }
    #on.exit(rm(data,df))
  }
}
# stat_helper-----------------------------------
#' Utility: Stat Helper
#' 
#' A function to a) mutate a dataframe of profile variables with Z-scores, and/or b) 
#' to test whether individual entries are significant on Rmd reports.
#'
#' @param data A profile data object created using create_profile(). 
#' @param year Year for data call.
#' @param statTable A dataframe of census tracts or blocks created with stat_table_builder. Input can receive either object or file path.
#' @param entryNum For looping. If a single use, value is `1`. Otherwise, dynamically populate.
#' @param zThresh The threshold for z-score divergence.
#' @param zType Whether we're examining relative (`pct`) or absolute (`estimate`).
#' @param typeFilter Numeric value to filter type levels in analysis.
#' @param tableID The tableID value for filtering.
#' @param variable The variable value for filtering.
#' @param dataType Allows us to specify whether this is a df, dfCount, or dfNoSummary table.
#' @param varsObject A dataframe containing census variables from the ACS or
#'   other dataset.
#' @param verbose Set to `TRUE` to print output on rmarkdown (or other) report.
#' @param variables List of tableID names to test tableID input.
#' 
#' @importFrom tools file_ext
#'
#' @return Either a mutated dataframe, or text output describing significqant z-score divergences.
#'
#' @examples
#' \dontrun{
#' stat_helper(df,entryNum=1,zThresh=1.5,zType="pct",tableID="B01001",dataType="df",verbose=TRUE)
#' }
stat_helper <- function(data,
                        statTable=NULL,
                        variables=NULL,
                        entryNum=1,
                        zThresh=1.5,
                        zType="pct",
                        tableID=NULL,
                        typeFilter=NULL,
                        variable=NULL,
                        dataType=4,
                        year=2022,
                        varsObject=NULL,
                        verbose=FALSE
){
  
  ## Deal with "no visible binding for global variable" error 
  tableID.x <- pct <- mean_pct <- sd_pct <- estimate <- mean_est <- sd_est <- 
    zScore_pct <- zScore_est <- table_id.x <- table_id <- NULL
  
  if(is.null(varsObject)){
    stop("To run efficiently, please include master census vars in stat_formula() execution.")
  }else{
    ACS.VARS <- varsObject[[1]]
    ACS.TABLES <- varsObject[[2]]
  }
  
  if(verbose==TRUE)message(" -- stat_helper | checking datatype")
  dtype <- type_data(data)
  if(dtype==5){
    data <- data$data[[dataType]]
  }else{
    if(as.numeric(dataType)!=dtype){
      stop("There's a data-type mismatch between data supplied, and dataType specified.")
    }
  }
  
  if(verbose==TRUE)message(" -- stat_helper | checking statTable exists")
  if(is.null(statTable)){
    stop("No statTable object included. Run stat_table_builder() to create object.")
  }
  
  df <- data

  if(verbose==TRUE)message(" -- stat_helper | loading variables/table_ids")

   if(!is.null(tableID)){
     if(verbose==TRUE)message(" -- stat_helper | checking tableID inputs")
     TILIST <- ACS.TABLES$table_id
    if(!(tableID %in% TILIST)){
      stop("Whoops, it appears that this table_id doesn't appear in variables.")
    }
      if(verbose==TRUE)message(" -- stat_helper | filtering via tableID")
      df <- df[df$table_id==tableID,]
   }
  
  ## Filter by row type
  if(!is.null(typeFilter)){
    if(is.numeric(typeFilter)){
      tf <- c()
      for(b in 1:length(typeFilter)){
        tfa <- case_when(
          typeFilter[b] == 1 ~ "root",
          typeFilter[b] == 2 ~ "summary",
          typeFilter[b] == 3 ~ "level_1",
          typeFilter[b] == 4 ~ "level_2",
          typeFilter[b] == 5 ~ "level_3",
          typeFilter[b] == 6 ~ "level_4",
          .default = "other"
        )
        tf <- c(tf,tfa)
      }
      typeFilter <- tf
    }
    df <- df[df$type %in% typeFilter,]
  }

  ## Filter if variable params.
  if(!is.null(variable)){
    if(verbose==TRUE)message(" -- stat_helper | filtering via variable")
    if(is.numeric(variable)){
      varfix <- c()
      for(a in 1:length(variable)){
      varfix <- c(varfix,paste(tableID,"_",sprintf("%03d",variable[a]),sep=""))
      }
      variable <- varfix
    }
    df <- df[df$variable %in% variable,]
  }

  ## Connect profile_all_summary_stats object to current filtered df.
  df <- left_join(ungroup(df),statTable,by="variable")
  # Mutate to add zScores.
  df <- df %>% mutate(zScore_pct=(pct-mean_pct)/sd_pct, 
                      zScore_est=(estimate-mean_est)/sd_est)
  
  if(zType=="pct"){
    df <- df %>% filter(zScore_pct >= zThresh)
  }else{
    df <- df %>% filter(zScore_est >= zThresh)
  }

  if(nrow(df)>0){
    if(verbose==TRUE){
      note <- paste0("*Special Note*  \n",
                     "The following variables have a significant z-score, relative to the national average. This may have significant implications for understanding your community.\n\n  ")
      for(w in 1:nrow(df)){
        note <- paste0(note,
                       "- ",
                       df[w,"labels"],
                       " is ",
                       round(df[w,"pct"]*100,2),
                       "%, which is ",
                       round(df[w,"zScore_pct"],2),
                       " standard deviations from the mean of ",
                       round(df[w,"mean_pct"]*100,2),"%.\n  ")
      }
      cat(note)
    }else{
      return(df) 
    }
  }
}

# type_data ---------------------------------------------------------------

# type_data

#' Type Data
#'
#' @param dataset A dataset to run type_data() on.
#' @param return Whether to return the dataframe (TRUE) or type (FALSE)
#'
#' @return numeric value
#'
#' @examples \dontrun{
#' type_data(x)
#' }
type_data <- function(dataset,
                      return=FALSE){
  ## Check to see if we have a full profile object.
  if(all(class(dataset)=="list")==TRUE){
    if(all(names(dataset)==c("info","data"))==TRUE &&
     all(names(dataset$data)==c("type1data",
                                "type2data",
                                "type3data",
                                "type4data"))==TRUE){
    check <- 5
    df <- dataset 
    ## Check to see if we have a direct profile object (no info)
    }else if(all(names(dataset$data)==c("type1data",
                                        "type2data",
                                        "type3data",
                                        "type4data"))==TRUE){
     check <- 6
     df <- dataset                                     
    }else{
      stop("!> Requires a censusprofiler object. Create one using profiler().")
    }
    ## If not profile, check to see if this is a single valid censusprofiler data object.
  }else if(all(class(dataset)==c("grouped_df",
                                 "tbl_df",
                                 "tbl",
                                 "data.frame"))==TRUE && 
           all(c("table_id",
                 "varID",
                 "dt") %in% names(dataset))){
    check <- unique(dataset$dt)
    ##ADDTEST 
    if(length(check)>1)stop("!> Error in data check. Multiple values in dt column.")
    df <- dataset 
  }else{  
    stop("!> Requires a censusprofiler object. Create one using profiler().")
  }
  if(return==TRUE){
    return(df)
  }else{
    return(check)
  }
}

# variable_builder-----------------------------------

#' Variable Builder
#'
#' A function to loop through and grab variables in a nested list for further
#' analysis.
#'
#' @param tableID Variable base, prior to the underscore.
#' @param censusVars Reference to variable table.
#' @param varStartNum Start number of variables.
#' @param varEndNum End number of variables.
#' @param varSummaryNum Summary number.
#' @param varArray A manual list if need be.
#'
#' @return Returns either a vector, or a nested list.
#'
#'
#' @examples
#' \dontrun{
#' variable_builder(c("B01001","B01002"),censusVars=ACS.VARS,
#' varStartNum = c(1,1),varEndNum=c(3,3), varArray=NULL,varSummaryNum = c(1,1))
#' }
variable_builder <- function(tableID=NULL,
                             censusVars=NULL,
                             varStartNum=NULL,
                             varEndNum=NULL,
                             varSummaryNum=NULL,
                             varArray=NULL){
  ACS.VARS <- censusVars
  x <- NULL
  varLists <- list()
  allVars <- NULL
  
  if(is.null(varStartNum) & is.null(varArray)){
    stop("You need to supply the proper start number for the variable.")
  }
  
  ## Account for if multiple values are given, but only a single start/end/summary
  ## value is given. This throws a warning.
  if(is.null(tableID)){
    lvb <- 1
  }else{
    lvb <- length(tableID)
  }
  
  if(length(varStartNum)!=lvb & lvb>1){
    warning("Are you sure you didn't mean to include a list for start values?")
    a <- NULL
    for(d in 1:lvb){
      a <- c(a,varStartNum)
    }
    varStartNum <- a
  }
  if(length(varEndNum)!=lvb & lvb>1){
    warning("Are you sure you didn't mean to include a list for end values?")
    b <- NULL
    for(e in 1:lvb){
      b <- c(b,varEndNum)
    }
    varEndNum <- b
  }
  
  # Often, we use a summary variable to do calculations. If unspecified, select 001.
  varSummary <- NULL
  warning <- NULL
  for(q in 1:lvb){
    if(is.null(varSummaryNum)){
      vS <- paste(tableID[q],"_001",sep="")
      warning <- c(warning,TRUE)
    }else if(length(varSummaryNum)==1 & lvb>1){
      warning <- c(warning,TRUE)
      vS <- paste(tableID[q],"_",sprintf("%03d",varSummaryNum),sep="")
    }else{
      vS <- paste(tableID[q],"_",sprintf("%03d",varSummaryNum[q]),sep="")
    }
    varSummary <- c(varSummary,vS)
  }
  
  if(!is.null(warning)){
    #     warning("Next time, it's recommended to include a list of summary vars. Otherwise you just end up repliacating the same number, which is unreliable.")
  }
  varSummary <- varSummary
  
  # VarArray specifies the exact variables/subvariables to call. Otherwise, use the manual start/end.
  if(!is.null(varArray)){
    variableList <- varArray
  }else{
    ## This logic allows for us to include multiple variable bases in this
    ## variable builder. Additionally, we can pass arrays of start/end numbers.
    for(v in 1:length(tableID)){
      variableList <- NULL
      varShortList <- NULL
      xdf <- NULL
      
      # Find the range of variables in the event that the varEndNum is NULL.
      if(is.null(varEndNum)){
        varEnd <- 1
        #   while(sum(str_detect(censusVars$name,paste("^",tableID[v],"_",sprintf("%03d",varEndNum[v]),sep="")))>0){
        while(sum(str_detect(censusVars$name,paste("^",tableID[v],"_",sprintf("%03d",varEnd),sep="")))>0){
          varEnd <- varEnd + 1
        }
        varEndNum <- varEnd-1 ## Account for loop logic; it'll always advance to the next one prior to testing.
      }
      
      # Logic if we're dealing with a single variable number.
      for(i in varStartNum[v]:varEndNum[v]){
        varBuild <- paste(tableID[v],"_",sprintf("%03d",i),sep="")
        variableList = c(variableList,varBuild)
        varShortList = c(varShortList,i)
      }
      varLists[[v]] <- variableList
    }
  }
  # Compile all variable lists.
  if(is.null(varArray)){
    allVars <- c(allVars,varSummary,variableList)
  }else{
    allVars <- variableList
  }
  
  ## Put a nested list together of all variables constructed so far.
  x <- list()
  if(is.null(varArray)){
    for(a in 1:lvb){ # Loop through tableID to assign subgroups by var.
      x[[paste(tableID[a],"_vars",sep="")]] <- varLists[[a]]
      x[[paste(tableID[a],"_summary_var",sep="")]] <- varSummary[[a]]
    }
  }
  x[[paste("allVars")]] <- allVars # Add a master list as well.
  x[[paste("allSummaryVars")]] <- varSummary # Add a master list as well.
  badVars <- setdiff(x$allVars,ACS.VARS$name)
  if(length(badVars)>0){
    bv <- NULL
    for(z in 1:length(badVars)){
      bv <- paste(bv,badVars[z],"\n")
    }
    badVars <- bv
    stop(paste("ERROR: You have malformed variables that do not occur in the ACS variable list.
                 \n The following do not exist:",badVars))
  }
  return(x)
}

# dur--------------------------------------
dur <- function(st){
  x <- paste(format(round(difftime(Sys.time(),st,units = "sec"),4),scientific=FALSE)," | ")
  return(x)
}

# tableID_pre_check--------------------------------------
# Table Profile selection
tableID_pre_check <- function(x=NULL,acsMatch=FALSE,ACS=NULL){
  if(acsMatch==FALSE){
    if(is.null(x)){
      z <- c("NO_TABLE_ID","CHECK")
    }else{
      z <- list()
      for(i in 1:length(x)){
        num <- as.character(c(0:9))
        zi <- case_when(
          stringr::str_starts(x[i],"B") & 
            stringr::str_sub(x[i],2,2) %in% num ~ c("B",""),           # Detailed Tables: Base Table
          stringr::str_starts(x[i],"CP") & 
            stringr::str_sub(x[i],3,3) %in% num~ c("CP","cprofile"), # Comparison Profile
          stringr::str_starts(x[i],"C") &
            stringr::str_sub(x[i],2,2) %in% num ~ c("C",""),           # Detailed Tables: Collapsed Table
          stringr::str_starts(x[i],"S") & 
            stringr::str_sub(x[i],2,2) %in% num ~ c("S","subject"),    # Subject Table
          stringr::str_starts(x[i],"DP") & 
            stringr::str_sub(x[i],3,3) %in% num~ c("DP","profile"),  # Data Profile
          stringr::str_starts(x[i],"S0201") &
            stringr::str_sub(x[i],6,6) %in% num ~ c("S0201","spp"),# Selected Population Profile
          #stringr::str_starts(x,"R") ~ "",    # Ranking Table (TODO this and below exist, but can't find them rn)
          #stringr::str_starts(x,"GCT") ~ "",  # Geographic Comparison Table
          #stringr::str_starts(x,"K20") ~ "",  # Supplemental Table
          #stringr::str_starts(x,"XK") ~ "",   # Experimental Estimates
          #stringr::str_starts(x,"NP") ~ "",   # Narrative Profile
          .default = c("NOTFOUND","ERROR")
        )
        z <- append(z,zi)
      }
    }
    return(z)
  }else{
    return(match(x,ACS[[1]]$table_id))
  }
}

# variable_pre_check--------------------------------------
# Variable construction checks
variable_pre_check <- function(var=NULL,tipc=NULL){
  if(is.null(tipc))stop("You must include a tipc argument in variable_pre_check().")
  ## tipc must be a tableID_pre_check object
  if(tipc[1]=="NO_TABLE_ID"){
    t <- tableID_pre_check(var)
    u <- grepl("_",var)
    if(t[1]=="NOTFOUND" || u==FALSE){
      stop(paste("!> `",var,"` is malformed. Check variable and try again.",sep=""))
    }
  }
  if(tipc[1]=="NOTFOUND"){ # Did we misspecify tableID in variable?
    stop(paste("!> It looks like `",var,"` is a malformed variable. Check for typos.",sep=""))
  # }else if(tipc[1]!=pseudo_tableID(var)){ # Did we mismatch variable / tableID?
  #   stop("!> It looks like you have a tableID / variable mismatch.")
  }else if(grepl("_",var)==FALSE){
    stop(paste("!> Wrong format for `",var,"`. Variables are either simple numeric c(1,2,3) / c(001,002,003) or in format of `B01001_001`.",sep=""))
  }else{
  return(1)
  }
}

# pseudo_tableID--------------------------------------
pseudo_tableID <- function(vars,
                           fast=FALSE,
                           test=FALSE,
                           verbose=FALSE){
  tid <- NULL
  if(test==FALSE && verbose==TRUE && length(vars)>1)pb <- txtProgressBar(min = 1, max = length(vars), style = 3)
  if(fast==FALSE){
    for(v in 1:length(vars)){
      if(str_detect(vars[v],"_")==FALSE || is.numeric(vars[v])){
        ##TODO I'd like to make an autocorrect for this so we can just include numeric values.
        stop("!> Insufficient data from variables to detect table_id. Please format variables in tableID+num_var format (i.e. `B01001_001`.")
      }
      tids <- stringr::str_split(vars[v],"_")
      tids <- unlist(tids)[1]
      tid <- c(tid,tids)
      if(test==FALSE && verbose==TRUE && length(vars)>1)setTxtProgressBar(pb, v)
    }
  }else{
    tid <- vapply(strsplit(vars,"_",fixed=TRUE),`[`,1,FUN.VALUE=character(1))
  }
  if(test==FALSE && verbose==TRUE && length(vars)>1)close(pb)
  # Check for errors
  # utid <- unique(tid)
  # for(t in 1:length(utid)){
  #   print(utid)
  #   tidt <- tableID_pre_check(utid[t])
  #   if(tidt[1]=="NOTFOUND"){
  #     ##ADDTEST
  #     stop("!> Malformed or unavailable table ID. Please correct.")
  #   }
  # }
  return(tid)
}

# variable_formatter--------------------------------------
variable_formatter <- function(var=NULL,
                               tipc=NULL,
                               tableID=NULL,
                               ACS=NULL){
  # tipc needs to be a tableID_pre_check object
  if(!is.numeric(var)){ # Check for numeric/badly formed mixed vars
    # Do variable testing.
    #variable_pre_check(var,tipc)
    tids <- pseudo_tableID(var)
    for(a in 1:length(tids)){
      tippt <- tableID_pre_check(tids[a])
      tipptm <- tableID_pre_check(tids[a],acsMatch = TRUE,ACS=ACS)
      tippt <- tippt[1]
      if(tippt=="NOTFOUND"){
        stop(paste("!> There is no table ID for `",tids[a],"` Check for typos.",sep=""))
      }
      if(is.na(tipptm)){
        stop(paste("!> There is no table ID for `",tids[a],"` Check for typos.",sep=""))
      }
    }
    # Did we pass the tests? Include.
    x <- var
  }else{ # Numeric only
    x <- paste(tableID,"_",sprintf("%03d",as.numeric(var)),sep="")
  }
  x <- c(paste(x,"E",sep=""),paste(x,"M",sep=""))
  return(x)
}

# # checkSave-------------------------
# checkSave <- function(x=NULL,
#                       test=FALSE){ ## Do I already have a version of this variable?
#   if(exists("profile_variables")){
#     ifelse(test==FALSE,
#            user_input <- readline("Looks like that exists. To overwrite, type y, otherwise type suffix."),
#            user_input <- "y")
#     if(user_input == "y"){
#       prdf <- "profile_variables"
#     }else{
#       prdf <- paste("profile_variables_",user_input,sep="")
#     }
#     assign(prdf,x,envir = .GlobalEnv)
#     message('Saved.')
#   }else{
#     profile_variables <<- x
#   }
# }
# 
# # saveMe------------------
# saveMe <- function(x=NULL,
#                    test=FALSE){
#   ifelse(test==FALSE,
#          user_input <- readline("Do you want to save resultant profile object? (y/n)"),
#          user_inpuyt <- "y")
#   if(user_input!="y"){
#     stop('Exiting without saving.')
#   }else{
#     checkSave(x,test=test)
#   }
#   #stop('Exiting since you did not press y')
# }

# summarize_data-------------------------
summarize_data <- function(data=NULL,
                           geography=NULL,
                           filterRadius=NULL,
                           state=NULL,
                           county=NULL,
                           tract=NULL,
                           block_group=NULL){
  
  table_id <- year <- variable <- concept <- calculation <- 
    type <- varID <- estimate <- subtotals <- NULL
  
  if(is.null(data) || is.null(geography)){
    stop("There are missing arguments in summarize_data().")
  }
  ## This is `type 3 data`
  data <- data %>% group_by(table_id,year,variable,concept,labels,calculation,type,varID) %>% summarize(estimate=sum(estimate))
  data <- data %>% group_by(table_id,type) %>% mutate(subtotals=sum(estimate))
  data <- data %>% mutate(pct = estimate/subtotals)
return(data)
  stop()
  # Add geographical details.
  if(geography=="state"){
    data <- data %>% mutate(state=unique(state))
  }else if(geography=="county"){
    data <- data %>% mutate(state=unique(state))
    data <- data %>% mutate(county=unique(county))
  }else if(geography=="tract"){
    data <- data %>% mutate(state=unique(state))
    data <- data %>% mutate(county=unique(county))
    data <- data %>% mutate(tract=unique(tract))
    print(data$county)
    stop()
  }else if(geography=="block group"){
    data <- data %>% mutate(state=unique(state))
    data <- data %>% mutate(county=unique(county))
    data <- data %>% mutate(tract=unique(tract))
    data <- data %>% mutate(block_group = unique(block_group))
    #TODO ^ this needs to get cleaned up. It's tricky because block groups 
    # are single numeric values (1,2,3), as opposed to tracts, which are 
    # lengthier and vary across geographies. Need to better identify block 
    # groups.
  }else{
  }
  data <- data %>% mutate(geography = geography)
  if(!is.null(filterRadius))data <- data %>% mutate(radius=filterRadius)
  return(data)
}
