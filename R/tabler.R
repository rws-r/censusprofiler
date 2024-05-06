#' TableR
#'
#' A censusprofiler wrapper for flextables in RDF files.
#'
#' @param mode Options for display calculations: "summarize", "summarizeinc","simple","bygeo","nosummary"
#' @param cols Defaults to "labels","estimate","pct", but can be modified.
#' @param dispPerc Whether to display percentages. This may be set to FALSE when
#' using median or mean data. 
#' @param type Parameter to filter by type ("root","summary","level_1", etc)
#' @param shorten Numeric value specifying the size of a slice.
#' @param sort Option to sort by value.
#' @param pdf Set to TRUE for latex use.
#' @param usCompare Pass comparison dataset offer a national compare value.
#' @param stateCompare Pass comparison dataset offer a state-level compare value.
#' @param summaryLevels How deep shaded levels will display.
#' @param pctFilter A numeric value filtering the minimum percentage shown.
#' @param filterAddress Address of centroid to filter census tracts/blocks.
#' @param filterRadius In miles, the radius for the filter.
#' @param filterByGeoType An irregular geo type to get a smaller overlapping set
#'   of tracts, block_groups or other geography from. Options are currently
#'   "metro", "place","combined_statistical_areas". E.g., Find all tracts in Chicago (place).
#' @param filterByGeoValue A value to find object for filtering. Either NAME or GEOID.   
#' @param state A state value to filter.
#' @param county A county value to filter.
#' @param geography Geography designation for capi().
#' @param year Year designation for capi(). 
#' @param verbose Pass through param to create_profile() for feedback.
#' @param geosObject Optional geosObject object to speed up geo processing.
#' @param data_object A census data object.
#' @param dataset_main Selection parameters for get_census_variables (e.g. "acs")
#' @param dataset_sub Selection parameters for get_census_variables (e.g. "acs5")
#' @param dataset_last Selection parameters for get_census_variables (e.g. "cprofile")
#' @param censusVars Passthrough object to bypass get_census_variables 
#' @param tableID Formerly known as varBase, or concept, or group: i.e., "B01001"
#' @param variables A vector of variables for the call. 
#' @param sort_bygroup Logical flag to specify whether to sort by variable group.
#' @param filterSummaryLevels Lowest summary level to include.
#' @param tract Input (abb. or FIPS) of tract for search.
#' @param block_group Input (abb. or FIPS) of block group for search.
#' @param moe Logical parameter to specify whether margin of error should be calculated.
#' @param groupPerc Logical parameter to specify whether to display percentages by type in tables.
#'
#' @import flextable
#' @import officer
#' @importFrom formattable percent
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr slice_max
#' @importFrom dplyr desc
#' @importFrom formattable percent
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' \dontrun{
#' tabler(data_object=data,datatype="acs",dataset="acs5",mode="simple",
#' tableID="B02001",variables=NULL,summaryLevels=1,)
#'}

tabler <- function(data_object=NULL,
                   mode="summarize",
                   tableID=NULL,
                   variables=NULL,
                   cols=c("labels","estimate","pct"),
                   dispPerc=TRUE,
                   type=NULL,
                   shorten=NULL,
                   sort=FALSE,
                   sort_bygroup=FALSE,
                   pctFilter=NULL,
                   pdf=FALSE,
                   usCompare=NULL,
                   stateCompare=NULL,
                   summaryLevels=1,
                   filterAddress=NULL,
                   filterRadius=NULL,
                   filterByGeoType=NULL,
                   filterByGeoValue=NULL,
                   filterSummaryLevels=NULL,
                   state=NULL,
                   county=NULL,
                   tract=NULL,
                   block_group=NULL,
                   geography=NULL,
                   year=NULL,
                   geosObject=NULL,
                   dataset_main="acs",
                   dataset_sub="acs5",
                   dataset_last=NULL,
                   censusVars=NULL,
                   moe=FALSE,
                   groupPerc=FALSE,
                   verbose=FALSE){
  
  ## To avoid "no visible binding for global variable" error
  estimate <- calculation <- pct <- us_profile <- us_comp <- 
    geo_states <- states_profile <- state_comp <- table_id <- 
    variable <- info <- NULL
  
# Internal functions ------------------------------------------------------
  
  st <- Sys.time()
  dur <- function(st){
    x <- paste(format(round(difftime(Sys.time(),st,units = "sec"),4),scientific=FALSE)," | ")
    return(x)
  }
  
# Error checking ----------------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Error checking..."))
  
  if(is.null(filterAddress) && is.null(filterByGeoType) 
     && is.null(state) && is.null(data_object) && geography!="us"){
    stop("No geogrpahic regions selected/filtered.")
  }
  
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
  
  if(is.null(geography) && is.null(data_object)){
    stop("!> You must provide one of the following geography parameters: `us`,`state`,`county`,`tract`,`block group`,`place`,`region`,`division`,`subdivision`,`consolidated city`")
  }
  if(!is.null(geography)){
    if(geography %in% c("region","division","subminor civil division","place","consolidated city")){
      stop(paste("!> Unfortunately, we cannot provide results for `",geography,"` geographies at this time.",sep=""))
    }
  }
  if(is.null(year) & is.null(data_object)){
    stop("!> You must provide a year parameter.")
  }
  if(is.null(dataset_main)){
    stop("!> You must provide a dataset. Options are `acs` and subsidiaries.")
  }
  if(is.null(tableID) && is.null(variables)){
    stop("!> No variables or tableID included. You need to include either a tableID to capture all subvariables, a tableID with numeric vector, or a complete vector of variables. You can also include a named list with multiple tableIDs and variables.")
  }

# Data selection and preparation-------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Selecting data..."))
  
  if(is.null(data_object)){
    if(verbose==TRUE)message(paste(dur(st),"Trying download..."))
    
    # ## Execute capi() with supplied parameters.
    data_object <- profiler(year=year,
                            tableID=tableID,
                            variables=variables,
                            geography=geography,
                            filterAddress=filterAddress,
                            filterRadius=filterRadius,
                            filterSummaryLevels=filterSummaryLevels,
                            filterByGeoType = filterByGeoType,
                            filterByGeoValue = filterByGeoValue,
                            state=state,
                            county=county,
                            tract=tract,
                            block_group=block_group,
                            censusVars=censusVars,
                            verbose=verbose,
                            st=st)

    if(verbose==TRUE)message(paste(dur(st),"Download success..."))
    # Check data_object 
    check <- type_data(data_object,FALSE)
    df <- type_data(data_object,TRUE)
    
  }else{
    if(verbose==TRUE)message(paste(dur(st),"Using supplied dataframe...checking..."))
    
    # Check data_object 
   check <- type_data(data_object,FALSE)
   df <- type_data(data_object,TRUE)
   
   if(verbose==TRUE)message(paste(dur(st),"Supplied dataframe looks good..."))
   
  }

  # Capture year from data_object 
  if(verbose==TRUE)message(paste(dur(st),"Getting year..."))

  if(is.null(year)){
    if(check==5){
      year <- df$info$year
    }else{
      year <- unique(df$year)
    }
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

  ## Select proper sub-data.
  if(verbose==TRUE)message(paste(dur(st),"Selecting correct subdata..."))

  if(mode=="summarize"){
    if(check==5){
      df <- df$data$type4data
      info <- data_object$info
    }else if(check==6){
      df <- df$type4data
    }else if(check==4){
      df <- df
    }else{
      #ADDTEST
      if(check==3){
        stop("!> Invalid data type for `summarize` mode. Requires type 4 data. Did you mean to use mode `summarizeinc`?")
      }else{
        stop("!> Invalid data type for `summarize` mode. Requires type 4 data.")
      }
    }
  }else if(mode=="summarizeinc"){
      if(check==5){
        df <- df$data$type3data
        info <- data_object$info
      }else if(check==6){
        df <- df$type3data
      }else if(check==3){
        df <- df
      }else{
        #ADDTEST
        if(check==4){
          stop("!> Invalid data type for `summarizeinc` mode. Requires type 3 data. Did you mean to use mode `summarize`?")
        }else{
          stop("!> Invalid data type for `summarizeinc` mode. Requires type 3 data.")
        }
      }
  }else if(mode=="simple"){
    if(check==5){
      df <- df$data$type1data
      info <- data_object$info
    }else if(check==6){
      df <- df$type1data
    }else if(check==1){
      df <- df
    }else{
      #ADDTEST
      if(check==2){
        stop("!> Invalid data type for `simple` mode. Requires type 1 data. Did you mean to use mode `nosummary`?")
      }else{
        stop("!> Invalid data type for `simple` mode. Requires type 1 data.")
      }
    }
  }else if(mode=="nosummary"){
    if(check==5){
      df <- df$data$type2data
      info <- data_object$info
    }else if(check==6){
      df <- df$type2data
    }else if(check==2){
      df <- df
    }else{
      #ADDTEST
      if(check==1){
        stop("!> Invalid data type for `nosummary` mode. Requires type 2 data. Did you mean to use mode `simple`?")
      }else{
        stop("!> Invalid data type for `nosummary` mode. Requires type 2 data.")
      }
    }
  }else if(mode=="bygeo"){
    if(check==5){
      df <- df$data$type1data
      info <- data_object$info
    }else if(check==6){
      df <- df$type1data
    }else if(check==1){
      df <- df
    }else{
      #ADDTEST
      if(check==2){
        stop("!> Invalid data type for `simple` mode. Requires type 1 data. Did you mean to use mode `nosummary`?")
      }else{
        stop("!> Invalid data type for `simple` mode. Requires type 1 data.")
      }
    }
  }else{
    #ADDTEST
    stop("!> Invalid mode entry. Available options are `simple`,`nosummary`,`summarize`,`summarizeinc`.")
  }

## tableID and variable checks ---------------------------------------------

  if(verbose==TRUE)message(paste(dur(st),"Checking table_id..."))
  
  if(length(unique(df$table_id))>1){
    if(tableID=="all"){
      tableID <- unique(df$table_id)
    }else if(!is.null(tableID)){
      tableID <- tableID
      df <- df %>% filter(table_id==tableID)
    }else{
      #ADDTEST
      stop("!> Too many tableIDs present, with no selection parameters given. If all intended, set tableID='all'.")
    }
  }else{
    tableID <- unique(df$table_id)
  }
  

## In-function modifications (from `mode`) ---------------------------------

  if(mode=="mean"){
    dfm <- df %>% group_by(variable) %>% summarize(estimate=mean(estimate))
    df <- df %>% select(variable,calculation,labels,type) %>% unique()
    df <- left_join(df,dfm,by="variable")
  }
  
## Filtering and sorting ---------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Filtering / sorting..."))
  
  if(!is.null(type)){
    df <- df %>% ungroup() %>% filter(type==!!type)
  }

  if(dispPerc==TRUE){
    df$pct <- formattable::percent(df$pct,digits=0)  
  }
  if(groupPerc==TRUE){
    df$pct_by_type <- formattable::percent(df$pct_by_type,digits=0)  
  }
  
  if(pdf==TRUE){
    fmt <- "latex"
  }else{
    fmt <- "html"
  }
  
  ## Apply any filtering.
  if(!is.null(shorten)){
    df <- df %>% ungroup() %>% slice_max(order_by=estimate,n=shorten)
  }
  if(sort==TRUE){
    df <- df %>% arrange(desc(estimate),.by_group = sort_bygroup)
  }
  if(!is.null(pctFilter)){
    df <- df %>% ungroup() %>% filter(pct >= pctFilter)
  }

## Get type values for table coloring -----------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Getting variable types for table display..."))
  
  ## Functionality to get list of rows with type, so I can format using flextable.
  tps <- unique(df$type)
  # Get lowest level in df
  tps_set <- c("root","summary","level_1","level_2","level_3","level_4","other")
  tt <- FALSE
  uu <- 0
  while(tt==FALSE & uu<=7){
    tt <- tps_set[uu+1] %in% tps
    uu <- uu + 1 # find level of lowest entry
  }

  # Identify which rows are which. Create a dataframe lookup table.
  lookupFrame <- data.frame(type=NULL,rowNum=NULL,level=NULL)
  for(u in 1:nrow(df)){
   level <- match(df[u,"type"],tps_set)
   
      # SL1 If misreading a header, option to push it back relatively
      if(summaryLevels<0){
        level <- level+1
        }
   
   lF <- data.frame(type=df[u,"type"],rowNum=u,level=level)
   lookupFrame <- rbind(lookupFrame,lF)
  }

  ## Set header rows by getting individual rows.
    if(uu==1){
      hRow <- (filter(lookupFrame,level==uu)$rowNum)
      hhRow <- (filter(lookupFrame,level==uu+1)$rowNum)
      hhhRow <- (filter(lookupFrame,level==uu+2)$rowNum)
      hhhhRow <- (filter(lookupFrame,level==uu+3)$rowNum)
      hhhhhRow <- (filter(lookupFrame,level==uu+4)$rowNum)
    }else{
      # What if it's just summary? Leave nonbold.
      # if(length(tps)==1){
      #   hRow <- 0
      #   hhRow <- 0
      #   hhhRow <- 0
      #   hhhhRow <- 0
      #   hhhhhRow <- 0
      # }else{
        hRow <- (filter(lookupFrame,level==uu)$rowNum)
        hhRow <- (filter(lookupFrame,level==uu+1)$rowNum)
        hhhRow <- (filter(lookupFrame,level==uu+2)$rowNum)
        hhhhRow <- (filter(lookupFrame,level==uu+3)$rowNum)
        hhhhhRow <- (filter(lookupFrame,level==uu+4)$rowNum)
      # }
    }

  # SL2 If misreading a header, option to push it back relatively; reset summaryLevels
  if(summaryLevels<0){
    summaryLevels <- abs(summaryLevels)
    }

## Comparison data ---------------------------------------------------------

  data_type <- case_when(
    mode=="summarize"~4,
    mode=="summarizeinc"~3,
    mode=="nosummary"~2,
    mode=="simple"~1,
    .default = 0
  )
  ## Functionality to create comparison columns for us/state.
  if(!is.null(usCompare)){
    if(!(tableID %in% usCompare$table_id)){
      stop("That tableID is not in the supplied usCompare data object. Remake usCompare with variables included.")
    }
    if(verbose==TRUE)message(paste(dur(st),"Building US comparison data..."))
    df <- comparison_helper(df = df,
                            comparisonDF = usCompare,
                            comp_type = "us",
                            tableID = tableID,
                            verbose=verbose)
   # df$us_comp <- formattable::percent(df$us_comp,digits=0)
  }

  if(!is.null(stateCompare)){
    if(!(tableID %in% stateCompare$table_id)){
      stop("That tableID is not in the supplied usCompare data object. Remake usCompare with variables included.")
    }
    if(verbose==TRUE)message(paste(dur(st),"Building state comparison data..."))

    if(!is.null(state)){
      sf <- state
    }else if("info" %in% names(df)){
      if("states" %in% names(df$info)){ 
        sf <- df$info$states
      }else{
        sf <- 99999
      }
    }else if(!is.null(info)){
      sf <- info$states
    }else if(nrow(unique(df %>% ungroup() %>% select(state)))==1){
      sf <- unique(df %>% ungroup() %>% select(state))
    }else{
      sf <- 99999
    }
    
    df <- comparison_helper(df = df,
                            comparisonDF = stateCompare,
                            comp_type = "state",
                            tableID = tableID,
                            stateFilter = sf,
                            verbose=verbose)

    #df$state_comp <- formattable::percent(df$state_comp,digits=0)
  }
 
  # Mutate table to include sub-summary percentage.
  #TODO Check this—is it necessary?
  # if(length(hRow)>1){
  #   hRowCounter <- 1
  #   for(v in 1:nrow(df)){
  #     if(v==hRow[hRowCounter]){
  #       df[v,"pct"] <- formattable::percent(df[v,"estimate"]/df[hRow[hRowCounter],"estimate"],digits=0)
  #       if(!is.null(usCompare)){df[v,"us_pct"] <- formattable::percent(df[v,"us_est"]/df[hRow[hRowCounter],"us_est"],digits=0)}
  #       if(!is.null(stateCompare)){df[v,"state_pct"] <- formattable::percent(df[v,"state_est"]/df[hRow[hRowCounter],"state_est"],digits=0)}
  #     }else{
  #       df[v,"pct"] <- formattable::percent(df[v,"estimate"]/df[hRow[hRowCounter],"estimate"],digits=0)
  #       if(!is.null(usCompare)){df[v,"us_pct"] <- formattable::percent(df[v,"us_est"]/df[hRow[hRowCounter],"us_est"],digits=0)}
  #       if(!is.null(stateCompare)){df[v,"state_pct"] <- formattable::percent(df[v,"state_est"]/df[hRow[hRowCounter],"state_est"],digits=0)}
  #       if(hRowCounter<length(hRow) & (v+1)==(hRow[hRowCounter+1])){
  #         hRowCounter <- hRowCounter+1
  #       }
  #     }
  #   }
  # }

  
  if(!is.null(usCompare)){
    df$us_pct <- formattable::percent(df$us_pct,digits=0)
  }
  if(!is.null(stateCompare)){
    df$state_pct <- formattable::percent(df$state_pct,digits=0)
  }  
  
  ## Update moe to include pretty display of +/-
  if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
    df <- df %>% mutate(moe=paste("+/-",moe))
  }
  
## Column selection --------------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Selecting columns..."))
  
  ## Select appropriate columns based on inputs.
  if(length(cols)==1){
    cols <- names(df)
    colOption <- 1
  }else{
    if("subtotals" %in% names(df)){
      cols <- cols
      #colName <- c("Label","Est. (n)","Est. (%)", "SubGroup %")
      colName <- c("Label","Est. (n)","Est. (%)")
      colOption <- 2
      if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
        cols <- c(cols,"moe")
        colName <- c(colName,"Margin of Error")
        colOption <- colOption+1
      }
      if(groupPerc==TRUE){
        cols <- c(cols,"subtotal_by_type","pct_by_type")
        colName <- c(colName,"Group Subtot.","Group %")
        colOption <- colOption+1
      }
    }else if(dispPerc==FALSE){
      cols <- c("labels","estimate")
      colName <- c("Variable","Est. (n)")
      colOption <- 3
      if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
        cols <- c(cols,"moe")
        colName <- c(colName,"Margin of Error")
        colOption <- colOption+1
      }
      if(groupPerc==TRUE){
        cols <- c(cols,"subtotal_by_type","pct_by_type")
        colName <- c(colName,"Group Subtot.","Group %")
        colOption <- colOption+1
      }
    }else if(mode=="bygeo"){
      if("median" %in% df$calculation){
        cols <- c("name","labels","estimate")
        colName <- c("Geography Name","Variable","Est. (n)") 
        colOption <- 4
        if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
          cols <- c(cols,"moe")
          colName <- c(colName,"Margin of Error")
          colOption <- colOption+1
        }
        if(groupPerc==TRUE){
          cols <- c(cols,"subtotal_by_type","pct_by_type")
          colName <- c(colName,"Group Subtot.","Group %")
          colOption <- colOption+1
        }
      }else{
        cols <- c("name","labels","estimate","pct")
        colName <- c("Geography Name","Variable","Est. (n)","Est. (%)") 
        colOption <- 5
        if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
          cols <- c(cols,"moe")
          colName <- c(colName,"Margin of Error")
          colOption <- colOption+1
        }
        if(groupPerc==TRUE){
          cols <- c(cols,"subtotal_by_type","pct_by_type")
          colName <- c(colName,"Group Subtot.","Group %")
          colOption <- colOption+1
        }
      }
    }else{
      cols <- c("labels","estimate","pct")
      colName <- c("Variable","Est. (n)","Est. (%)")
      colOption <- 6
      if(moe==TRUE & mode %in% c("simple","nosummary","bygeo")){
        cols <- c(cols,"moe")
        colName <- c(colName,"Margin of Error")
        colOption <- colOption+1
      }
      if(groupPerc==TRUE){
        cols <- c(cols,"subtotal_by_type","pct_by_type")
        colName <- c(colName,"Group Subtot.","Group %")
        colOption <- colOption+1
      }
    }
    if(!is.null(usCompare)){
      # cols <- c(cols,"us_sub_pct","us_comp_diff")
      # colName <- c(colName,"Tot. US %","Diff.")
      # cols <- c(cols,"us_sub_pct")
      cols <- c(cols,"us_pct")
      colName <- c(colName,"Tot. US %")
    }
    if(!is.null(stateCompare)){
      # cols <- c(cols,"state_sub_pct","state_comp_diff")
      # colName <- c(colName,paste("Tot.",stNAME,"%"),"Diff.")
      cols <- c(cols,"state_pct")
      colName <- c(colName,"Tot State %")
    }
  }
  if(length(cols)!=length(colName)){
    stop(paste("!> col and colName do not match in length.\n col=",
               paste(unlist(cols),collapse=","),
               "\n colName=",
               paste(unlist(colName),collapse=","),
               "\n colOption = ",
               paste(unlist(colOption),collapse=","),
               sep=""))
  }
  if(verbose==TRUE)message(paste(dur(st),"Building colnames. Including: [",paste(unlist(colName),collapse=","),"]..."))
  
# Table creation ----------------------------------------------------------
  if(verbose==TRUE)message(paste(dur(st),"Building table..."))
  
  # Get table title
  table_type <- case_when(
    mode %in% c("summarize","summarizeinc") ~ "(aggregate)",
    mode == "bygeo" ~ "(by geographical unit)",
    .default = ""
  )
  table_title <- paste(as.character(unique(df$concept)),table_type)
  
  ## Create the table.
   if(mode=="bygeo"){
     x <- as_grouped_data(df,groups = 'name',columns=cols)
     x <- as_flextable(x,col_keys = cols)
   }else{
     x <- flextable(df,col_keys = cols)
   }
   x <- theme_censusprofiler(x)
   x <- set_header_labels(x,values=unlist(colName))
   x <- set_caption(x,
                    as_paragraph(
                      as_chunk(table_title,props = fp_text_default(font.family = "Open Sans"))),
                    word_stylename="Table Caption")
   #x <- add_header_row(x, values = unique(df$concept),colwidths = length(cols))
   #x <- width(x,j="labels",width=1.5)
   if(verbose==TRUE)message(paste(dur(st),"Adding row type coloring..."))

   header <- 0
   if(length(hRow)==1){
     # create header row if just one.
     x <- bg(x,i=hRow,bg="#ccd5dd") 
     x <- italic(x,i=hRow,italic=TRUE,part="body")
     x <- bold(x,i=hRow,bold=TRUE,part="body")
     header <- 1
   }
   if(length(hRow)>1 & (length(hRow)<length(hhRow))){
     x <- bg(x,i=hRow,bg="#ccd5dd") 
     ## If multiple, create bold-italics. Also, make sure not more hRow than hhRow.
     x <- italic(x,i=hRow,italic=TRUE,part="body")
     x <- bold(x,i=hRow,bold=TRUE,part="body")
     header <- 1
   }
   if(length(hhRow)>0 && header==1){
     # Leave 2nd string normal if middle.
   }
   if(length(hhRow)>0 && header==0 && length(hhhRow)==0){
     # If hhRow is subvariable, and multiple primary variables exist (no 
     # header variable), then italicize the subs.
     x <- italic(x,i=hhRow,italic=TRUE,part="body")
   }
   
   if(length(hhhRow)>0 && header==1){
     x <- italic(x,i=hhhRow,italic=TRUE,part="body")
   }
   if(length(hhhhRow)>0 && header==1){
     x <- color(x, i=hhhhRow,color = "#333333", part="body")
     x <- padding(x, i=hhhhRow, padding.left = 12, part="body")
   }
   if(length(hhhhhRow)>0 && header==1){
     x <- color(x, i=hhhhhRow,color = "#333333", part="body")
     x <- padding(x, i=hhhhhRow, padding.left = 18, part="body")
   }
   
   #x <- add_header_row(x,values=c("Estimates","Comparisons"),colwidths = c(3,2))
   # x <- align(x,align="center",part="header")
   # x <- align(x,align="right",part="body")
   #set_table_properties(x, layout = "autofit")
   
   if(verbose==TRUE)message(paste(dur(st),"Done."))
   
  return(x)

}
