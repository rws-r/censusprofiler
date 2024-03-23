
#' Profile Helper
#'
#' A function designed to interactively create a vectorized list of selected
#' variables to use for profiler() functionality. 
#'
#' @param year Year of call.
#' @param tableID Vector with variables for inclusion.
#' should be accessed.
#' @param allCols Parameter to display all columns during review of variables 
#' for selection.
#' @param test Internal: logical parameter to specificy testing environment.
#'
#' @return Vector with variable values stored.
#' @export
#'
#' @examples
#' \dontrun{
#' profile_builder(tableID=tableID,addNumbering=TRUE)
#' }
profile_helper <- function(tableID=NULL,
                           year=NULL,
                           allCols=FALSE,
                           test=FALSE){
  
  ## Deal with "no visible binding for global variable" error 
  censusDataset <- table_id <- name <- label <- 
    type <- NULL

  if(is.null(year)){
    stop("You need to include a year.")
  }

  ACS <- acs_vars_builder(year,dataset=censusDataset)
  ACS.VARS <- ACS[[1]]
  ACS.GROUPS <- ACS[[2]]

# if(addNumbering==TRUE){
  # if(exists("profile_variables")){
  #   message("It looks like you already have a profile_variables object created. \n Should we build on this? (yes) \n If (no), we'll start over. ")
  #   ifelse(test==FALSE,
  #          user_input <- readline(),
  #          user_input <- "no")
  #   if(user_input=="yes"){
  #     profileVarsDF <- ACS.VARS %>% filter(table_id %in% tableID)
  #     vbpv <- unique(as.vector(unlist(ACS.VARS %>% filter(name %in% profile_variables) %>% select(table_id))))
  #     profileVarsDF <- profileVarsDF %>% filter(!(table_id %in% vbpv))
  #     profileVariableList <- profileVarsDF
  #     tableID <- tableID[!(tableID %in% vbpv)]
  #    }else{
  #     profileVarsDF <- ACS.VARS %>% filter(table_id %in% tableID)
  #     profileVariableList <- NULL
  #    }
  # }else{
    profileVarsDF <- ACS.VARS %>% filter(table_id %in% tableID)
    profileVariableList <- NULL
  # }
    for(i in 1:length(tableID)){
      vb <- tableID[i]
      dispVars <- profileVarsDF %>% filter(table_id==vb)
      if(test==FALSE)message(cat("-----------\n table_id for review: ",unlist(unique(dispVars$concept)),"\n-----------\n"))
      # Select columns; option to display all cols.
      if(allCols==FALSE){
        dispVars <- dispVars %>% select(name,label,type)
      }
      if(test==FALSE)print(dispVars,n=100)
      ifelse(test==FALSE,
         user_input <- readline("Which numbers do we want to keep? (`stop`,`all`,`type`,number array):"),
         user_input <- "all")
      if(user_input != 'stop'){
       if(user_input=="type"){
        user_input_b <- readline("Which type should we filter by? (array accepted)")
        user_input_b <- unlist(strsplit(user_input_b,","))
        # Check inputs
        for(x in 1:length(user_input_b)){
          if(user_input_b[x] %in% c("root","summary","level_1","level_2","level_3","level_4","other")==FALSE){
            message(paste("Whoops, looks like ",user_input_b[x],"doesn't exist. Please start over and try again."))
            #saveMe(test=test)
            stop()
            }
           }
        ui <- user_input_b
        vC <- dispVars %>% filter(type %in% ui)
        vC <- vC[["name"]]
        profileVariableList <- c(profileVariableList,vC)
       

       }else{
        if(user_input=="all"){
          ui <- c(1:nrow(dispVars))
         }else{
          ui <- eval(parse(text=paste("c(",user_input,")",sep="")))
         }
          vl <- NULL
          for(v in 1:length(ui)){
           vC <- paste(vb,"_",sprintf("%03d",ui[v]),sep="")
           profileVariableList <- c(profileVariableList,vC)
          }
         }

      }else{
        #saveMe(profileVariableList,test=test)
        return(profileVariableList)
      }
      if(test==FALSE)message(cat(">> Variables added: ",profileVariableList))
    }
  ifelse(test = FALSE,
         return(profileVariableList),
         #checkSave(profileVariableList,test=test),
         return(profileVariableList))

  # }else if(review==TRUE){
  #   message("Parameter 'review' is unimplemented as of now.")
  #   # #TODO Fix this.
  #   # for(i in 1:length(tableID)){
  #   #   print(capi("state",table_id=tableID[i],varStartNum = 1, year=2021,state="IL",
  #   #                          plot = TRUE))
  #   # 
  #   #   user_input <- readline("Should I continue? (y/n)  ")
  #   #   if(user_input != 'y') stop('Exiting since you did not press y')
  #   # }
  # }else{
  # stop("You did not specify any arguments.")
  # }
}
