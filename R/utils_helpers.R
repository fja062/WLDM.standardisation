#' @title listReactiveValues
#' @description Allows to turn a `reactiveValues`` object into a non-reactive list. Recursive method. Written by Elie
#'
#' @param rv reactiveValues. Target to turn into list.
#' @param lv internal. Verbose purposes.
#' @param name internal. Current root node name
#'
#' @import shiny
#' 
#' @export
listReactiveValues <- function(rv, lv = 0, name = "root") {
  if (missing(rv)) {
    stop("No reactiveValues provided")
  }
  if (!is.reactivevalues(rv)) {
    message("Provided `rv` is not a reactiveValues")
    return(rv)
  }
  
  # Check for children
  children <- isolate(reactiveValuesToList(rv))
  n <- names(children)
  out <- lapply(seq_along(children), function(sub) {
    subrv <- children[[sub]]
    
    if (is.reactivevalues(subrv)) {
      listReactiveValues(subrv, lv + 1, name = n[sub])
    }
    else if (is.reactive(subrv)) {
      return(isolate(subrv()))
    }
    else {
      return(subrv)
    }
  })
  names(out) <- n
  
  return(out)
}



#' @title saveVar
#' @description Save a dataframe in reactiveValues form into a json format. Written by Elie
#'
#' @param data Reactive dataframe 
#' @param fname File name
#'
#' @import shiny
#' 
#' @export
saveVar <- function(data, fname){
  # Cas d'une app shiny: on peut y passer une reactiveValues
  if(is.reactivevalues(data))
    data <- listReactiveValues(data) # transforme `data` en liste
  
  jsonlite:: write_json(jsonlite::serializeJSON(data), fname)
  
}



#' @title Split.to.Tree
#' @description Transform a dataframe into a list of list, as required for the first step to shape a Tree. Recursive function. Deprecated, use df2list
#'
#' @param dtf dataframe with one or several dimensions. The last column of dtf must be the branch number
#'
#' @import shiny
#' 
#' @export
Split.to.Tree <- function(dtf){
  
  dtf <- data.frame(dtf)
  
  if(dim(dtf)[2] > 1){
    
    splitted_dtf <- split(dtf[,-1], dtf[,1])
    
    splitted_dtf <- lapply(splitted_dtf,
                           function(x) {
                             if(data.frame(x)[1,1] != "") {y <- Split.to.Tree(x)}
                             else{y <- x$nbr}
                             return(y)
                           })
  }
  
  else{splitted_dtf <- dtf$dtf}
  
  return(splitted_dtf)
}



#' @title Structure.to.Tree 
#' @description Transform a list from Split.to.Tree() to the shape required by shinyTree(). Recursive function. Deprecated used df2list
#'
#' @param dtf list of (list of) dataframes. dtf must be the output of the function Split.to.Tree()
#'
#' @import shiny
#' 
#' @export
Structure.to.Tree <- function(dtf){
  
  if(is.list(dtf)) {structured_dtf <- lapply(dtf, function(x) Structure.to.Tree(x))}
  
  else(structured_dtf <- structure(dtf, stselected=F))
  
}


#' @title df2list
#' @description Transform a dataframe to the shape required by shinyTree().
#'
#' @param dtf a dataframe. Each column represent a hierarchical level of the tree to be built
#' @param title name of the first item, closed. Optionnal, if it is not used, the first items of the tree will be shown
#'
#' @import shiny
#' 
#' @export


df2list<-function(df,title=NA){

  if(df[,1]%>%unique()%>%length()==1){
    df<-df[,-1]}
  
  df<-cbind(Dim_fack="1",Dim_Title=title,df)
 
  if(is.na(title)){
    df<-df[,-2]}
  
  cols<-1:dim(df)[2]
  #names(df)<-c(paste("Dim",cols,sep=""))
  df$pathString<-apply(df[,1:dim(df)[2]],1,paste,collapse="/")
  df$pathString<-sub('/$',"",df$pathString)
 
  
  df<-df %>% 
    dplyr::select(pathString)
  df_po<-data.tree::as.Node(df)
  df_l<-as.list(df_po)
  
  df_l[[1]]<-NULL
  
  return(df_l)}
