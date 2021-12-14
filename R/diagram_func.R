

#' Upper String
#'
#' @param x a string
#'
#' @return a string
#' @export
#'
#' @examples
#' firstup("example")
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}



selecLabel <- function(df, label) {

  labels <- bind_rows(df %>%  filter(parentType == label)%>%
                        select(parent)%>%
                        distinct(parent)%>%
                        rename(label = parent),
                      df %>%  filter(childrenType == label)%>%
                        select(children)%>%
                        distinct(children)%>%
                        rename(label = children))%>%
    distinct(label)

  return(labels) }


#' Generate a Label Code
#'
#' `codeGenerate()` is a function used to generate a code for each instances
#' used in a graph or subgraph. This code is used to simplify the template
#' syntax.
#'
#' @param df a dataframe containing a column "parent"
#' @param symbol a character string used in the instance code (e.g. 'E' for
#' events)
#'
#' @return data.frame containing a column "code"
#' @family string processing function
#'
#' @keywords string
#'
codeGenerate <- function(df) {
  df <- data.frame(df)
    # Event
  listEvent <- data.frame(selecLabel(df, label = 'event'))

  dfRecode <- listEvent %>%
    mutate(code = paste0("E_",row.names(listEvent)))%>%
    mutate(type = 'event')

  # Occurrence
  listOccu <- data.frame(selecLabel(df, label = 'occurrence'))

  dfRecode <- bind_rows(dfRecode,
                        listOccu %>%
                          mutate(code = paste0("O_",row.names(listOccu)))%>%
                          mutate(type = 'occurrence'))

  # MeasurementEvent
  listMeas <- data.frame(selecLabel(df, label = 'measurementE'))
  dfRecode <- bind_rows(dfRecode,
                        listMeas %>%
                          mutate(code = paste0("ME_",row.names(listMeas)))%>%
                          mutate(type = 'measurementE')
  )

  # MeasurementOcc
  listMeasO <- data.frame(selecLabel(df, label = 'measurementO'))
  dfRecode <- bind_rows(dfRecode,
                        listMeasO %>%
                          mutate(code = paste0("MO_",row.names(listMeasO)))%>%
                          mutate(type = 'measurementO')
  )

  return(dfRecode)
}



#' Create a Subgraph
#'
#' `createSubgraph()` is a function used to transform create subgraph based on
#' DiagrammeR syntax and template. It is used to create one subgraph per group
#' of records according to the Darwin Core scheme (i.e. Event Core, Occurrence
#' and eMoF extensions). It models homogeneous relations (several levels in the
#' same group of records). It returns a character string to insert into
#' a template readable by grViz() function.
#'
#' @param dfRelations a dataframe specific to one group of record (homogeneous),
#' that contains at least one column "parent" and one column "children"
#' @param dfNodeLabel a dataframe that contains the labels of instances (which
#' will be used for visual representation) and a code
#' @param labelSubgraph a character string corresponding to the group of records
#' represented in the subgraph (i.e. Event Core, Occurrence ext. or eMoF ext.)
#' @param numCluster an integer corresponding to the subgraph number. The
#' subgraphs must be numbered from 0 to N in ascending order to avoid errors
#' when using grViz() function.
#' @param style a character string indicating the style of subgraph box border
#' (e.g. bold, solid, dashed)
#'
#' @return a character string containing the subgraphs caracteristics, to insert
#' into a template for grViz() function.
#'
#' @family DiagrammeR function
#'
#' @keywords graph

createSubgraph <- function(df, dfNodeLabel, level, labelSubgraph,
                           numCluster,style=solid) {
  df <- data.frame(df)

  # 1. Code les éléments
  df <- left_join(df, dfNodeLabel%>%
                    rename(parent = label),
                  by = 'parent')%>%
    rename(parentCode = code)

  df <- left_join(df, dfNodeLabel%>%
                    rename(children = label),
                  by = 'children')%>%
    rename(childrenCode = code, color = color.x)

  # 2. Crée les relations d'attributs (intra-level)

  dfRelationsA <- df %>% filter(parentType == level & childrenType == level & relationType == 'attribute')
  color <- unique(dfRelationsA$color)

  if (nrow(dfRelationsA)==0) {
    schemeRelationA <- ""
  }  else {
    dfRelationsA <- dfRelationsA %>%
      mutate(relation =  paste(parentCode,
                               childrenCode, sep = '->'))%>%
      mutate(relation =  paste(relation, sprintf('[color = \'%s\', penwidth =2, arrowhead = None]', color)))
    # mutate(relation =  paste(relation, '[color = DarkOrange, penwidth =2]'))


    schemeRelationA <- paste(dfRelationsA$relation, collapse = " ")}


  # 3. Crée les relations de hiérarchie (intra-level)

  dfRelationsH <- df %>% filter(parentType == level & childrenType == level & relationType == 'level')

  if (nrow(dfRelationsH)==0) {
    schemeRelationH <- ""
  } else {
    dfRelationsH <- dfRelationsH %>%
      mutate(relation =  paste(parentCode,
                               childrenCode, sep = '->'))

    schemeRelationH <- paste(dfRelationsH$relation, collapse = " ")
  }

  # 3. Crée les relations de hiérarchie (extra-level)

  dfRelationsEH <- df %>% filter(parentType != level & childrenType == level & relationType == 'level') %>%
    filter(!children%in%dfRelationsH$parent)
  if (nrow(dfRelationsEH)==0) {
    schemeRelationEH <- ""
  } else {
    schemeRelationEH <- paste(dfRelationsEH$childrenCode, sep = "\n ", collapse = " ")}

  # 4. Crée les relations de hiérarchie
  if (nrow(df[df$parentType == "event" & (df$parentType != "measurementE")& (df$childrenType != "measurementE"),])==1 & level == "event") {
    schemeRelation <- "E_1"
  } else {
  schemeRelation <- paste(schemeRelationH, schemeRelationA, schemeRelationEH) }

  subgraph <- sprintf("subgraph cluster_%d {
       label = \'%s\'
       fontname = Helvetica
       style=%s
       %s
     }", numCluster, labelSubgraph, style, schemeRelation )



  return(subgraph)

}


#' Create a Graph
#'
#' `createGraph()` is a function used to transform create subgraph based on
#' DiagrammeR syntax and template. It is used to create one subgraph per group
#' of records according to the Darwin Core scheme (i.e. Event Core, Occurrence
#' and eMoF extensions). It models homogeneous relations (several levels in the
#' same group of records). It returns a character string to insert into
#' a template readable by grViz() function.
#'
#' @param dfRelations a dataframe containing heterogeneous relations (between
#' two different groups of records, that contains at least one column "parent"
#' and one column "children").
#' @param dfNodeLabel a dataframe that contains the labels of instances (which
#' will be used for visual representation) and a code
#'
#' @return a character string containing the heterogeneous relations in the
#' form parent -> children, to insert into a template for grViz() function.
#'
#' @family DiagrammeR function
#'
#' @keywords graph
#'
createGraph <- function(dfRelations, dfNodeLabel) {
  # colnames(dfRelations) <- c('parent', 'children')
  dfRelations <- data.frame(dfRelations)
  dfNodeLabel <- data.frame(dfNodeLabel)

  # 1. Code les éléments
  dfRelations <- left_join(dfRelations, dfNodeLabel%>%
                             rename(parent = label),
                           by = 'parent')%>%
    rename(parentCode = code)

  dfRelations <- left_join(dfRelations, dfNodeLabel%>%
                             rename(children = label),
                           by = 'children')%>%
    rename(childrenCode = code)

  dfRelations <- dfRelations%>%
    mutate(relation =  paste(parentCode, childrenCode, sep = '->'))

  return(paste(dfRelations$relation, collapse = " ")) }


#' Old versions

createSubgraphOld <- function(dfRelations, dfNodeLabel, labelSubgraph,
                              numCluster,style=solid) {

  # 1. Code les éléments
  dfRelations <- left_join(dfRelations, dfNodeLabel%>%
                             rename(parent = label),
                           by = 'parent')%>%
    rename(parentCode = code)

  dfRelations <- left_join(dfRelations, dfNodeLabel%>%
                             rename(children = label),
                           by = 'children')%>%
    rename(childrenCode = code)

  # 2. Crée les relations de hiérarchie

  if (nrow(dfRelations %>%
           filter(!is.na(children))) ==0) {
    schemeRelation <- dfRelations$parentCode[1]
  } else {
    dfRelations <- dfRelations %>%
      filter(!is.na(children)) %>%
      mutate(relation =  paste(parentCode,
                               childrenCode, sep = '->'))

    schemeRelation <- paste(dfRelations$relation, collapse = " ")}

  subgraph <- sprintf("subgraph cluster_%d {
       label = \'%s\'
       fontname = Helvetica
       style=%s
       %s
     }", numCluster, labelSubgraph, style, schemeRelation )

  return(subgraph) }







f <- function(x1,x2) {
  if (x1 == x2) relation <- "attribute"
  else relation <- "level"
  return(relation)
}

format_df_for_diagram <- function(dtf.info, type_level,mapping) {
  dtf  <- data.frame(dtf.info$fields)
  dtf <- arrange(dtf, level)
 rownames(dtf) <- 1:nrow(dtf)
# dtf$fieldname <- sapply(dtf$fieldname, replaceFieldname, mapping,  USE.NAMES = FALSE)
if (nrow(dtf) > 1 ) {


  df = data.frame("parent" = head(dtf %>% pull(fieldname), -1),
                  "children" = tail(dtf %>% pull(fieldname), -1),
                  "parentLevel" = head(dtf %>% pull(level), -1),
                  "childrenLevel" = tail(dtf %>% pull(level), -1),
                  "parentType" = type_level,
                  "childrenType" = type_level,
                  "box" = type_level)
  }else {
                    df = data.frame("parent" = dtf %>% pull(fieldname),
                                    "children" = dtf %>% pull(fieldname),
                                    "parentLevel" = dtf %>% pull(level),
                                    "childrenLevel" = dtf %>% pull(level),
                                    "parentType" = type_level,
                                    "childrenType" = type_level,
                                    "box" = type_level)

  }

  df<- df %>% rowwise() %>% mutate(relationType = f(parentLevel, childrenLevel))%>%
    arrange(childrenLevel)
  return(df)


}

format_df_for_diagram_2 <- function(dtf.info, type_level, df) {

  nbLevels <- max(dtf.info$fields[["level"]])
  # dfMoFtmp <- data.frame("level"=NA,"measurementType"=NA )
  for (i in 1:nbLevels) {
    if (as.character(i) %in% names(dtf.info$parameters)) {

      paramMof  <- dtf.info$parameters[[as.character(i)]]$Mof$Mof

      if (!is.null(paramMof)) {

        measureType <- ifelse(type_level == "event", "measurementE", "measurementO")

        if (length(df$childrenLevel[df$childrenLevel == i]) == 1) {
          if (type_level == "event" ) {
          parentVal <- df$parent[df$childrenLevel == i]
          } else {
          parentVal <- df$children[df$childrenLevel == i] }
        } else {
          parentVal <- tail(df$children[df$box == type_level & df$childrenLevel == i],1)
        }
        df_to_add = data.frame(
          "parent" = parentVal,
          "children" = paramMof[, c("measurementType")] ,
          "parentLevel" = i,
          "childrenLevel" = 1,
          "parentType" =type_level,
          "childrenType" = measureType,
          "box"   = measureType,
          "relationType"   = "level")

      } else {
        df_to_add <- data.frame()

      }

      df <- rbind(df, df_to_add)

      paramMofMof  <- dtf.info$parameters[[as.character(i)]]$Mof$MofMof

      if (!is.null(paramMofMof)) {
        paramMofMof <- rename(paramMofMof, parent = measurementParent)
        paramMofMof <- rename(paramMofMof, children = measurementType)

        parentLevel <- c()
        for (j in 1:nrow(paramMofMof)) {
          if (paramMofMof$parent[j] %in% df$parent) {
            parentLevel <- c(parentLevel, strtoi(df$parentLevel[df$parent == paramMofMof$parent[j]]))
          } else {
            parentLevel <- c(parentLevel, strtoi(df$childrenLevel[df$children == paramMofMof$parent[j]]))
          }

        }
        childrenLevel <- parentLevel + 1
        parentType <- ifelse(type_level == "event", "measurementE", "measurementO")

        df_to_add <- bind_cols(select(paramMofMof, all_of(c("parent", "children"))),
                               "parentLevel" = parentLevel,
                               "childrenLevel" = childrenLevel,
                               "parentType" = parentType,
                               "childrenType" = parentType,
                               "box"   = parentType,
                               "relationType"   = "level"
        )

      } else {
        df_to_add <- data.frame()

      }

      df <- rbind(df, df_to_add)
    }
  }
  return(df)}

format_df_for_diagram_2_init <- function(dtf.info, type_level, df) {

  nbLevels <- max(dtf.info$fields[["level"]])
 # dfMoFtmp <- data.frame("level"=NA,"measurementType"=NA )
  for (i in 1:nbLevels) {
    if (as.character(i) %in% names(dtf.info$parameters)) {

      paramMof  <- dtf.info$parameters[[as.character(i)]]$Mof$Mof

      if (!is.null(paramMof)) {

        measureType <- ifelse(type_level == "event", "measurementE", "measurementO")

        if (length(df$childrenLevel[df$childrenLevel == i]) == 1) {
          parentVal <- df$parent[df$childrenLevel == i]
        } else {
          parentVal <- tail(df$children[df$box == type_level & df$childrenLevel == i],1)
        }
        df_to_add = data.frame(
          "parent" = parentVal,
          "children" = paramMof[, c("measurementType")] ,
          "parentLevel" = i,
          "childrenLevel" = 1,
          "parentType" =type_level,
          "childrenType" = measureType,
          "box"   = measureType,
          "relationType"   = "level")

      } else {
        df_to_add <- data.frame()

      }

      df <- rbind(df, df_to_add)    } }
  return(df)}

df_for_diagram <- function(dtf.event, dtf.occ, mapping) {

  df_event <- format_df_for_diagram(dtf.event, "event", mapping)
  df_occ <- format_df_for_diagram(dtf.occ, "occurrence", mapping)

  row_to_add = c(tail(df_event %>% pull(children),1),
                 head(df_occ %>% pull(parent),1),
                 strtoi(tail(df_event %>% pull(childrenLevel),1)),
                 strtoi(head(df_occ %>% pull(parentLevel),1)),
                 "event",
                 "occurrence",
                 "occurrence",
                 "level")
  df <- rbind(df_event,
              setNames(row_to_add, names(df_event)),
              df_occ)

  df <- df[df$parent != df$children, ]
  df <- format_df_for_diagram_2(dtf.event, "event", df)
  df <- format_df_for_diagram_2(dtf.occ, "occurrence", df)

  return(df)
}

generate_diagram <- function(df, mapping) {
 df <- data.frame(df)

  dfNodeLabel <- codeGenerate(df)

  dfNodeLabel <- dfNodeLabel %>% mutate(color = ifelse(type == 'event', 'DarkOrange', ifelse(type == 'occurrence', 'OliveDrab3', 'CornFlowerBlue')))
  dfNodeLabel2 <- dfNodeLabel %>% mutate(label = sapply(label, replaceFieldname, mapping,  USE.NAMES = FALSE))
  nodeLabel <-dfNodeLabel2 %>%
    mutate(encode =  sprintf("%s [label = \'%s\', fillcolor = %s] ", code, label, color))

  nodeLabel <- paste(nodeLabel$encode, collapse = " ")

  ################
  ### SUBGRAPH ###
  ################


  # Event
  i <- 1
  labelSubgraph <- 'Event core'
  style <- 'bold'
  numCluster <- i
  i <- i+1
  level = 'event'

  subgraphEvent <- createSubgraph(df, dfNodeLabel, level, labelSubgraph,
                                  numCluster,style)

  # Occurrence

  labelSubgraph <- 'Occurrence extension'
  style <- 'solid'
  numCluster <- i
  i <- i+1
  level = 'occurrence'
  subgraphOccu <- createSubgraph(df, dfNodeLabel, level, labelSubgraph,
                                 numCluster,style)



  # Measurements Event

  labelSubgraph <- 'Event eMo extension'
  style <- 'dashed'
  numCluster <- i
  i <- i+1
  level = 'measurementE'
  df_sub <- df[df$box == 'measurementE',]
  subgraphMeasurement <- createSubgraph(df_sub, dfNodeLabel, level, labelSubgraph,
                                        numCluster,style)


  # Measurements Occurrence

  labelSubgraph <- 'Occurrence eMoF extension'
  style <- 'dashed'
  numCluster <- i
  i <- i+1
  level = 'measurementO'
  df_sub <- df[df$box == 'measurementO',]
  subgraphMeasurementO <- createSubgraph(df_sub, dfNodeLabel, level, labelSubgraph,
                                         numCluster,style)

  #########
  # GRAPH #
  #########
  d <- df[df$parentType != df$childrenType, ]
  allGraph <- createGraph(d, dfNodeLabel)
  return(list("nodeLabel" =nodeLabel ,
"subgraphEvent" = subgraphEvent,
"subgraphOccu" = subgraphOccu,
"subgraphMeasurement" = subgraphMeasurement,
"subgraphMeasurementO" = subgraphMeasurementO,
              "allGraph" = allGraph
  ))}

