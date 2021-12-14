

#' Clean column names
#' Used in func:create_mapping
#' @param oldName a string
#'
#' @return a string
#' @export
#'
clean_names <- function(oldName) {
  newName <- janitor::make_clean_names(oldName)
  newName <- gsub('_', '', newName)
  newName <- gsub('\\d', '', newName)
  return(newName)
}

#' Map fieldnames to Darwin core names
#'`create_mapping()` is a function create the dictionnary to map columns names
#' from the user dataset to standard from Darwin Core
#' Used in module: mod_Import
#'
#' @param dico_init a data.frame containing standard names
#' @param list_user a vector of strings, containing fieldnames
#'
#' @return
#' @export
#'
create_mapping <- function(dico_init, list_user) {
  if (nrow(dico_init) == 0) {
    stop("Darwinizer dictionnary empty.")

  }  else if (length(intersect(colnames(dico_init), c("standard", "variant", "concept"))) < 3) {
    stop(
      "Darwinizer dictionnary should contain the columns \"standard\",
           \"variant\" and \"concept\"."
    )

  } else {
    # Creation of temporary names (lowercased, without special characters)

    dico_init <- dico_init %>% mutate(tmpStandard =
                                        clean_names(standard),
                                      tmpVariant =
                                        clean_names(variant)) %>%
      drop_na(standard)

    data_user_names <-
      data.frame('fieldname' = list_user,
                 'tmpFieldname' = clean_names(list_user))

    #~~~~~~~~~~~~~~~~~#
    # Perfect matches #
    #~~~~~~~~~~~~~~~~~#
    data_mapped <- merge(
      data_user_names,
      select(dico_init, standard, tmpStandard, concept),
      by.x = "tmpFieldname",
      by.y = "tmpStandard"
    ) %>% distinct()

    #~~~~~~~~~~~~~~~~~~~~~#
    # Matches by variants #
    #~~~~~~~~~~~~~~~~~~~~~#
    if (nrow(data_mapped) > 0) {
      data_user_names_2 <-
        filter(data_user_names,!fieldname %in% data_mapped$fieldname)

    } else {
      data_user_names_2 <- data_user_names
    }

    data_mapped_2 <- merge(
      data_user_names_2,
      select(dico_init, standard, tmpVariant, concept),
      by.x = "tmpFieldname",
      by.y = "tmpVariant"

    )
    data_mapped <- bind_rows(data_mapped, data_mapped_2)

    return(data_mapped)
  }
}

#' Tidy Data.Frame
#'
#' `tidy_df()` is a function used to transform a data.frame into its tidy
#' version (one variable per column)
#'
#' Used in func: tidy_data
#' @param df an untidy data.frame
#' @param untidy_col a vector of column names that are not tidy (e.g. "wild boars")
#' @param new_col a string indicating the new column names corresponding to the
#' untidy columns (e.g. "species")
#' @param new_value a string indicating the name of the value corresponding to the
#' untidy columns (e.g. "individualCount")
#'
#' @return a tidy data.frame of data, with new column names
#'
#' @family pre-processing function
#'
rename_base <- function(df, old_colname, new_colname) {
  colnames(df)[colnames(df) == old_colname] <-
    new_colname
  return(df)
}

tidy_df <- function(df, untidy_col, new_col, new_value) {
  df <- df %>% gather(oldCol, oldValue, all_of(untidy_col)) %>%
    rename((!!new_col) := oldCol) %>%
    rename((!!new_value) := oldValue)

  return(df)
}

#' Tidy User Data.Frame
#' Used in module: mod_Import
#' @param df a user data.frame to tidy
#' @param dfTidy a data.frame containing the information to tidy
#'
#' @return a tiy data.frame
#' @export
#'
tidy_data <- function(df, dfTidy) {
  if (length(unique(dfTidy[['New_column']])) + length(unique(dfTidy[['New_value']])) == 2) {
    finalDF <-
      tidy_df(df, dfTidy[['Untidy_column']], unique(dfTidy[['New_column']]), unique(dfTidy[['New_value']]))

  } else {
    for (uniqueNew_value in unique(dfTidy[["New_value"]])) {
      for (uniqueNew_column in unique(pull(filter(
        dfTidy, New_value == uniqueNew_value
      ), New_column))) {
        columnsUntidy <-
          pull(filter(
            dfTidy,
            (
              New_value == uniqueNew_value & New_column == uniqueNew_column
            )
          ),
          Untidy_column)

        tmpDF <-
          df %>% select(-all_of(setdiff(
            pull(dfTidy, Untidy_column), columnsUntidy
          )))
        tmpDF <-
          tidy_df(tmpDF,
                  columnsUntidy,
                  uniqueNew_column,
                  uniqueNew_value)

        if (exists("finalDF")) {
          finalDF <- smartbind(finalDF, tmpDF)


        } else {
          finalDF <- tmpDF
        }
      }
    }
  }

  return(finalDF)

}

#' Replace Fieldname
#' Used in func: rename_df, make_temporal, createLongFileEvent,
#' createLongFileOcc
#' @param name a string (fieldname)
#' @param mapping a data.frame (to map the fieldname with a standard name)
#'
#' @return a string (darwinized fieldname)
#' @export
#'
concat <- function(df, col1, col2, sep) {
  new_col <- apply( df[ , c(col1,col2)] , 1 , paste , collapse = sep )
  df[col1] <- new_col
  return(df)
}

substr_col <- function(df, col1, start, stop) {
  new_col <- apply( data.frame(df[ , col1]) , 1 , substr , start, stop)
  df[col1] <- new_col
  return(df)
}


split_col <- function(df, col, col2, sep) {

  if (!col2%in%names(df)) {

    if (sep == ".") {

      sep <- "[.]" }

    df_sub <- df %>% separate(col, c(col, col2), sep= sep, fill = "right")

    if (sum(is.na(df_sub[col2])) == nrow(df_sub)) {
      # if one column is empty (no separation), do not modify the data.frame
      return(df)
    } else {
      return(df_sub)
    }
  }

}



replaceFieldname <- function(name, mapping) {
  if (!name %in% mapping[["fieldname"]]) {
    return(name)
  } else {
    standard <- pull(filter(mapping, fieldname == name), standard)
    if (length(standard) == 0 | is.na(unique(standard))) {
      return(name)
    } else {
      return(standard)
    }
  }
}

#' Transform Dates into ISO 8601 format, either datetime or interval
#'
#' `dateIso()` is a function used to
#' Used in func: make_temporal
#' @param d1 a character string corresponding to the beginning date of an
#' interval. Must be be "YYYY", "YYYY-MM", or "YYYY-MM-DD".
#' @param d2 an optional character string corresponding to the ending date of an
#' interval. Must be be "YYYY", "YYYY-MM", or "YYYY-MM-DD".
#' @param simplify logical indicating if iso8601 format should be "YYYY-MM-DD"
#' even if the month and day are not provided (set to "01" by default)
#' @param format_date a character vector of date-time formats
#' @param sep_interval a character, indicating the interval between begin and end date

#' @return a character object formatted according to ISO 8601
#'
#' @family standardization function
#'
#' @keywords date
#'

dateIso <-
  function(d1,
           d2 = NA,
           simplify = TRUE,
           format_date = NA,
           sep_interval = NA) {
    d1 <- as.character(d1)

    if (!is.na(sep_interval)) {
      d <- d1
      d1 <- str_split(d, sep_interval)[[1]][1]
      d2 <- str_split(d, sep_interval)[[1]][2]

      if (!is.na(format_date)) {
        d1 <- parse_date_time(d1, orders = format_date)
        d2 <- parse_date_time(d2, orders = format_date)

        if (!str_detect(format_date, 'd')) {
          if (!str_detect(format_date, 'm'))  {
            d1 <- substr(d1, 1, 4)
            d2 <- substr(d2, 1, 4)
          } else {
            d1 <- substr(d1, 1, 7)
            d2 <- substr(d2, 1, 7)
          }
        }
      }

    } else {
      if (!is.na(format_date)) {
        d1 <- parse_date_time(d1, orders = format_date)

        if (!str_detect(format_date, 'd')) {
          if (!str_detect(format_date, 'm')) {
            d1 <- substr(d1, 1, 4)
          } else {
            d1 <- substr(d1, 1, 7)
          }
        }

      }
    }

    d1 <- gsub("/", "-", d1)
    d2 <- gsub("/", "-", d2)
    # If punctual datetime
    if (is.na(d2)) {
      if (simplify) {
        return(d1)
      } else {
        return(iso8601(anydate(d1)))
      }

    } else {
      # If interval
      if (simplify) {
        return(paste(d1, d2, sep = "/"))
      } else {
        return(paste(iso8601(anydate(d1)), iso8601(anydate(d2)), sep = "/"))
      }


    }
  }



#' Create temporal data.frame
#'
#' @param df a data.frame (user)
#' @param df_mapped a data.frame (darwinizer mapping)
#' @param type a string (controlled list of values indicating the type of conversion)
#' @param format_date a character vector of date-time formats
#' @param sep_interval a character, indicating the interval between begin and end date
#' @param values list of two dates
#' @param tf_dic #not functional
#'
#' @return
#' @export
#'

make_temporal <- function(df,
                          df_mapped,
                          type,
                          format_date = NA,
                          sep_interval = NA,
                          values = NA,
                          tf_dic = NA) {
  colnames(df) <- sapply(colnames(df),
                         replaceFieldname,  df_mapped, USE.NAMES = FALSE)

  if (type == "1_col_auto") {
    if (!'dataTime' %in% colnames(df))
      df[['dataTime']] <- NA


    res <- tryCatch(
      dateIso(
        d1 = df[["dataTime"]][1],
        d2 = NA,
        simplify = TRUE,
        format_date = format_date,
        sep_interval = sep_interval
      ),
      error = function(e)
        e,
      warning = function(w)
        w
    )

    if (!is(res, "warning")) {
      df <-
        df %>% rowwise() %>% mutate(
          eventDate = dateIso(
            d1 = dataTime,
            d2 = NA,
            simplify = TRUE,
            format_date = format_date,
            sep_interval = sep_interval
          )
        )

      df <-
        df %>% rowwise() %>% mutate(beginDate = sapply(sapply(eventDate, str_split, "/", USE.NAMES = FALSE),
                                                       function(x)
                                                         x[1], USE.NAMES = FALSE))

      df <-
        df %>% rowwise() %>% mutate(endDate = sapply(sapply(eventDate, str_split, "/", USE.NAMES = FALSE),
                                                     function(x)
                                                       x[2], USE.NAMES = FALSE))
    }


  } else if (type == "manu") {
    df["beginDate"] <- values[1]
    df["endDate"] <- values[2]

  } else if (type == "2_col_auto") {
    if (!'beginDate' %in% colnames(df)) {
      df <- df %>% mutate(beginDate = NA)
    } else {
      df <- df %>% rowwise() %>%
        mutate(
          beginDate = dateIso(
            beginDate,
            d2 = NA,
            simplify = TRUE,
            format_date = format_date,
            sep_interval = NA
          )
        )

    }
    if (!'endDate' %in% colnames(df)) {
      df <- df %>% mutate(endDate = NA)
    } else {
      df <- df %>% rowwise() %>%
        mutate(
          endDate = dateIso(
            endDate,
            d2 = NA,
            simplify = TRUE,
            format_date = format_date,
            sep_interval = NA
          )
        )
    }

  }

  if (!'beginDate' %in% colnames(df)) {
    df <- df %>% mutate(beginDate = NA)
  }

  if (!'endDate' %in% colnames(df)) {
    df <- df %>% mutate(endDate = NA)
  }

  if (!'eventDate' %in% colnames(df)) {
    df <- df %>% rowwise() %>%
      mutate(
        eventDate = dateIso(
          d1 = beginDate,
          d2 = endDate,
          simplify = TRUE,
          format_date = NA,
          sep_interval = sep_interval
        )
      )

  }


  return(df)

}



#' Rename Columns based on Prefix
#' Used in func: createMoF
#' @param df a data.frame to rename
#' @param oldpref a string, prefix to select column names
#' @param newpref a string, to replace selected column names
#'
#' @return
#' @export
#'

rename_col_start <- function(df, oldpref, newpref) {
  df %>%
    rename_at(vars(starts_with(oldpref)), ~ paste0(newpref))


}

#' Convert Column values
#' Used in module: mod_Import
#' @param df a data.frame
#' @param userDic a data.frame, used to convert old values in new values
#' in df
#'
#' @return
#' @export
#'

convert_columns <- function(df, userDic) {
  cols <- intersect(colnames(df), unique(userDic[["Column"]]))

  if (length(cols) > 0) {
    for (col in cols) {
      userDicTmp <- userDic[which(userDic$Column == col),]

      userDicTmp <-
        setNames(userDicTmp[, c("Initial_value", "New_value")],
                 c("Initial_value", col))

      df[["Initial_value"]] <- df[[col]]

      nm <-
        unique(df[["Initial_value"]])[!unique(df[["Initial_value"]]) %in% userDicTmp[["Initial_value"]]]

      userDicTmp <- rbind(userDicTmp,
                          setNames(data.frame(matrix(
                            nm, ncol = 2, nrow = length(nm)
                          )), names(userDicTmp)))

      df <- select(df,-all_of(col))
      df <- merge(
        df,
        userDicTmp,
        by.all = "Initial_value",
        all.x = TRUE,
        all.y = FALSE,
        sort = FALSE
      )
      df <- select(df,-Initial_value)
    }
  }
  return(df)

}

#' Create list of Levels
#' Used in func: createAggregatedDF
#' @param dtf.event a list (Event parameters)
#' @param dtf.occurrence a list (Occurrence parameters)
#'
#' @return a list of strings (either "Event" or "Occ")
#' @export
#'

createNameLevels <- function(dtf.event, dtf.occurrence) {
  n1 = 0
  n2 = 0
  if (nrow(dtf.event) > 0)
    n1 <- length(unique(dtf.event[['level']]))
  if (nrow(dtf.occurrence) > 0)
    n2 <- length(unique(dtf.occurrence[['level']]))
  nameLevels <- c(rep("Event", n1), rep("Occ", n2))

  return(nameLevels)
}


#' Create list of Event/Occ fieldnames
#'
#' @param dtf.event a list (Event parameters)
#' @param dtf.occurrence a list (Occurrence parameters)
#'
#' @return a list of vectors, containing Event and Occurrence fieldnames
#' @export
#'

create_group_cols <- function(dtf.event, dtf.occurrence) {
  n1 = c()
  n2 =  c()

  if (nrow(dtf.event) > 0)
    n1 <- sort(unique(dtf.event[['level']]))
  if (nrow(dtf.occurrence) > 0)
    n2 <- sort(unique(dtf.occurrence[['level']]))


  event <- list()
  occurrence <- list()

  if (length(n1) > 0) {
    for (i in 1:length(n1)) {
      event[[i]] <- pull(filter(dtf.event, level == n1[i]), fieldname)

    }

    if (length(n2) > 0) {
      for (j in 1:length(n2)) {
        occurrence[[j]] <-
          pull(filter(dtf.occurrence, level == n2[j]), fieldname)



      }

    }
  }
  group_cols <- c(event, occurrence)
  return(group_cols)
}


#' Create Aggregated Data.Frame
#'
#' @param df a data.frame
#' @param dtf.event a list (Event parameters)
#' @param dtf.occurrence a list (Occurrence parameters)
#'
#' @return a data.frame
#' @export
#'

createAggregatedDF_v1 <- function(df, dtf.event, dtf.occurrence) {
  nameLevels <-
    createNameLevels(dtf.event$fields, dtf.occurrence$fields)
  group_cols <-
    create_group_cols(dtf.event$fields, dtf.occurrence$fields)

  nbLevels <- length(group_cols)
  nameColAll <- c()
  counterLevel <- 0

  for (le in 1:nbLevels) {
    dfTmp <- unique(select(df, all_of(c(
      group_cols[[le]], nameColAll
    ))))

    # Remove NA rows for "individualCount","occurrenceStatus"
    vars <-  c("individualCount", "occurrenceStatus")
    dfTmp <- drop_na(dfTmp , any_of(vars))
    if (le == 1) {
      counterLevel <- counterLevel + 1
    }  else if (nameLevels[le] == nameLevels[le - 1]) {
      counterLevel <- counterLevel + 1
    } else {
      counterLevel <- 1
    }

    if (is.null(nameColAll)) {
      dfTmp[['mergeTmp']] <-
        paste0(substr(nameLevels[le], 1, 1), seq(1, nrow(dfTmp)))

    } else {
      # We extract the number of the parent level (e.g. If parentLevel ID is "Event1-2", we extract "2")
      dfTmp <- data.frame(dfTmp, check.names = F)
      dfTmp <- dfTmp %>%
        mutate(mergeTmp = dfTmp[[tail(nameColAll, 1)]])

      # dfTmp <-dfTmp %>%
      #   mutate(mergeTmp = sapply(sapply(dfTmp[[nameColAll[le-1]]], str_extract_all, "\\w+", USE.NAMES = FALSE),
      #                            function(x) x[2], USE.NAMES = FALSE))

      # We extract the observation number inside the parent Level (if its is the second observation, set to 2)
      dfTmp <- dfTmp %>% group_by(mergeTmp) %>%
        dplyr::mutate(row_nb = dplyr::row_number())

      #init
      #  dfTmp <-dfTmp %>%
      #    mutate(mergeTmp = paste0(nameLevels[le],counterLevel, "-", mergeTmp, "-",row_nb ))

      dfTmp <- dfTmp %>%
        mutate(mergeTmp = paste0(mergeTmp, "-", substr(nameLevels[le], 1, 1), row_nb))
      dfTmp <- select(dfTmp,-all_of("row_nb"))
    }

    # Update df
    df <-
      merge(df,
            dfTmp,
            by.all = c(group_cols[[le]], nameColAll),
            sort = FALSE)
    nameCol <- paste0(nameLevels[le], "-", counterLevel)
    nameColAll <- c(nameColAll, nameCol)
    df <- rename(df, (!!nameCol) := mergeTmp)
  }

  return(df)
}

createAggregatedDF <- function(df, dtf.event, dtf.occurrence) {
  nameLevels <-
    createNameLevels(dtf.event$fields, dtf.occurrence$fields)
  group_cols <-
    create_group_cols(dtf.event$fields, dtf.occurrence$fields)

  nbLevels <- length(group_cols)
  nameColAll <- c()
  counterLevel <- 0

  for (le in 1:length(group_cols)) {

    dfTmp <- unique(select(df, all_of(c(
      group_cols[[le]], nameColAll
    ))))

    # Remove NA rows for "individualCount","occurrenceStatus"
    vars_sel <-  intersect(names(dfTmp),
                           c("individualCount", "occurrenceStatus"))

    if (length(vars_sel) > 0) {dfTmp <- drop_na(dfTmp , any_of(vars_sel)) }

    if (le == 1) {
      counterLevel <- counterLevel + 1
    }  else if (nameLevels[le] == nameLevels[le - 1]) {
      counterLevel <- counterLevel + 1
    } else {
      counterLevel <- 1
    }

    if (is.null(nameColAll)) {

      dfTmp[['mergeTmp']] <-
        paste0(nameLevels[le], le, "-", seq(1, nrow(dfTmp)))


    } else {
      # We extract the number of the parent level (e.g. If parentLevel ID is "Event1-2", we extract "2")
      dfTmp <- dfTmp %>%
        mutate(mergeTmp = str_replace(dfTmp[[nameColAll[le - 1]]],
                                      paste0(
                                        str_replace(nameColAll[le - 1], "-", ""), "-"
                                      ), ""))

      dfTmp <- dfTmp %>%
        mutate(mergeTmp = str_replace(dfTmp[[nameColAll[le - 1]]],
                                      paste0(
                                        str_replace(nameColAll[le - 1], "-", ""), "-"
                                      ), ""))
      # dfTmp <-dfTmp %>%
      #   mutate(mergeTmp = sapply(sapply(dfTmp[[nameColAll[le-1]]], str_extract_all, "\\w+", USE.NAMES = FALSE),
      #                            function(x) x[2], USE.NAMES = FALSE))

      # We extract the observation number inside the parent Level (if its is the second observation, set to 2)
      dfTmp <- dfTmp %>% group_by(mergeTmp) %>%
        dplyr::mutate(row_nb = dplyr::row_number())


      dfTmp <- dfTmp %>%
        mutate(mergeTmp = paste0(nameLevels[le], counterLevel, "-", mergeTmp, "-", row_nb))

      dfTmp <- select(dfTmp,-all_of("row_nb"))
    }

    # Update df
    df <-
      merge(df,
            dfTmp,
            by.all = c(group_cols[[le]], nameColAll),
            sort = FALSE)
    nameCol <- paste0(nameLevels[le], "-", counterLevel)
    nameColAll <- c(nameColAll, nameCol)
    df <- rename(df, (!!nameCol) := mergeTmp)
  }

  return(df)
}

#' Create the Event Data.Frame
#'
#' `createLongFileEvent()`is a function used to create the Event data.frame
#' (will be exported).
#'
#' @param df_agg data.frame, output of createAggregatedDF()
#' @param dtf.event a list of Event parameters
#' @param mapping a data.frame (darwinizer mapping)
#' @return a data.frame
#'
#' @family organisation function
#'

createLongFileEvent <- function(df_agg, dtf.event, mapping) {
  eventCol <-
    c(
      "datasetID",
      "parentEventID",
      "eventID",
      "eventName",
      "originalID",
      "recordedBy",
      "locality",
      "locationID",
      "locationAccordingTo",
      "locationType",
      "xyType",
      "decimalLongitude",
      "decimalLatitude",
      "xyUncertainty",
      "footPrintWKT",
      "referenceSystem",
      "countryCode",
      "areaType",
      "areaSize",
      "areaSizeUnit",
      "timeLevel",
      "beginDate",
      'endDate',
      "eventDate",
      "dataTime",
      "notes"
    )

  finalDF <- data.frame()

  group_cols <- create_group_cols(dtf.event$fields, data.frame())

  nbLevels <- length(group_cols)
  nameLevels = paste0("Event-", seq(1, nbLevels))

  selectLevels <- c()

  for (i in 1:nbLevels) {
    selectLevels <- nameLevels[i]

    if (i > 1) {
      selectLevels <- c(selectLevels, nameLevels[i - 1])

    }

    df_agg_tmp <-
      df_agg %>% select(all_of(c(group_cols[[i]], selectLevels))) %>% distinct(across(everything()))

    colVal <- data.frame()

    if (as.character(i) %in% names(dtf.event$parameters)) {
      paramTempo <- dtf.event$parameters[[as.character(i)]]$Tempo

    } else {
      paramTempo <- c()
    }

    if (as.character(i) %in% names(dtf.event$parameters)) {
      paramSpatial <- dtf.event$parameters[[as.character(i)]]$Spatial

    } else {
      paramSpatial <- c()
    }


    if ("type_extract" %in% names(paramTempo)) {
      if (!is.na(paramTempo[["type_extract"]])) {
        df_agg_tmp <-
          make_temporal(
            df_agg_tmp,
            mapping,
            type = paramTempo$type_extract,
            format_date = paramTempo$format_date,
            sep_interval = paramTempo$sep_interval,
            values = paramTempo$values,
            tf_dic = paramTempo$tf_dic
          )
      }


    }

    if ("new_cols" %in% names(paramSpatial)) {
      for (col in names(paramSpatial$new_cols)) {
        df_agg_tmp[[col]] <- paramSpatial$new_cols[[col]]

      }
    }


    if ("new_cols" %in% names(paramTempo)) {

      for (col in names(paramTempo$new_cols)) {

        df_agg_tmp[[col]] <- paramTempo$new_cols[[col]]

      }
    }


    if (i == 1) {
      df_agg_tmp <- rename(df_agg_tmp, eventID = all_of(nameLevels[i]))


    } else {
      df_agg_tmp <- rename(df_agg_tmp, eventID = all_of(nameLevels[i])) %>%
        rename(parentEventID = all_of(nameLevels[i - 1]))


    }

    colnames(df_agg_tmp) <-
      sapply(colnames(df_agg_tmp),
             replaceFieldname,
             mapping,
             USE.NAMES = FALSE)

    finalDF <- bind_rows(finalDF, df_agg_tmp)

    finalDF <- orderDataFrame(finalDF, "eventID")
    finalDF <- orderDataFrame(finalDF, "parentEventID")


  }

  finalDF <- remove_col_all(finalDF)

  finalDF <- standardizeDF(finalDF, eventCol)

  # finalDF <- finalDF[order(finalDF[,"parentEventID"], na.last=F),]
  return(finalDF)
}

#' Create the Occurrence Data.Frame
#'
#' `createLongFileOcc()`is a function used to create the Occurrence data.frame
#' (will be exported).
#'
#' @param df_agg data.frame, output of createAggregatedDF()
#' @param dtf.occurrence a list of Occurrence parameters
#' @param dtf.event a list of Event parameters
#' @param mapping a data.frame (darwinizer mapping)
#' @return a data.frame
#'
#' @family organisation function
#'

createLongFileOcc <-
  function(df_agg,
           dtf.event,
           dtf.occurrence,
           mapping) {
    occCol <-
      c(
        "datasetID",
        "parentEventID",
        "parentOccurrenceID",
        "occurrenceID",
        "originalID",
        "recordedBy",
        "locality",
        "locationType",
        "xyType",
        "decimalLongitude",
        "decimalLatitude",
        "xyUncertainty",
        "footPrintWKT",
        "locationID",
        "locationAccordingTo",
        "referenceSystem",
        "countryCode",
        "areaType",
        "areaSize",
        "areaSizeUnit",
        "timeLevel",
        "beginDate",
        'endDate',
        "eventDate",
        "dataTime",
        "basisOfRecord",
        "scientificName",
        "occurrenceStatus",
        "recordType",
        "sex",
        "lifeStage",
        "individualCount",
        "individualID",
        "notes"
      )


    finalDF <- data.frame()
    group_cols <-
      create_group_cols(dtf.occurrence$fields, data.frame())

    nbLevels <- length(group_cols)

    nbEventLevels <-
      length(create_group_cols(dtf.event$fields, data.frame()))

    if (nbLevels >= 1) {
      nameLevels = paste0("Occ-", seq(1, nbLevels))

      for (i in 1:nbLevels) {
        selectLevels <- c(nameLevels[i], paste0("Event-", nbEventLevels))

        if (i > 1) {
          selectLevels <- c(selectLevels, nameLevels[i - 1])

        }


        df_agg_tmp <-
          df_agg %>% select(all_of(c(group_cols[[i]], selectLevels))) %>% distinct(across(everything()))


        if (as.character(i) %in% names(dtf.occurrence$parameters)) {
          paramTempo <- dtf.occurrence$parameters[[as.character(i)]]$Tempo

          paramSpatial <-
            dtf.occurrence$parameters[[as.character(i)]]$Spatial

          paramBiology <-
            dtf.occurrence$parameters[[as.character(i)]]$Biology

          paramMof  <-
            dtf.occurrence$parameters[[as.character(i)]]$Mof$Mof
        } else {
          paramTempo <- c()
          paramSpatial <- c()
          paramBiology <- c()
          paramMof <- c()
        }


        if (length(paramBiology$to_convert) > 0) {
          for (j in 1:length(paramBiology$to_convert)) {
            if (is.data.frame(paramBiology$to_convert[[j]])) {
              tabMerge <- setNames(paramBiology$to_convert[[j]],
                                   c(
                                     names(paramBiology$to_convert)[j],
                                     colnames(paramBiology$to_convert[[j]])[2]
                                   ))

              df_agg_tmp <- merge(df_agg_tmp, tabMerge)
              df_agg_tmp <-
                df_agg_tmp[, !names(df_agg_tmp) %in% c(names(paramBiology$to_convert)[j])]

            }
          }
        }

        if (length(paramBiology$new_cols) > 0) {
          for (col in names(paramBiology$new_cols)) {
            df_agg_tmp[[col]] <- paramBiology$new_cols[[col]]

          }
        }

        if ("type_extract" %in% names(paramTempo)) {
          if (!is.na(paramTempo[["type_extract"]])) {
            df_agg_tmp <-
              make_temporal(
                df_agg_tmp,
                mapping,
                type = paramTempo$type_extract,
                format_date = paramTempo$format_date,
                sep_interval = paramTempo$sep_interval,
                values = paramTempo$values,
                tf_dic = paramTempo$tf_dic
              )
          }

        }

        if ("new_cols" %in% names(paramSpatial)) {
          for (col in names(paramSpatial$new_cols)) {
            df_agg_tmp[[col]] <- paramSpatial$new_cols[[col]]

          }
        }

        if ("new_cols" %in% names(paramTempo)) {
          for (col in names(paramTempo$new_cols)) {
            df_agg_tmp[[col]] <- paramTempo$new_cols[[col]]

          }
        }


        if (i == 1 & nbLevels > 1) {
          df_agg_tmp <-
            rename(df_agg_tmp, occurrenceID = all_of(nameLevels[i]))

          if (isTRUE(paramBiology$sumCount$sum_count)) {
            field <- paramBiology$sumCount$sum_col

            df_agg_copy <- df_agg
            df_agg_copy[["individualCount"]] <-
              strtoi(df_agg_copy[[field]])
            dfSum <-
              select(df_agg_copy, all_of(c(
                "individualCount", nameLevels[i]
              ))) %>% group_by_at(c(nameLevels[i])) %>%
              summarise(
                individualCount = sum(individualCount, na.rm = TRUE),
                .groups = "keep"
              )
            dfSum <-
              rename(dfSum, occurrenceID = all_of(nameLevels[i]))

            df_agg_tmp <-
              merge(df_agg_tmp, dfSum, by.all = "occurrenceID")

          }



        } else {
          df_agg_tmp <-
            rename(df_agg_tmp, occurrenceID = all_of(nameLevels[i])) %>%
            rename(parentOccurrenceID = all_of(nameLevels[i - 1]))

        }


        df_agg_tmp <-
          rename(df_agg_tmp, parentEventID = all_of(paste0("Event-", nbEventLevels)))

        colnames(df_agg_tmp) <-
          sapply(colnames(df_agg_tmp),
                 replaceFieldname,
                 mapping,
                 USE.NAMES = FALSE)
        finalDF <- bind_rows(finalDF, df_agg_tmp)



      }

      finalDF <- orderDataFrame(finalDF, "parentEventID")
      finalDF <- orderDataFrame(finalDF, "occurrenceID")
    }

    finalDF <- remove_col_all(finalDF)
    finalDF <- standardizeDF(finalDF, occCol)


    return(finalDF)
  }


#' Create the first part of Measurement data.frame (1st level MoF)
#'
#' @param dtf.info  list of Event or Occurrence parameters
#' @param df_agg data.frame, output of createAggregatedDF()
#' @param typeLevel a string, either "Event" or "Occurrence"
#'
#' @return a data.frame
#' @export
#'

createMoF <- function(dtf.info, df_agg, typeLevel = "Event") {
  group_cols <- create_group_cols(dtf.info$fields, data.frame())

  nbLevels <- length(group_cols)

  typeLevelShort <-
    ifelse(typeLevel == "Occurrence", "Occ", "Event")
  dfMoF <- data.frame()



  for (i in 1:nbLevels) {
    nameLevel <- paste0(typeLevelShort, '-', i)
    if (typeLevelShort == "Occ")
      nameLevel <- c(nameLevel, find_parent_event(nameLevel, df_agg))

    if (as.character(i) %in% names(dtf.info$parameters)) {
      paramMof  <- dtf.info$parameters[[as.character(i)]]$Mof$Mof


    } else {
      paramMof <- NULL
    }

    if (!is.null(paramMof) && nrow(paramMof) > 0) {
      for (k in 1:nrow(paramMof)) {
        from <- paramMof[k, "From"]

        if (from == "value") {
          dfMoFtmp <- select(df_agg, all_of(c(nameLevel)))
          dfMoFtmp <- bind_cols(dfMoFtmp,
                                paramMof[k, c("measurementType",
                                              "measurementValue",
                                              "measurementUnit")] %>%
                                  mutate_all(as.character))

        } else if (from == "column") {
          valColumn <-  paramMof[k, "measurementValue"]
          dfMoFtmp <-
            distinct(select(df_agg, all_of(c(
              nameLevel, valColumn
            ))))
          dfMoFtmp <-
            rename(dfMoFtmp,
                   'measurementValue' = !!rlang::sym(valColumn))
          dfMoFtmp <- bind_cols(dfMoFtmp,
                                paramMof[k, c("measurementType",
                                              "measurementUnit")] %>%
                                  mutate_all(as.character))

          dfMoFtmp <- dfMoFtmp[!is.na(dfMoFtmp$measurementValue),]
          dfMoFtmp <- dfMoFtmp[!(dfMoFtmp$measurementValue == ""), ]
        } else {
          dfMoFtmp <- data.frame()

        }

        dfMoFtmp <- distinct(dfMoFtmp)
        dfMoFtmp <-
          rename_col_start(dfMoFtmp, "Event-", "parentEventID")
        dfMoFtmp <-
          rename_col_start(dfMoFtmp, "Occ-", "parentOccurrenceID")




        dfMoF <- rbind(dfMoF, dfMoFtmp)

      }
    }



  }

  if (typeLevel == "Event" &
      nrow(dfMoF) > 0 & (!'measurementID' %in% colnames(dfMoF))) {
    #   dfMoF <- orderDataFrame(dfMoF, "parentEventID")
    dfMoF <-
      bind_cols(data.frame('measurementID' =  paste0("Measurement", 1, "-", seq(1, nrow(
        dfMoF
      )))),
      dfMoF)
  } else if (typeLevel == "Occurrence" &
             nrow(dfMoF) > 0 & (!'measurementID' %in% colnames(dfMoF))) {
    #  dfMoF <- orderDataFrame(dfMoF, "parentOccurrenceID")
    dfMoF <-
      bind_cols(data.frame('measurementID' =  paste0("Measurement", 1, "-", seq(1, nrow(
        dfMoF
      ))),
      dfMoF))
  }

  if (typeLevel == "Occurrence") {
    l_col <- c(
      "parentMeasurementID",
      "measurementID",
      "parentOccurrenceID",
      "parentEventID",
      "measurementType",
      "measurementValue",
      "measurementUnit"
    )
  } else {
    l_col <- c(
      "parentMeasurementID",
      "measurementID",
      "parentEventID",
      "measurementType",
      "measurementValue",
      "measurementUnit"
    )
  }

  if (nrow(dfMoF) > 0) {
    dfMoF <- mutate(dfMoF, parentMeasurementID = NA)
    dfMoF <- select(dfMoF, all_of(l_col))
  }
  #  dfMoF <- remove_col_all(dfMoF)
  return(dfMoF)
}











createMoF_v1 <- function(dtf.info, df_agg, typeLevel = "Event") {
  group_cols <- create_group_cols(dtf.info$fields, data.frame())

  nbLevels <- length(group_cols)

  typeLevelShort <-
    ifelse(typeLevel == "Occurrence", "Occ", "Event")
  dfMoF <- data.frame()



  for (i in 2) {
    nameLevel <- paste0(typeLevelShort, '-', i)
    if (typeLevelShort == "Occ")
      nameLevel <- c(nameLevel, find_parent_event(nameLevel, df_agg))

    if (as.character(i) %in% names(dtf.info$parameters)) {
      paramMof  <- dtf.info$parameters[[as.character(i)]]$Mof$Mof

    } else {
      paramMof <- NULL
    }

    if (!is.null(paramMof)) {
      for (k in 1:nrow(paramMof)) {
        from <- paramMof[k, "From"]

        if (from == "value") {
          dfMoFtmp <- select(df_agg, all_of(c(nameLevel)))
          dfMoFtmp <- bind_cols(dfMoFtmp,
                                paramMof[k, c("measurementType",
                                              "measurementValue",
                                              "measurementUnit")] %>%
                                  mutate_all(as.character))

        } else if (from == "column") {
          valColumn <-  paramMof[k, "measurementValue"]
          dfMoFtmp <-
            distinct(select(df_agg, all_of(c(
              nameLevel, valColumn
            ))))
          dfMoFtmp <-
            rename(dfMoFtmp,
                   'measurementValue' = !!rlang::sym(valColumn))
          dfMoFtmp <- bind_cols(dfMoFtmp,
                                paramMof[k, c("measurementType",
                                              "measurementUnit")] %>%
                                  mutate_all(as.character))

          dfMoFtmp <- dfMoFtmp[!is.na(dfMoFtmp$measurementValue),]
          dfMoFtmp <- dfMoFtmp[!(dfMoFtmp$measurementValue == ""), ]
        } else {
          dfMoFtmp <- data.frame()

        }

        dfMoFtmp <- distinct(dfMoFtmp)
        dfMoFtmp <-
          rename_col_start(dfMoFtmp, "Event-", "parentEventID")
        dfMoFtmp <-
          rename_col_start(dfMoFtmp, "Occ-", "parentOccurrenceID")

        dfMoF <- rbind(dfMoF, dfMoFtmp)

      }
    }

    if (typeLevel == "Event" &
        nrow(dfMoF) > 0 & (!'measurementID' %in% colnames(dfMoF))) {
      dfMoF <- orderDataFrame(dfMoF, "parentEventID")
      dfMoF <-
        bind_cols(data.frame('measurementID' =  paste0("Measurement", i, "-", seq(
          1, nrow(dfMoF)
        ))),
        dfMoF)
    } else if (typeLevel == "Occurrence" &
               nrow(dfMoF) > 0 & (!'measurementID' %in% colnames(dfMoF))) {
      dfMoF <- orderDataFrame(dfMoF, "parentOccurrenceID")
      dfMoF <-
        bind_cols(data.frame('measurementID' =  paste0("Measurement", i, "-", seq(
          1, nrow(dfMoF)
        )),
        dfMoF))
    }

  }



  if (typeLevel == "Occurrence") {
    l_col <- c(
      "parentMeasurementID",
      "measurementID",
      "parentOccurrenceID",
      "parentEventID",
      "measurementType",
      "measurementValue",
      "measurementUnit"
    )
  } else {
    l_col <- c(
      "parentMeasurementID",
      "measurementID",
      "parentEventID",
      "measurementType",
      "measurementValue",
      "measurementUnit"
    )
  }

  if (nrow(dfMoF) > 0) {
    dfMoF <- mutate(dfMoF, parentMeasurementID = NA)
    dfMoF <- select(dfMoF, all_of(l_col))
  }
  #  dfMoF <- remove_col_all(dfMoF)
  return(dfMoF)
}


#' Remove Empty Columns
#' Used in func: createLongFileEvent, createLongFileOcc
#' @param df a data.frame
#'
#' @return a data.frame
#' @export
#'

remove_col_all <- function(df) {
  if (nrow(df) > 0) {
   # df[df == ""] <- NA
    # df <- df[, colSums(is.na(df)) < nrow(df), drop = FALSE]

    df_mod <- df %>%
      mutate(across(everything(), as.character))
    df_mod[df_mod == ""] <- NA
    df_mod <- df_mod[, colSums(is.na(df_mod)) < nrow(df_mod), drop = FALSE]
    df <- select(df, all_of(names(df_mod)))

  }

  return(df)
}


#' Title
#'
#' @param dtf.info  list of Event or Occurrence parameters
#' @param dfMoF data.frame, output of createMoF()
#' @param df_agg data.frame, output of createAggregatedDF()
#' @param typeLevel a string, either "Event" or "Occurrence"
#'
#' @return a data.frame
#' @export
#'

createMoFMoF <-
  function(dtf.info, dfMoF, df_agg, typeLevel = "Event") {
    group_cols <- create_group_cols(dtf.info$fields, data.frame())
    nbLevels <- length(group_cols)

    typeLevelShort <-
      ifelse(typeLevel == "Occurrence", "Occ", "Event")
    parentField <-
      ifelse(typeLevel == "Occurrence",
             "parentOccurrenceID",
             "parentEventID")
    #dfMoF_init <- dfMoF
    if (nrow(dfMoF) > 0) {
      if (!"parentMeasurementID" %in% colnames(dfMoF)) {
        dfMoF <- mutate(dfMoF, parentMeasurementID = NA)

      }
      if (!"parentEventID" %in% colnames(dfMoF)) {
        dfMoF <- mutate(dfMoF, parentEventID = NA)

      }
      for (i in 1:nbLevels) {
        nameLevel <- paste0(typeLevelShort, '-', i)
        # if (typeLevelShort == "Occ") nameLevel <- c(nameLevel, find_parent_event(nameLevel, df_agg))

        if (as.character(i) %in% names(dtf.info$parameters)) {
          paramMof  <- dtf.info$parameters[[as.character(i)]]$Mof$MofMof

        } else {
          paramMof <- NULL
        }

        if (!is.null(paramMof) && nrow(paramMof) > 0) {
          for (k in 1:nrow(paramMof)) {
            from <- paramMof[k, "From"]
            parent <- paramMof[k, "measurementParent"]

            if (from == "value") {
              sel_cols <- c("measurementID",
                            "parentEventID")
              if (typeLevelShort == "Occ")
                sel_cols <- c(sel_cols, "parentOccurrenceID")
              dfMoFtmp <-
                select(filter(dfMoF, measurementType == parent),
                       all_of(sel_cols)) %>%
                rename(parentMeasurementID = measurementID)

              dfMoFtmp <- bind_cols(dfMoFtmp,
                                    paramMof[k, c("measurementType",
                                                  "measurementValue",
                                                  "measurementUnit")])

            } else if (from == "column") {
              valColumn <-  paramMof[k, "measurementValue"]
              dfMoFtmp <-
                distinct(select(df_agg, all_of(c(
                  nameLevel, valColumn
                )))) %>%
                rename(!!parentField := !!rlang::sym(nameLevel))
              dfMoFtmp2 <-
                distinct(select(
                  filter(dfMoF, measurementType == parent),
                  all_of(c(
                    "measurementID",
                    parentField
                  ))
                ))

              dfMoFtmp <- merge(dfMoFtmp, dfMoFtmp2) %>%
                rename(parentMeasurementID = measurementID)

              dfMoFtmp <-
                rename(dfMoFtmp,
                       'measurementValue' = !!rlang::sym(valColumn))
              dfMoFtmp <- bind_cols(dfMoFtmp,
                                    paramMof[k, c("measurementType",
                                                  "measurementUnit")])

              dfMoFtmp[["measurementValue"]] <-
                as.character(dfMoFtmp[["measurementValue"]])

            } else {
              dfMoFtmp <- data.frame()

            }

            dfMoFtmp <- distinct(dfMoFtmp)
            level = unique(as.numeric(str_extract(dfMoFtmp[["parentMeasurementID"]], "[0-9]+")[[1]])) + 1
            dfMoF[["level"]] <-
              as.numeric(str_extract(dfMoF[["parentMeasurementID"]], "[0-9]+")[[1]])
            maxID <- nrow(filter(dfMoF, level == level))
            dfMoFtmp <-
              bind_cols(data.frame('measurementID' =  paste0(
                "Measurement", level, "-", seq(1 + maxID, nrow(dfMoFtmp))
              )),
              dfMoFtmp)

            if (!"parentMeasurementID" %in% colnames(dfMoFtmp)) {
              dfMoFtmp <- mutate(dfMoFtmp, parentMeasurementID = NA)

            }
            if (!"parentEventID" %in% colnames(dfMoFtmp)) {
              dfMoFtmp <- mutate(dfMoFtmp, parentEventID = NA)

            }

            dfMoF <- rbind(select(dfMoF,-all_of("level")), dfMoFtmp)

          }

          if (typeLevel == "Occurrence") {
            l_col <- c(
              "parentMeasurementID",
              "measurementID",
              "parentOccurrenceID",
              "parentEventID",
              "measurementType",
              "measurementValue",
              "measurementUnit"
            )
          } else {
            l_col <- c(
              "parentMeasurementID",
              "measurementID",
              "parentEventID",
              "measurementType",
              "measurementValue",
              "measurementUnit"
            )
          }

          dfMoF <- select(dfMoF, all_of(l_col))
        }

      }

    }

    return(dfMoF)
  }



#' Create Levels Relationships
#' Used in func: find_parent
#' @param df a data.frame, output of createAggregatedDF()
#'
#' @return a data.frame (columns: parent, children)
#' @export
#'

create_df_level <- function(df) {
  levels <-
    colnames(df %>% select(starts_with("Event-") |
                             starts_with("Occ-")))
  df_levels <-
    data.frame("parent" = c(NA, levels),
               "children" = c(levels, NA))
  return(df_levels)
}

#' Find the parent Level
#' Used in func: find_parent_event
#' @param level a string, either "Occ" or "Event"
#' @param df df a data.frame, output of createAggregatedDF()
#'
#' @return a string, the name of the parent level (e.g. "Event-2")
#' @export
#'

find_parent <- function(level, df) {
  df_levels <- create_df_level(df)
  previous_level <-
    pull(filter(df_levels, children == level), parent)
  return(previous_level)

}

#' Find the Event parent Level
#' Used in func: createMoF
#' @param level a string, either "Occ" or "Event"
#' @param df df a data.frame, output of createAggregatedDF()
#'
#' @return a string, the name of the Event parent level (e.g. "Event-2")
#' @export
#'

find_parent_event <- function(level, df) {
  parent <- find_parent(level, df)
  while (isFALSE(str_detect(parent, "Event-"))) {
    parent <- find_parent(parent, df)
  }
  return(parent)

}

#' Duplicate a Column
#' Used in module: mod_Import
#' @param df a data.frame
#' @param col_dupli a list of strings
#'
#' @return a data.frame
#' @export
#'

duplicate_columns <- function(df, col_dupli) {
  col_dupli <- col_dupli[col_dupli %in% names(df)]

  if (length(col_dupli) == 0) {
    return(df)

  } else {
    v <- c(names(df), col_dupli)

    new_colnames <- pull(
      tibble(v) %>% mutate(id = 1) %>%
        mutate(v = str_replace(v, "(_[0-9]+)", "")) %>%
        na.omit %>%
        group_by(v) %>%
        summarise(id = cumsum(id)) %>%
        mutate(v2 = paste(v, id, sep = "_")) %>%
        mutate(v2 = ifelse(id == 1, v, v2)) %>%
        filter(id > 1) %>%
        arrange(desc(id)),
      v2
    )
    new_colnames <- new_colnames[!new_colnames %in% names(df)]
    new_df <-
      setNames(data.frame(df[, sort(col_dupli)]), sort(new_colnames))
    new_df <- bind_cols(df, new_df)
    return(new_df)

  }
}

#' Order a Data.frame (row-based)
#' Used in func: createLongFileEvent, createLongFileOcc
#' @param df a data.frame
#' @param col a column name, basis to order data.frame
#'
#' @return a data.frame
#' @export
#'

orderDataFrame <- function(df, col) {
  if (col %in% colnames(df)) {
    df[["colToProcess"]] <- df[[col]]

    df <-
      df %>% rowwise() %>% mutate(nbLevel = length(unlist(str_split(
        colToProcess, "-"
      ))))
    maxLevel = max(df[["nbLevel"]])
    df <-
      df %>% rowwise() %>% mutate(nbLevel = maxLevel - nbLevel) %>% rowwise() %>%
      mutate(comp = paste(replicate(nbLevel, "-0"), collapse = "")) %>% rowwise() %>%
      mutate(colToProcess = paste0(colToProcess, comp))
    df <- select(df,-all_of(c("comp", "nbLevel")))

    orderCols <-
      data.frame(matrix(
        unlist(str_split(df[["colToProcess"]], "-")),
        nrow = nrow(df),
        ncol = length(unlist(str_split(df[["colToProcess"]], "-"))) /
          nrow(df),
        byrow = TRUE
      ))[, -1, drop = FALSE]

    orderCols <- orderCols %>% mutate_all(as.numeric)

    df <- bind_cols(df, orderCols)

    for (i in (ncol(df):(ncol(df) - ncol(orderCols) + 1))) {
      df <- df[order(df[, i]), ]
    }

    df <-
      select(df,-all_of(c(colnames(orderCols), "colToProcess")))
    df <- data.frame(df)
  }
  return(df)
}



#' Order a Data.frame (column-based)
#' Used in func: createLongFileEvent, createLongFileOcc
#' @param df a data.frame
#' @param finalCol a list of columns
#'
#' @return a data.frame
#' @export
#'

standardizeDF <- function(df, finalCol) {
  finalCol <- c(finalCol[finalCol %in% colnames(df)],
                colnames(df)[!colnames(df) %in% finalCol])
  df <- select(df, all_of(finalCol))
  return(df)

}


#' List Member
#' Used in func: exclude
#' @param list a list
#' @param names a vector of strings
#'
#' @return a list
#' @export
#'

member <- function(list, names) {
  ## return the elements of the list with the input names
  member..names <- names(list)
  index <- which(member..names %in% names)
  list[index]
}


#' Exclude From A List
#'
#' @param list a list
#' @param names a vector of strings
#'
#' @return a list
#' @export
#'

exclude <- function(list, names) {
  ## return the elements of the list not belonging to names
  member..names <- names(list)
  index <- which(!(member..names %in% names))
  list[index]
}
