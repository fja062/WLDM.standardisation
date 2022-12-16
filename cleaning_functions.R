#### functions
# imports data and removes empty and unused columns
import_data <- function(country) {
  st_read(dsn = "/Users/f.jaroszynska/Desktop", layer = "wldm_db") %>%
    filter(country == {{country}}) %>%
    select(!starts_with("bio_")) %>%
    select(!starts_with("lc_")) %>%
    janitor::remove_empty("cols")
}


# check dates
check_date <- function(data) {
  as_tibble({{data}}) %>%
    distinct(
      dayBeginDate,
      monthBeginDate,
      yearBeginDate,
      dayEndDate,
      monthEndDate,
      yearEndDate
    )
}

# tidy column headings
tidy_col_names <- function(df) {
  names(df) <- gsub("\\*", "", names(df))
  names(df) <- gsub("\\^", "", names(df))
  names(df) <- gsub("\\ ", "_", names(df))
  names(df) <- janitor::make_clean_names(names(df), "small_camel")
  #df
  # rename column headings
  df <- if(!("xyType" %in% colnames(df))){rename(df, xyType = locationType, locationType = areaType)} else{df}
#, scientificName = species
  df <- df %>%
    filter(across(everything(), ~ !grepl("FIELD CONTENT", .))) %>%
    filter(across(everything(), ~ !grepl("EXAMPLE", .))) %>%
    filter(across(everything(), ~ !grepl("ESEMPIO", .))) %>%
    filter(across(everything(), ~ !grepl("DESCRIZIO", .))) %>%
    filter(across(everything(), ~ !grepl("Reference of metadata code", .))) %>%
    janitor::remove_empty("cols")


  df
}

concatenate_sex <- function(df){
  if(any(grepl("harvIndet", colnames(df))))
  {
     df <- df %>%
    pivot_longer(c(harvMal, harvFem, harvIndet),
                 names_to = "sex",
                 values_to = "individualCount") %>%
    mutate(individualCount = as.numeric(individualCount),
           sex = case_when(
             sex == "harvMal" ~ "male",
             sex == "harvFem" ~ "female",
             sex == "harvIndet" ~ "indetermined"
           ))
  }
  else{
    df <- df %>%
      pivot_longer(c(harvMal, harvFem),
                   names_to = "sex",
                   values_to = "individualCount") %>%
      mutate(individualCount = as.numeric(individualCount),
             sex = case_when(
               sex == "harvMal" ~ "male",
               sex == "harvFem" ~ "female"
             ))
  }

  df <- filter(df, !is.na(individualCount))
  df

}

concatenate_age_sex <- function(df){
  if(any(grepl("harvIndet", colnames(df))))
  {
    df <- df %>%
      #select(-harvTot) %>%
      pivot_longer(c(harvMal, harvFem, harvJuv, harvIndet),
                   names_to = "sex",
                   values_to = "individualCount",
                   values_drop_na = TRUE) %>%
      mutate(individualCount = as.numeric(individualCount),
             lifeStage = case_when(
               sex == "harvJuv" ~ "juvenile",
               sex %in% c("harvMal", "harvFem") ~ "adult",
               sex == "harvIndet" ~ "indetermined"
             ),
             sex = case_when(
               sex == "harvJuv" ~ "indetermined",
               sex == "harvMal" ~ "male",
               sex == "harvFem" ~ "female",
               sex == "harvIndet" ~ "indetermined"
             )
      )}
  else{
    df <- df %>%
      #select(-individualCount) %>%
      pivot_longer(c(harvMal, harvFem, harvJuv),
                   names_to = "sex",
                   values_to = "individualCount",
                   values_drop_na = TRUE) %>%
      mutate(individualCount = as.numeric(individualCount),
             lifeStage = case_when(
               sex == "harvJuv" ~ "juvenile",
               sex %in% c("harvMal", "harvFem") ~ "adult"
             ),
             sex = case_when(
               sex == "harvJuv" ~ "indetermined",
               sex == "harvMal" ~ "male",
               sex == "harvFem" ~ "female"
             )
      )
  }
  df <- filter(df, !is.na(individualCount))
  df


}

create_dates <- function(df){
  if (any(grepl("dayBeginDate", colnames(df)))) {
    df <- mutate(df, beginDate = paste(dayBeginDate, monthBeginDate, yearBeginDate, sep = "/"))
  }
    if(any(grepl("dayEndDate", colnames(df)))) {
    df <- mutate(df, endDate = paste(dayEndDate, monthEndDate, yearEndDate, sep = "/"))
  }
  else if(any(grepl("monthBeginDate", colnames(df)))) {

    df <- df %>%
      mutate(beginDate = case_when(
        !is.na(monthBeginDate) ~ paste(monthBeginDate, yearBeginDate, sep = "/"),
        TRUE ~ yearBeginDate
      ),
      endDate = case_when(
        !is.na(monthEndDate) ~ paste(monthEndDate, yearEndDate, sep = "/"),
        TRUE ~ yearEndDate)
      )

  }
  else if(any(grepl("yearBeginDate", colnames(df)))){
    df <- df %>%
      mutate(beginDate = yearBeginDate,
             endDate = yearEndDate
      )
  }
  else {
    df <- df %>%
      mutate(beginDate = yearEndDate,
             endDate = yearEndDate
      )

  }
  df

}

load_data <- function(country,
                      dataType,
                      species,
                      scientificName) {
  if ({{country}} == "Estonia") {
    # load directory list
    dir_list <- fs::dir_ls(paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/original/Carnivores/CR-HuntingBags/", {{species}}), regexp = ".*\\.shp$")

    # function to read in file names and append file name
    read_files <- function(filename) {
      table <- read_sf(filename)
      table <- table %>%
        mutate(locationAccordingTo = str_remove(filename, paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/original/Carnivores/CR-HuntingBags/", {{species}}, "/")))
    }

    # apply read-in function to file list
    shapefile_list <- lapply(dir_list, read_files)

    #bind files together
    spp <- bind_rows(shapefile_list)

    # create timestamps
    spp <- spp %>%
      mutate(
        beginDate = paste(Day_start, Month_star, Year_start, sep = "/"),
        endDate = paste(Day_end, Month_end, Year_end, sep = "/"),
        # create missing columns
        scientificName = {{scientificName}},
        referenceSystem = "WGS84",
        areaType = "huntingGround",
        locality = paste(County, HG_Name, sep = " - ")
      ) %>%
      st_drop_geometry() %>%
      #rename columns
      rename(
        locationID = HG_ID,
        x = X_WGS,
        y = Y_WGS,
        dayBeginDate = Day_start,
        monthBeginDate = Month_star,
        yearBeginDate = Year_start,
        dayEndDate = Day_end,
        monthEndDate = Month_end,
        yearEndDate = Year_end
      ) %>%
      as.data.frame()

  }
  else {
    dir_list <- fs::dir_ls(paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/in_progress/Latvia/", {{dataType}}, "/", {{species}}), regexp = ".*\\.shp$")

    read_files <- function(filename) {
      table <- read_sf(filename)
      table <- table %>%
        mutate(locationAccordingTo = str_remove(
          filename, paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/in_progress/Latvia/", {{dataType}},  "/", {{species}}, "/")
        ))
    }
    # apply read-in function to file list
    shapefile_list <- lapply(dir_list, read_files)

    #bind files together
    spp <- bind_rows(shapefile_list)

    # create timestamps
    spp <- spp %>%
      mutate(beginDate = paste(Start_day, Start_mont, Start_year, sep = "/"),
             endDate = paste(End_day, End_month, End_year, sep = "/"),
             # create missing columns
             referenceSystem = "WGS84",
             areaType = "huntingGround",
      ) %>%
      st_drop_geometry() %>%
      #rename columns
      rename(
        locationID = FO_ID,
        x = X_WGS,
        y = Y_WGS,
        dayBeginDate = Start_day,
        monthBeginDate = Start_mont,
        yearBeginDate = Start_year,
        dayEndDate = End_day,
        monthEndDate = End_month,
        yearEndDate = End_year,
        scientificName = Species
      ) %>%
      as.data.frame()

  }


  # show data
  spp

}




# function to read in file names and append file name
import_excel_data <- function(directory){
  dir_list <- fs::dir_ls(paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-se-UNISS-UNITO/", {{country}}, "/", {{species}}), regexp = ".*\\.xlsx$")

  read_files <- function(filename){
    table <- read_excel(path = filename, sheet = 3)
    table <- table %>%
      mutate(locationAccordingTo = str_remove(filename, paste0("/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-se-UNISS-UNITO/", {{country}},  "/", {{species}}, "/"))) %>%
      janitor::remove_empty("cols") %>%
      filter(is.na(occurrenceID))
  }

  # apply read-in function to file list
  file_list <- lapply(dir_list, read_files)

  #bind files together
  spp <- bind_rows(file_list)
  spp
}


library(tidyverse)

path1 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-se-UNISS-UNITO/"
path2 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/in_progress/"
path3 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-nw-ITAW/"
path4 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-ne-MRI/"
path5 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/EW-sw-OFB/"
path6 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/IREC/"
path7 <- "/Users/f.jaroszynska/Documents/enetwild/1_data/original/"
