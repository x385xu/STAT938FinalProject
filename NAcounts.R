# https://github.com/stanford-policylab/opp/blob/master/README.md
# install required packages
# renv::restore(
#   lockfile = "renv.lock",
#   rebuild  = TRUE
# )
# 
# # rgdal was removed from cran in 2023
# # renv::snapshot(exclude = "rgdal")
# 
# source("opp.R")
# 
# # Set download directory
# opp_set_download_directory("D:/OneDrive - University of Waterloo/GradCourses/STAT 938/Final Project/Data")
#  
# # Download and load clean data
# 
# #====seattle==========
# opp_download_clean_data("wa", "seattle")
# seattle <- opp_load_clean_data("wa", "seattle")
# 
# seattle <- seattle %>% 
#   select(date, time, 
#          subject_race, subject_sex, 
#          violation, outcome, 
#          vehicle_color, vehicle_make)
# 
# na_counts <- seattle %>% summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}"))
# 
# na_counts %>%
#   mutate(state = "wa",
#          city  = "seattle",
#          total = length(seattle$date)) %>%
#   select(state, city, total, everything())

#==========Helper Function=============
library(dplyr)
library(purrr)
library(glue)
library(tibble)

get_na_counts <- function(state, city, data_dir = "data/") {
  # build the local file path
  file_name <- if (city == "statewide") {
    glue("{state}_statewide.rds")
  } else {
    glue("{city}.rds")
  }
  path <- file.path(file_name)
  
  # 1) Try to read locally, fail gracefully
  df <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    warning(glue("{state}/{city}: failed to read {path}: {e$message}"))
    return(tibble())  # so downstream still works
  })
  
  # If read failed, return one row of NAs
  if (nrow(df) == 0) {
    na_cols <- paste0("na_", c(
      "date","time","subject_race","subject_sex",
      "violation","outcome",
      "vehicle_color","vehicle_make"
    ))
    return(
      tibble(
        state = state,
        city  = city,
        total = NA_integer_,
        !!!set_names(as.list(rep(NA_integer_, length(na_cols))), na_cols)
      )
    )
  }
  
  # 2) existing logic for counting NAs
  desired_cols <- c(
    "date","time","subject_race","subject_sex",
    "violation","outcome","vehicle_color","vehicle_make",
    "officer_age", "officer_race"
  )
  present <- intersect(desired_cols, names(df))
  missing <- setdiff(desired_cols, names(df))
  if (length(missing)) {
    warning(glue("{state}/{city} missing columns: {paste(missing, collapse=', ')}"))
  }
  
  df_sel <- df %>%
    mutate(!!!set_names(lapply(missing, function(x) NA), missing)) %>%
    select(all_of(desired_cols))
  
  na_counts <- df_sel %>%
    summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}"))
  
  # 3) tag & return
  na_counts %>%
    mutate(
      state = state,
      city  = city,
      total = nrow(df_sel)
    ) %>%
    select(state, city, total, everything())
}


states <- tribble(
  ~state, ~city,
  "ca", "los angeles",
  "ca", "san diego",
  "ca", "san francisco",
  "ca", "statewide",
  "fl", "tampa",
  "fl", "statewide",
  "il", "chicago",
  "il", "statewide",
  "la", "new orleans",
  "wa", "seattle",
  "wa", "statewide",
  "ny", "albany",
  "ny", "statewide"
)

na_summary <- states %>% pmap_dfr(~ get_na_counts(..1, ..2))

na_summary <- na_summary %>%
  mutate(
    across(
      starts_with("na_"), 
      ~ if_else(.x == total, NA_integer_, .x)
    )
  )

write.csv(na_summary, "D:/OneDrive - University of Waterloo/GradCourses/STAT 938/Final Project/STAT938FinalProject/na_summary.csv", row.names = FALSE)


