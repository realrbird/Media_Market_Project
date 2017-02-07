# Load libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)
library(stringr)
library(data.table)

#*******************************************************************
##############################################################
# Read in and bind files into complete file and save as rds
##############################################################
# ******************************************************************

# list.files()
# file_list <- list()
# for (i in 1:length(list.files())) {
#   file_list[[i]] <- data.table::fread(list.files()[i])
# }
# df <- as_tibble(rbindlist(file_list)) 
# glimpse(df)
# readr::write_rds(df, "entire_data.rds")

##################################################
# Original Read in data -- No longer needed
##################################################

# dma_vap <- readxl::read_excel("C:\\Users\\robert_bird\\Desktop\\Media Market Project\\battleground_DMAs.xlsx")
# bg_dma <- readxl::read_excel("C:\\Users\\robert_bird\\Desktop\\Media Market Project\\battleground_DMAs.xlsx",
                             # sheet = 2)
#ad_archive <- readr::read_csv("C:\\Users\\robert_bird\\Desktop\\Media Market Project\\political_ad_archive_data.csv")
#dplyr::glimpse(ad_archive) # Look at data
# Convert to tibbles
#dma_vap <- tibble::as_tibble(dma_vap)
#bg_dma <- tibble::as_tibble(bg_dma)

######################################################
# New Code
#####################################################

# Set working directory
setwd("C:\\Users\\robert_bird\\Desktop\\Media_Market_Project\\PolAdFiles")

# Load in data
ad_archive <- readr::read_rds("entire_data.rds")
dd <- read_csv("C:\\Users\\robert_bird\\Desktop\\Media_Market_Project\\orig_data.csv")

# Unique locationd
orig_cities <- unique(dd$location)
orig_cities <- orig_cities[1:25]; orig_cities

# find coresponding dma codes for each city. Must be in same order as 'unique_cities'
# Original dma_codes
dma_codes <- c(839, 560, 539, 510, 637,504,751,617,807,753,506,511,534,
               624,501,811,515,679,544,567,752,528, 517,546,573)

# Set up a list with each city and its dma
zip_vec <- function(vec1, vec2) {
  stopifnot(length(vec1) == length(vec2))
  alist <- list()
  for (i in 1:length(vec1)) {
    alist[[i]] <- c(Location = vec1[i], DMA = vec2[i])
  }
  return(alist)
}

#create list
cities_dma_list <- zip_vec(orig_cities, dma_codes)
cities_dma_list

# remove orig_data 
rm(dd)

# find Unique values for location for the as_archive data
unique_cities <- unique(ad_archive$location)
unique_cities <- unique_cities[1:25]; unique_cities

# Make sure orig_data locations and ad_archive locations are the same
identical(sort(orig_cities), sort(unique_cities))
# If it is, we can assign orig_cities to unique_cities and delete orig_cities
unique_cities <- orig_cities; rm(orig_cities)

# Convert from strings to datetimes
ad_archive$start_time <- lubridate::as_datetime(ad_archive$start_time)
ad_archive$end_time <- lubridate::as_datetime(ad_archive$end_time)



# Write function to create vector of dma codes for each location
parse_dma <- function(loc) {
  if (is.na(loc)) {
    return(NA)
  } else {
    for (i in 1:25) {
      if (loc == "") {
        return(NA)
      }
      else if (loc == unique_cities[i]) {
        return(dma_codes[i])
      } 
    }
  }
}
# apply function to each element in the location variable of the data frame
dma <- as.numeric(sapply(ad_archive$location, parse_dma))
# create a new column with the dma codes in the data frame
ad_archive$dma <- dma

# Create vector of dma codes which have overlapping states. 
overlap_dmas <- c(560, 504, 751, 506, 511, 624, 501, 811, 515, 679, 544, 567, 517, 573) 

parse_overlap <- function(adma) {
  if (is.na(adma)) {
    return(NA)
  } else {
    if (adma %in% overlap_dmas) {
      return(1)
    } else {
      return(0)
    }
  }
}
dma_overlap <- sapply(ad_archive$dma, parse_overlap)
ad_archive$dma_overlap <- dma_overlap

# Create a Date variable in addition to the datetime variables
ad_archive$date <- lubridate::as_date(ad_archive$start_time)

# Add States for each ad based on DMA code
parse_states <- function(adma) {
  if (is.na(adma)) {
    return(list(State_1 = NA, State_2 = NA, State_3 = NA, State_4 = NA, State_5 = NA))
  } else {
    if (!(adma %in% overlap_dmas)) {
      for (i in 1:25) {
        if (adma == dma_codes[i]) {
          return(list(State_1 = str_sub(unique_cities[i],-2,-1), State_2 = NA, State_3 = NA, State_4 = NA, State_5 = NA))
        }
      }
    } else {
      if (adma == 560) {
        return(list(State_1 = "NC", State_2 = "VA", State_3 = NA, State_4 = NA, State_5 = NA))
      } else if (adma == 504) {
        return(list(State_1 = "PA", State_2 = "NJ", State_3 = "DE", State_4 = NA, State_5 = NA))
      } else if (adma == 751) {
        return(list(State_1 = "CO", State_2 = "NE", State_3 = "NV", State_4 = "WY", State_5 = NA))
      } else if (adma == 506) {
        return(list(State_1 = "MA", State_2 = "NH", State_3 = "VT", State_4 = NA, State_5 = NA))
      } else if (adma == 511) {
        return(list(State_1 = "DC", State_2 = "MD", State_3 = "PA", State_4 = "VA", State_5 = "WV"))
      } else if (adma == 624) {
        return(list(State_1 = "IA", State_2 = "NE", State_3 = "SD", State_4 = NA, State_5 = NA))
      } else if (adma == 501) {
        return(list(State_1 = "CT", State_2 = "NJ", State_3 = "NY", State_4 = "PA", State_5 = NA))
      } else if (adma == 811) {
        return(list(State_1 = "CA", State_2 = "NV", State_3 = NA, State_4 = NA, State_5 = NA))
      } else if (adma == 515) {
        return(list(State_1 = "IN", State_2 = "KY", State_3 = "OH", State_4 = NA, State_5 = NA))
      } else if (adma == 679) {
        return(list(State_1 = "IA", State_2 = "MO", State_3 = NA, State_4 = NA, State_5 = NA))
      } else if (adma == 544) {
        return(list(State_1 = "NC", State_2 = "VA", State_3 = NA, State_4 = NA, State_5 = NA))
      } else if (adma == 567) {
        return(list(State_1 = "NC", State_2 = "SC", State_3 = "GA", State_4 = NA, State_5 = NA))
      } else if (adma == 517) {
        return(list(State_1 = "NC", State_2 = "SC", State_3 = NA, State_4 = NA, State_5 = NA))
      } else if (adma == 573) {
        return(list(State_1 = "VA", State_2 = "WV", State_3 = NA, State_4 = NA, State_5 = NA))
      }
    }
  }
}
# apply function
States <- lapply(ad_archive$dma, parse_states)
# bind list of 5 states into a data.table
states_df <- rbindlist(States)
# Coerce to tibble
states_df <- as_tibble(states_df)
# bind this new states tibble to the entire ad_archive tibble
df <- bind_cols(ad_archive, states_df)
# Reorder columns
df <- dplyr::select(df, date, dma, State_1:State_5, dma_overlap, everything())
# Edit State names -- Two were messed us due to them being spelled out in 'unique_cities' rather than abbriviations
df$State_1 <- ifelse(df$State_1 == 'wa', "IA",
                     ifelse(df$State_1 == "io", "OH", df$State_1))
# sort by 
df <- dplyr::arrange(df, date)

# We can remove all the old data now that we have out new data frame
rm(States); rm(ad_archive); rm(states_df)

# Create a list of primary Dates
states <- c(state.name)
state_abb <- c(state.abb)
rep_prim <- c("2016-03-01","2016-03-01","2016-03-22","2016-03-01","2016-06-07","2016-03-01", "2016-04-26",
              "2016-04-26","2016-03-15","2016-03-01","2016-03-08","2016-03-08","2016-03-15","2016-05-03",
              "2016-02-01","2016-03-05", "2016-03-05","2016-03-05","2016-03-05","2016-04-26","2016-03-01",
              "2016-03-08","2016-03-01","2016-03-08","2016-03-15","2016-06-07","2016-05-10","2016-02-23",
              "2016-02-09","2016-06-07","2016-06-07","2016-04-19","2016-03-15","2016-06-07","2016-03-15",
              "2016-03-01","2016-05-17","2016-04-26","2016-04-26","2016-02-20","2016-06-07","2016-03-01",
              "2016-03-01","2016-03-22","2016-03-01","2016-03-01","2016-05-24","2016-05-10","2016-04-05",
              "2016-03-12")

dem_prim <- c("2016-03-01","2016-03-26","2016-03-22","2016-03-01","2016-06-07","2016-03-01","2016-04-26",
              "2016-04-26","2016-03-15","2016-03-01","2016-03-26","2016-03-22","2016-03-15","2016-05-03",
              "2016-02-01","2016-03-05", "2016-05-17","2016-03-05","2016-03-06","2016-04-26","2016-03-01",
              "2016-03-08","2016-03-01","2016-03-08","2016-03-15","2016-06-07","2016-03-05","2016-02-20",
              "2016-02-09","2016-06-07","2016-06-07","2016-04-19","2016-03-15","2016-03-01", "2016-03-15",
              "2016-03-01","2016-05-17","2016-04-26","2016-04-26","2016-02-27","2016-06-07","2016-03-01",
              "2016-03-01","2016-03-22","2016-03-01","2016-03-01","2016-03-26","2016-05-10","2016-04-05",
              "2016-04-09")

sp_df <- as_tibble(data.frame(cbind(states, state_abb, rep_prim, dem_prim), stringsAsFactors = F))
sp_df$rep_prim <- as_date(sp_df$rep_prim)
sp_df$dem_prim <- as_date(sp_df$dem_prim)
sp_df <- dplyr::arrange(sp_df, rep_prim, dem_prim)
sp_df

# Remove data we don't need 
rm(states); rm(state_abb); rm(rep_prim); rm(dem_prim)


# Set up data
df_for_next_function <- dplyr::select(df, State_1:State_5,date); df_for_next_function

bg_dummy <- function(x, var) {
  if (is.na(x[6])) {
  result <- NA
  }
  else if (is.na(x[1])) {
    result <- NA
  } else {
    x_6 <- readr::parse_date(x[6], format = "%Y-%m-%d")
    result <- 0
    for (i in 1:5) {
      if (is.na(x[i])) {
        break
      } else {
        if (x[i] %in% sp_df$state_abb) {
          the_state <- x[i] 
          index <- grep(the_state, sp_df$state_abb)
          rep_prim_date <- var[index]
          if (between(x_6, rep_prim_date - weeks(4), rep_prim_date)) {
            result <- 1
            break
          }
        }
      }
    }
  }
  return(result)
}

(a_now <- lubridate::now())
rep_prim_dummy <- apply(df_for_next_function, 1, bg_dummy, var = sp_df$rep_prim)
lubridate::now() - a_now
df$rep_prim_dummy <- rep_prim_dummy; rm(rep_prim_dummy)
df <- dplyr::select(df, date:dma_overlap, rep_prim_dummy, everything())
df <- dplyr::rename(df, rep_battleground = rep_prim_dummy)

(a_now <- lubridate::now())
dem_prim_dummy <- apply(df_for_next_function, 1, bg_dummy, var = sp_df$dem_prim)
lubridate::now() - a_now
df$dem_battleground <- dem_prim_dummy; rm(dem_prim_dummy)
df <- dplyr::select(df, date:dma_overlap, dem_battleground, everything())





