#############################################################################
#     _   _____    ____        __  ____
#    / | / /   |  / __ )____ _/ /_/ __ \
#   /  |/ / /| | / __  / __ `/ __/ /_/ /
#  / /|  / ___ |/ /_/ / /_/ / /_/ _, _/
# /_/ |_/_/  |_/_____/\__,_/\__/_/ |_|
#
# R Tools for accessing and manipulating North American Bat Monitoring data
#
# Github: https://github.com/ennsk/NABatR/tree/service_data_access
# Written by: Kyle Enns
# Created: 4/29/2019
#############################################################################

library(jsonlite)
library(tidyverse)

# Read in species datatable from nabat
species_df = read_csv("data/bat_species_table.csv")
species = species_df$species_code[1:53]

#' @title NABat Build Acoustic Observed Nights Function
#'
#' @import jsonlite
#' @import tidyverse
#'
#' @description
#' This function takes the acoustic data output from rename_raw_acoustic.R and rebuilds a dataframe
#' that can be used in "Wilson's" model.
#'
#' @param acoustic_data data.frame which has all needed data for the project.
#' @param out_file String to write the new data.frame out as. ex: '/path/to/downloads/Observed_Nights_Acoustic_Auto.csv'
#' @param acoustic_type String that can either be 'manual' or 'auto'
#' @keywords bats, NABat, GQL, Acoustic, Stationary, Data, Automatic, Manual, Software
#'
#' @export
build_nights_df = function(acoustic_data,
                           out_file,
                           acoustic_type){

  nights_df = data.frame()
  project_id = unique(acoustic_data$project_id)
  # Extract all GRTS Cell ids within acoustic_data
  GRTS_ids = unique(acoustic_data$grts_cell_id)

  for (id in GRTS_ids){
    ex_grts_df = subset(acoustic_data, acoustic_data$grts_cell_id == id) %>%
      select(grts_cell_id, survey_start, survey_end, audio_recording_name, auto_id, manual_id)

    # Replace Unconfirmed with NoID in Auto Id field
    ex_grts_df$auto_id = as.character(ex_grts_df$auto_id)
    ex_grts_df$auto_id[is.na(ex_grts_df$auto_id)] = "NoID"

    # Replace Unconfirmed with NoID in Manual Id field
    ex_grts_df$manual_id = as.character(ex_grts_df$manual_id)
    ex_grts_df$manual_id[is.na(ex_grts_df$manual_id)] = "NoID"

    # Edit dates
    ex_grts_df$survey_start = as.Date(ex_grts_df$survey_start)
    ex_grts_df$survey_end = as.Date(ex_grts_df$survey_end)

    survey_dates = unique(ex_grts_df$survey_start)

    project_data = data.frame()

    for (x in c(1:length(survey_dates))){
      date = survey_dates[x]
      night_row = data.frame(project_id = project_id)
      night_row$grts_cell_id = id
      night_row$observed_night = date
      # find all species present and then add them all together
      night_data = subset(ex_grts_df, ex_grts_df$survey_start == date)

      # Adding number of species at each night and adding it to the row
      for (s in species){
        if (acoustic_type == 'auto'){
          species_count = dim(subset(night_data, night_data$auto_id == s))[1]
        }else if (acoustic_type == 'manual'){
          species_count = dim(subset(night_data, night_data$manual_id == s))[1]
        }
        night_row[,s] = species_count
      }

      # bind this row for this date to final dataframe
      if (dim(project_data)[1]==0){
        project_data = night_row
      }else{
        project_data = rbind(project_data,night_row)
      }
    }

    if (dim(nights_df)[1] == 0){
      nights_df = project_data
    }else{
      nights_df = rbind(nights_df, project_data)
    }
  }
  write_csv(nights_df, out_file)
  return (nights_df)
}
