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
species_df = read_csv(paste0(project_location, './Examples/data/bat_species_table.csv'))
species = species_df$species_code[1:53]
species

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
#' @export
build_nights_df = function(acoustic_data,
                           out_file,
                           acoustic_type){

  nights_df = data.frame()
  project_id = unique(acoustic_data$Project_ID)
  # Extract all GRTS Cell ids within acoustic_data
  GRTS_ids = unique(acoustic_data$GRTS_Cell_ID)

  for (id in GRTS_ids){
    print (paste0('id: ',id))

    ex_grts_df = subset(acoustic_data, acoustic_data$GRTS_Cell_ID == id) %>%
      select(GRTS_Cell_ID, Survey_Start, Survey_End, Audio_Recording_Name, Auto_ID, Manual_ID)

    # Replace Unconfirmed with NoID in Auto Id field
    ex_grts_df$Auto_ID = as.character(ex_grts_df$Auto_ID)
    ex_grts_df$Auto_ID[is.na(ex_grts_df$Auto_ID)] = "NoID"

    # Replace Unconfirmed with NoID in Manual Id field
    ex_grts_df$Manual_ID = as.character(ex_grts_df$Manual_ID)
    ex_grts_df$Manual_ID[is.na(ex_grts_df$Manual_ID)] = "NoID"

    # Edit dates
    ex_grts_df$Survey_Start = as.Date(ex_grts_df$Survey_Start)
    ex_grts_df$Survey_End = as.Date(ex_grts_df$Survey_End)

    print (paste0('number of entries: ',dim(ex_grts_df)[1]))

    survey_dates = unique(ex_grts_df$Survey_Start)


    project_data = data.frame()

    for (x in c(1:length(survey_dates))){
      date = survey_dates[x]
      print (date)
      night_row = data.frame(Project_ID   = project_id)
      night_row$GRTS_Cell_ID = id
      night_row$Observed_Night = date
      # display number of records for this night
      print (dim(subset(ex_grts_df, ex_grts_df$Survey_Start == date))[1])
      # find all species present and then add them all together
      night_data = subset(ex_grts_df, ex_grts_df$Survey_Start == date)

      # Adding number of species at each night and adding it to the row
      for (s in species){
        if (acoustic_type == 'auto'){
          species_count = dim(subset(night_data, night_data$Auto_ID == s))[1]
        }else if (acoustic_type == 'manual'){
          species_count = dim(subset(night_data, night_data$Manual_ID == s))[1]
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
      print ('start over')
      nights_df = project_data
    }else{
      print('after')
      nights_df = rbind(nights_df, project_data)
    }
  }
  write_csv(nights_df, out_file)
  return (nights_df)
}
