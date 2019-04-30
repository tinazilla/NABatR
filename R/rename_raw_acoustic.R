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
library(DT)

#' @title NABat Rename Raw Acoustic Data Function
#'
#' @import jsonlite
#' @import tidyverse
#' @import DT
#'
#' @description
#' This function takes the raw data output from query_nabat_gql.R and renames all of the headers
#' to match the Bulk upload template for acoustic data on the NABat website.
#'
#' @param raw_data data.frame to use in header to access GraphicalQL.
#' @param out_file String to write the CSV out to.  ex: '/path/to/downloads/Stationary_Acoustic_GQL.csv'
#' @keywords bats, NABat, Acoustic, Stationary, data
#'
#' @export

rename_raw_acoustic = function(raw_data,
                               out_file){

  renaming_df = raw_data
  names(renaming_df)[names(renaming_df) == 'projectId'] = 'project_id'
  names(renaming_df)[names(renaming_df) == 'grtsId'] = 'grts_cell_id'
  names(renaming_df)[names(renaming_df) == 'id'] = 'stationary_acoustic_values_id'
  names(renaming_df)[names(renaming_df) == 'locationName'] = 'location_name'
  names(renaming_df)[names(renaming_df) == 'surveyId'] = 'survey_id'
  names(renaming_df)[names(renaming_df) == 'activationStartTime'] = 'survey_start'
  names(renaming_df)[names(renaming_df) == 'activationEndTime'] = 'survey_end'
  names(renaming_df)[names(renaming_df) == 'microphoneOrientationByMicrophoneOrientationId'] = 'microphone_orientation'
  names(renaming_df)[names(renaming_df) == 'microphoneHeight'] = 'microphone_height'
  names(renaming_df)[names(renaming_df) == 'distanceToClutterMeters'] = 'distance_to_nearest_clutter'
  names(renaming_df)[names(renaming_df) == 'distanceToWater'] = 'distance_to_nearest_water'
  names(renaming_df)[names(renaming_df) == 'waterType'] = 'water_type'
  names(renaming_df)[names(renaming_df) == 'percentClutterMethod'] = 'percent_clutter'
  names(renaming_df)[names(renaming_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.manufacturer'] = 'device_manufacturer'
  names(renaming_df)[names(renaming_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.model'] = 'device_model'
  names(renaming_df)[names(renaming_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.model'] = 'microphone_model'
  names(renaming_df)[names(renaming_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.manufacturer'] = 'microphone_manufacturer'
  names(renaming_df)[names(renaming_df) == 'clutterTypeByClutterTypeId.description'] = 'clutter_type'
  names(renaming_df)[names(renaming_df) == 'habitatTypeByHabitatTypeId.description'] = 'broad_habitat_type'
  names(renaming_df)[names(renaming_df) == 'wavFileName'] = 'audio_recording_name'
  names(renaming_df)[names(renaming_df) == 'softwareBySoftwareId.name'] = 'software_name'
  names(renaming_df)[names(renaming_df) == 'softwareBySoftwareId.versionNumber'] = 'software_version'
  names(renaming_df)[names(renaming_df) == 'speciesBySpeciesId.speciesCode'] = 'auto_id'
  names(renaming_df)[names(renaming_df) == 'speciesByManualId.speciesCode'] = 'manual_id'

  reordered_df = renaming_df[, c(2,1,4,3,5,6,7,8,9,10,
                                 11,12,13,14,15,16,17,
                                 18,19,20,21,22,23,24)]
  write_csv(reordered_df, out_file)
  return (reordered_df)
}
