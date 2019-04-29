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
  names(renaming_df)[names(renaming_df) == 'projectId'] = 'Project_ID'
  names(renaming_df)[names(renaming_df) == 'grtsId'] = 'GRTS_Cell_ID'
  names(renaming_df)[names(renaming_df) == 'id'] = 'Stationary_Acoustic_Values_ID'
  names(renaming_df)[names(renaming_df) == 'location'] = 'Location_Name'
  names(renaming_df)[names(renaming_df) == 'surveyId'] = 'Survey_ID'
  names(renaming_df)[names(renaming_df) == 'activationStartTime'] = 'Survey_Start'
  names(renaming_df)[names(renaming_df) == 'activationEndTime'] = 'Survey_End'
  names(renaming_df)[names(renaming_df) == 'microphoneOrientationByMicrophoneOrientationId'] = 'Microphone_Orientation'
  names(renaming_df)[names(renaming_df) == 'microphoneHeight'] = 'Microphone_Height'
  names(renaming_df)[names(renaming_df) == 'distanceToClutterMeters'] = 'Distance_to_Nearest_Clutter'
  names(renaming_df)[names(renaming_df) == 'distanceToWater'] = 'Distance_to_Nearest_Water'
  names(renaming_df)[names(renaming_df) == 'waterType'] = 'Water_Type'
  names(renaming_df)[names(renaming_df) == 'percentClutterMethod'] = 'Percent_Clutter'
  names(renaming_df)[names(renaming_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.manufacturer'] = 'Device_Manufacturer'
  names(renaming_df)[names(renaming_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.model'] = 'Device_Model'
  names(renaming_df)[names(renaming_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.model'] = 'Microphone_Model'
  names(renaming_df)[names(renaming_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.manufacturer'] = 'Microphone_Manufacturer'
  names(renaming_df)[names(renaming_df) == 'clutterTypeByClutterTypeId.description'] = 'Clutter_Type'
  names(renaming_df)[names(renaming_df) == 'habitatTypeByHabitatTypeId.description'] = 'Broad_Habitat_Type'
  names(renaming_df)[names(renaming_df) == 'wavFileName'] = 'Audio_Recording_Name'
  names(renaming_df)[names(renaming_df) == 'softwareBySoftwareId.name'] = 'Software_Name'
  names(renaming_df)[names(renaming_df) == 'softwareBySoftwareId.versionNumber'] = 'Software_Version'
  names(renaming_df)[names(renaming_df) == 'speciesBySpeciesId.speciesCode'] = 'Auto_ID'
  names(renaming_df)[names(renaming_df) == 'speciesByManualId.speciesCode'] = 'Manual_ID'

  reordered_df = renaming_df[, c(2,1,4,3,5,6,7,8,9,10,
                                 11,12,13,14,15,16,17,
                                 18,19,20,21,22,23,24,
                                 25,26)]
  write_csv(reordered_df, out_file)
  return (reordered_df)
}
