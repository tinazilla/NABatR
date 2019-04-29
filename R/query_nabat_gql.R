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

library(ghql)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)

#' @title NABat GraphicalQL Query Function
#'
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyverse
#' @import DT
#'
#' @description
#' This function Queries the North American Bat Database using a GraphicalQL (GQL) String to extract
#' Stationary Acoustic Data for a single project id. Note: hits 'Production' database with GQL.
#'
#' @param token String to use in header to access GraphicalQL.
#' @param user_name String to use in header to access GraphicalQL.
#' @param project_id Integer to use as an identifier for the project in NABat.
#' @param out_file String to write the CSV out to.  ex: '/path/to/downloads/Raw_Acoustic_GQL.csv'
#' @keywords bats, NABat, GQL, Acoustic, Stationary
#'
#' @export
query_nabat_gql = function(token,
                           user_name,
                           project_id,
                           out_file){

  cli <- GraphqlClient$new(
    url = 'https://api.sciencebase.gov/nabatmonitoring-survey/graphql',
    headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token),
                                       'X-email-address' = username
    )))

  qry <- Query$new()
  qry$query('counts', paste0('{
  allSurveys (filter :{projectId:{equalTo:',project_id,'}}){
    nodes{
      projectId
      grtsId
      stationaryAcousticEventsBySurveyId {
        nodes{
          id
          location
          surveyId
          activationStartTime
          activationEndTime
          deviceByDeviceId{
            deviceTypeByDeviceTypeId{
              manufacturer
              model
            }
          }
          microphoneByMicrophoneId{
            microphoneTypeByMicrophoneTypeId{
              model
              manufacturer
            }
          }
          microphoneOrientationByMicrophoneOrientationId{
            shortName
          }
          microphoneHeight
          distanceToClutterMeters
          clutterTypeByClutterTypeId{
            description
          }
          distanceToWater
          waterType
          percentClutterMethod
          habitatTypeByHabitatTypeId{
            description
          }
          stationaryAcousticValuesBySaSurveyId{
            nodes{
              wavFileName
              softwareBySoftwareId{
                name
                versionNumber
              }
              speciesBySpeciesId{
                speciesCode
              }
              speciesByManualId{
                speciesCode
              }
              }
            }
          }
        }
      }
    }
  }'))

  qry$queries$counts

  ac_dat = cli$exec(qry$queries$counts)
  ac_df  = fromJSON(ac_dat, flatten = TRUE)
  ac_df

  # Temporary dataframe that has the project ID and grts ID and the associated Dataframe with
  tmp_df = data.frame(ac_df$data$allSurveys$nodes)
  tmp_df_len = dim(tmp_df)[1]

  # Removing fields we don't want
  raw_df = as_tibble(tmp_df) %>% unnest() %>% unnest %>% as.data.frame() %>%
    subset(select= -c(speciesBySpeciesId, speciesByManualId, habitatTypeByHabitatTypeId, deviceByDeviceId))

  # Write out raw data csv
  write_csv(raw_df, out_file)
  raw_df1 = read_csv(raw_out_file)
  # read_csv uses desired field types to return
  return (raw_df1)
}
