# Author: Joao Muianga (muiangaj@who.int)
# Date: 2023-12-14
# Purpose: import USA deaths data from CDC website

########################################################
# Import package
pacman::p_load(here, rio, janitor, lubridate, tidyverse, httr, jsonlite, gtsummary)

# import dataset 
path <- "https://data.cdc.gov/resource/r8kw-7aab.json?$limit=50000"  # prepare request and take first 50000 rows
request <- GET(url = path)                                           # get path
request$status_code                                                   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
usa_deaths <- base::rawToChar(request$content)
usa_deaths <- jsonlite::fromJSON(usa_deaths, flatten = TRUE)


# data cleaning
linelist <- usa_deaths %>%
  clean_names() %>%
  select(state, week_ending_date, covid_19_deaths) %>%                          # select desired columns
  mutate(state= 
         recode(state, 
                "United States" = "UNITED STATES OF AMERICA"))%>%       # recode usa name
  distinct()%>%                                                        # remove duplication
  mutate(week_ending_date        = as.Date(week_ending_date),                           # convert end_date in date
         covid_19_deaths = as.numeric(covid_19_deaths))%>%              # convert deaths in numeric value
  mutate(epiweek = floor_date(                                          # create the epiweek column
                              week_ending_date,
                              unit = "week",
                              week_start = 1))%>%
  mutate(year = year(epiweek),                                          # create  year column
         week_number = week(epiweek),                                   # create  week column
         WHO_REGION  = "AMRO",                                          # create WHO_region column
         ISO_3_CODE  = "USA")%>%                                        # create ISO Code column
  
  filter(state == "UNITED STATES OF AMERICA" & week_ending_date != is.na(week_ending_date))%>%  # filter only United States
  filter(year > "2022")

export(linelist, here("data", "usa_deaths.csv"))












