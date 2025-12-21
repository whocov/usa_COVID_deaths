# Author: Joao Muianga (muiangaj@who.int)
# Date: 2023-12-14
# Purpose: import USA deaths data from CDC website

########################################################
# Import package
pacman::p_load(here, rio, janitor, lubridate, tidyverse, httr, jsonlite, gtsummary)


library(dplyr)

# import dataset with total deaths cases by week
path <- "https://data.cdc.gov/resource/r8kw-7aab.json?$limit=50000"  # prepare request and take first 50000 rows
request <- GET(url = path)                                           # get path
request$status_code                                                   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
usa_deaths <- base::rawToChar(request$content)
usa_deaths <- jsonlite::fromJSON(usa_deaths, flatten = TRUE)


# import dataset with deaths cases by agegroup disaggregation
path_age_sex <- "https://data.cdc.gov/resource/vsak-wrfu.json?$limit=50000"  # prepare request and take first 50000 rows
request <- GET(url = path_age_sex)                                           # get path
request$status_code                                                   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
usa_deaths_agegroup <- base::rawToChar(request$content)
usa_deaths_agegroup <- jsonlite::fromJSON(usa_deaths_agegroup, flatten = TRUE)


# data cleaning
linelist <- usa_deaths %>%
  clean_names() %>%
  select(COUNTRY_NAME = state, week_ending_date, CASES_DEATHS = covid_19_deaths) %>%                          # select desired columns
  mutate(COUNTRY_NAME= 
         recode(COUNTRY_NAME, 
                "United States" = "UNITED STATES OF AMERICA"))%>%       # recode usa name
  distinct()%>%                                                        # remove duplication
  mutate(week_ending_date        = as.Date(week_ending_date),                           # convert end_date in date
         CASES_DEATHS = as.numeric(CASES_DEATHS))%>%              # convert deaths in numeric value
  mutate(epiweek = floor_date(                                          # create the epiweek column
                              week_ending_date,
                              unit = "week",
                              week_start = 1))%>%
  mutate(ISO_YEAR = year(epiweek),                                          # create  year column
         ISO_WEEK = week(epiweek),                                   # create  week column
         WHO_REGION  = "AMRO",                                          # create WHO_region column
         ISO_3_CODE  = "USA",
         SEX = "All",
         AGEGROUP = "All",
         AGEGROUP_NUM = 0)%>%                                        # create ISO Code column
  select(-week_ending_date)%>%
  
  filter(COUNTRY_NAME == "UNITED STATES OF AMERICA")%>%  # filter only United States
  filter(ISO_YEAR > "2022")




## data clean agegroup dataset
linelist_agegroup <- usa_deaths_agegroup%>%
  janitor::clean_names()%>%
  select(state, week_ending_date, sex, age_group, covid_19_deaths)%>%
  mutate(state = recode(state, "United States" = "UNITED STATES OF AMERICA"))%>%
  distinct()%>%
  mutate(week_ending_date = as.Date(week_ending_date),
         covid_19_deaths  = as.numeric(covid_19_deaths))%>%
  mutate(epiweek = floor_date(                                          # create the epiweek column
    week_ending_date,
    unit = "week",
    week_start = 1))%>%
  mutate(year = year(epiweek),                                          # create  year column
         ISO_WEEK = week(epiweek),                                   # create  week column
         WHO_REGION  = "AMRO",                                          # create WHO_region column
         ISO_3_CODE  = "USA",
         SEX = "All",
         AGEGROUP = "All",
         AGEGROUP_NUM = 0
         )%>%                                        # create ISO Code column
  select(-week_ending_date)

linelist_agegroup%>%group_by(state, epiweek, age_group, sex)%>%
  summarise(deaths = sum(covid_19_deaths, na.rm = T))%>%view()


# split agegroup into two columns
linelist_agegroup_1 <- linelist_agegroup%>%
  separate(age_group, into = c("agegroup_num", "agegroup_year", "other"), sep = " ", extra = "merge")

# all sex and unknown

# New  column with WHO agegroup
linelist_agegroup_2 <- linelist_agegroup_1%>%
  mutate(age_group = case_when(
    agegroup_num == "Under" ~ "Deaths04",
    agegroup_num == "1-4"   ~ "Deaths04",
    agegroup_num == "5-14"  ~ "Deaths0514",
    agegroup_num == "15-24" ~ "Deaths1564",
    agegroup_num == "25-34" ~ "Deaths1564",
    agegroup_num == "35-44" ~ "Deaths1564",
    agegroup_num == "45-54" ~ "Deaths1564",
    agegroup_num == "55-64" ~ "Deaths1564",
    agegroup_num == "65-74" ~ "Deaths65plus",
    agegroup_num == "75-84" ~ "Deaths65plus",
    agegroup_num == "85" ~ "Deaths65plus",
    agegroup_num == "All" ~ "All",
    is.na(agegroup_num) ~ "Unknown"
  ))

# select interested columns
deaths_agegroup <- linelist_agegroup_2%>%
  select(WHO_REGION, ISO_3_CODE, country_name = state, year, ISO_WEEK, epiweek, covid_19_deaths, age_group)

## pivot data wider


deaths_agegroup_1 <- deaths_agegroup%>%
  mutate(covid_19_deaths = as.numeric(covid_19_deaths),
         age_group       = as.character(age_group))%>%
  filter(age_group != "All" & covid_19_deaths> 0)%>%
  select(- ISO_WEEK)%>%
  distinct()

deaths_agegroup_piv <- deaths_agegroup_1%>%
  pivot_wider(
    names_from = age_group,
    values_from = covid_19_deaths)

# join agegroup and aggregated deaths tables

export(linelist, here("data", "usa_agegroup_deaths.csv"))
export(linelist, here("data", "usa_deaths.csv"))


## age_group
## age_group_num
## sex









