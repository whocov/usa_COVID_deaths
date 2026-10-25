# Author: Craig Schultz; adapted from Joao Muianga's USA import
# Purpose: Export USA and England-source GBR deaths to data/usa_deaths.csv.
# Input: retrieve USA and England data directly from their public APIs.
# IMPORTANT: England is a subset of the UK. GBR is the UK ISO alpha-3 code.
# The requested GBR mapping below does NOT turn England counts into UK totals.
# This script uses the supplied England dates as weekly labels and assigns
# each to the Monday in the same week. It does not redistribute daily counts.

required_packages <- c("here", "httr", "jsonlite", "janitor", "dplyr",
                       "lubridate", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) install.packages(missing_packages)
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# 1. Configuration ---------------------------------------------------------
england_api_url <- paste0(
  "https://api.ukhsa-dashboard.data.gov.uk/",
  "themes/infectious_disease/sub_themes/respiratory/topics/COVID-19/",
  "geography_types/Nation/geographies/England/",
  "metrics/COVID-19_deaths_ONSRegByWeek"
)
usa_first_iso_year <- 2023L  # Original USA filter: ISO_YEAR > 2022
england_start_date <- as.Date("2021-01-01")
# Include provisional observations, matching the supplied all-week USA export.
# Set TRUE to omit England rows explicitly flagged as in reporting delay.
exclude_england_reporting_delay <- FALSE
output_file <- here::here("data", "usa_deaths.csv")

output_columns <- c("COUNTRY_NAME", "CASES_DEATHS", "epiweek", "ISO_YEAR",
                    "ISO_WEEK", "WHO_REGION", "ISO_3_CODE", "SEX",
                    "AGEGROUP", "AGEGROUP_NUM")

parse_source_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  # ISO dates and ISO datetime strings; ambiguous date formats are rejected.
  suppressWarnings(as.Date(substr(trimws(as.character(x)), 1, 10),
                           format = "%Y-%m-%d"))
}

validate_totals <- function(x, source) {
  if (!nrow(x)) stop(source, ": no matching observations.")
  if (anyNA(x$epiweek)) stop(source, ": missing or invalid dates.")
  # Keep genuinely missing/suppressed counts as NA; never replace with zero.
  if (any(!is.na(x$CASES_DEATHS) &
          (!is.finite(x$CASES_DEATHS) | x$CASES_DEATHS < 0 |
           x$CASES_DEATHS != floor(x$CASES_DEATHS)))) {
    stop(source, ": invalid death counts.")
  }
  duplicates <- x %>% count(ISO_3_CODE, epiweek, SEX, AGEGROUP) %>% filter(n > 1)
  if (nrow(duplicates)) {
    stop(source, ": conflicting/multiple totals for the same week; inspect source.")
  }
  x
}

# 2. USA: CDC weekly total deaths ------------------------------------------
cdc_url <- "https://data.cdc.gov/resource/r8kw-7aab.json"
response <- httr::RETRY(
  "GET", cdc_url,
  query = list(`$where` = "state = 'United States'",
               `$limit` = 50000, `$order` = "week_ending_date DESC"),
  httr::timeout(120), times = 3, quiet = TRUE
)
httr::stop_for_status(response)
usa_deaths <- jsonlite::fromJSON(
  httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE
) %>% janitor::clean_names()
if (!all(c("state", "week_ending_date", "covid_19_deaths") %in% names(usa_deaths))) {
  stop("CDC response does not contain the expected columns.")
}
if (nrow(usa_deaths) >= 50000) stop("CDC row limit reached; add pagination.")

linelist_usa <- usa_deaths %>%
  filter(state == "United States") %>%
  transmute(
    COUNTRY_NAME = "UNITED STATES OF AMERICA",
    CASES_DEATHS = as.numeric(covid_19_deaths),
    epiweek = as.Date(floor_date(parse_source_date(week_ending_date),
                                 unit = "week", week_start = 1)),
    ISO_YEAR = isoyear(epiweek),
    ISO_WEEK = isoweek(epiweek),
    WHO_REGION = "AMRO", ISO_3_CODE = "USA",
    SEX = "All", AGEGROUP = "All", AGEGROUP_NUM = 0L
  ) %>% distinct()
# Filter before validation, matching the original missing-date handling.
linelist_usa <- linelist_usa %>%
  filter(ISO_YEAR >= usa_first_iso_year)
linelist_usa <- validate_totals(linelist_usa, "USA")

# 3. England: UKHSA API, mapped to GBR -------------------------------------
# Documentation: https://ukhsa-dashboard.data.gov.uk/access-our-data/examples
# Matches the CSV metric COVID-19_deaths_ONSRegByWeek, Nation = England.
# No date cutoff: every run retrieves the available history and revisions.
get_ukhsa_data <- function(url) {
  next_url <- httr::modify_url(
    url, query = list(format = "json", page_size = 365,
                      sex = "all", age = "all", stratum = "default")
  )
  pages <- list()
  visited <- character()
  repeat {
    if (next_url %in% visited) stop("UKHSA returned a repeated pagination URL.")
    visited <- c(visited, next_url)
    response <- httr::RETRY(
      "GET", next_url, httr::accept_json(), httr::timeout(120),
      times = 3, quiet = TRUE
    )
    httr::stop_for_status(response)
    payload <- jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    if (!is.list(payload) || is.null(payload$results)) {
      stop("Unexpected UKHSA response: missing results field.")
    }
    if (length(payload$results)) {
      if (!is.data.frame(payload$results)) {
        stop("Unexpected UKHSA results structure.")
      }
      pages[[length(pages) + 1L]] <- payload$results
    }
    next_page <- payload[["next"]]
    if (is.null(next_page) || !length(next_page) ||
        is.na(next_page) || !nzchar(next_page)) break
    next_url <- next_page
  }
  result <- dplyr::bind_rows(pages)
  if (!nrow(result)) stop("UKHSA returned no data for the requested metric.")
  result
}

england_raw <- get_ukhsa_data(england_api_url) %>% janitor::clean_names()
required_england <- c("theme", "sub_theme", "topic", "geography_type",
                      "geography", "metric", "sex", "age", "stratum",
                      "date", "metric_value", "in_reporting_delay_period")
if (!all(required_england %in% names(england_raw))) {
  stop("UKHSA response is missing columns: ",
       paste(setdiff(required_england, names(england_raw)), collapse = ", "))
}

england_selected <- england_raw %>%
  mutate(date = parse_source_date(date)) %>%
  filter(
    date >= england_start_date,
    tolower(trimws(theme)) == "infectious_disease",
    tolower(trimws(sub_theme)) == "respiratory",
    tolower(trimws(topic)) == "covid-19",
    tolower(trimws(geography_type)) == "nation",
    tolower(trimws(geography)) == "england",
    metric == "COVID-19_deaths_ONSRegByWeek",
    tolower(trimws(sex)) == "all",
    tolower(trimws(age)) == "all",
    tolower(trimws(stratum)) == "default"
  )

if (exclude_england_reporting_delay) {
  england_selected <- england_selected %>%
    filter(tolower(trimws(as.character(in_reporting_delay_period))) %in%
             c("false", "0"))
}

# Intentionally use the pipeline country identifier requested by the user.
# All GBR observations here are ENGLAND ONLY, deaths by registration week.
# COUNTRY_NAME must match your pipeline's GBR reference table exactly.
linelist_gbr <- england_selected %>%
  transmute(
    COUNTRY_NAME = "THE UNITED KINGDOM",
    CASES_DEATHS = as.numeric(metric_value),
    epiweek = as.Date(floor_date(parse_source_date(date),
                                 unit = "week", week_start = 1)),
    ISO_YEAR = isoyear(epiweek),
    ISO_WEEK = isoweek(epiweek),
    WHO_REGION = "EURO", ISO_3_CODE = "GBR",
    SEX = "All", AGEGROUP = "All", AGEGROUP_NUM = 0L
  ) %>% distinct()
# The source-date cutoff above is independent of the ISO year.
# For example, 1 January 2021 belongs to ISO year 2020 and remains eligible.
linelist_gbr <- linelist_gbr %>%
  filter(!is.na(epiweek))
linelist_gbr <- validate_totals(linelist_gbr, "England / GBR")

warning("GBR rows contain England-only deaths by registration week; ",
        "they are not whole-UK totals.", call. = FALSE)

# 4. Combine using the exact downstream schema -----------------------------
linelist <- bind_rows(linelist_usa, linelist_gbr) %>%
  select(all_of(output_columns)) %>%
  arrange(ISO_3_CODE, desc(epiweek))
if (!all(c("USA", "GBR") %in% linelist$ISO_3_CODE)) {
  stop("Both USA and GBR must have rows after their date filters; export cancelled.")
}
stopifnot(identical(names(linelist), output_columns))

# 5. Export: downstream filename stays usa_deaths.csv -----------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(linelist, output_file, na = "")

# Preserve the original script's legacy second export: USA all-age totals.
# The supplied original code wrote linelist here, not its age-disaggregated data.
# No unused CDC age/sex API request is needed for either all-age export.
readr::write_csv(linelist_usa,
                 here::here("data", "usa_agegroup_deaths.csv"), na = "")

message("Exported USA + England-source GBR: ", output_file)
print(linelist %>% group_by(ISO_3_CODE) %>%
        summarise(rows = n(), first_week = min(epiweek),
                  latest_week = max(epiweek), .groups = "drop"))
print(head(linelist_gbr, 8))
