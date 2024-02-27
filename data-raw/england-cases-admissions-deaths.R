## code to prepare `england` dataset from PHE url.
library(tidyverse)
library(httr)
library(jsonlite)
library(readr)
library(timetk)

# params
date.format <- "%Y-%m-%d"
timeout <- 20

# Get English cases, admissions, deaths and hospitalisations.
url <- paste0(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=",
  "E92000001&metric=cumAdmissions&metric=cumCasesBySpecimenDate&metric=",
  "cumDeaths28DaysByDeathDate&metric=hospitalCases&format=json"
)

# Make call to https://coronavirus.data.gov.uk/ and process response.
response <- httr::VERB("GET", url <- url, httr::timeout(timeout))
if (response$status_code >= 400) {
  err_msg <- httr::http_status(response)
  stop(err_msg)
} else if (response$status_code == 204) {
  response <- NULL
} else {
  # Convert response from binary to JSON:
  json_text <- httr::content(response, "text")
  data <- jsonlite::fromJSON(json_text)
}


england <- data$body %>%
  as_tibble() %>%
  mutate(date, date = parse_date(date, format = date.format)) %>%
  rename(
    cum_admissions=cumAdmissions,
    cum_cases=cumCasesBySpecimenDate,
    cum_deaths=cumDeaths28DaysByDeathDate,
    hospital_cases=hospitalCases
  ) %>%
  select(c(
    'date', 'cum_cases', 'cum_admissions', 'cum_deaths', 'hospital_cases')) %>%
  na.omit() %>%
  tk_xts(select=-date,date_var=date)


# Stick
usethis::use_data(england, overwrite = TRUE, compress = 'xz')

# Quick plot to see recovering what we want.
autoplot(diff(england$cum_cases))