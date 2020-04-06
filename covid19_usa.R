# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)
library(gsheet)
library(scales)


# RUN covid_update_data.R to update the data
us_state_data <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
df <- read_csv(us_state_data)


usa <- df %>%
  rename(
    cum_cases = cases
    , cum_deaths = deaths
  ) %>%
  arrange(state, date)

data_last_refreshed = max(usa$date)
data_last_refreshed

