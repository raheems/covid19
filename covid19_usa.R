# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial

# install.packages('gsheet')
library(tidyverse)


# RUN covid_update_data.R to update the data
us_state_data <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
df <- read_csv(us_state_data)


df <- df %>%
  rename(
    cum_cases = cases
    , cum_deaths = deaths
  ) %>%
  arrange(state, date)

data_last_refreshed = max(df$date)
data_last_refreshed

