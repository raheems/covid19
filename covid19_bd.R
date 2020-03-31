# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial

# install.packages('gsheet')
library(gsheet)
library(tidyverse)
library(plotly)

# RUN covid_update_data.R to update the data

df <- read_csv('covid19_bd.csv')

# names(df)

df <- df %>%
  rename(
    date = "তারিখ"
    , daily_deaths_govt = "দৈনিক মৃত্যু (সরকারি)"
    , daily_deaths_suspected = "দৈনিক মৃত্যু (বেসরকারি)"
    , total_tests = `Total Tests`
      ) %>%
  select(
    date, daily_deaths_govt, daily_deaths_suspected, total_tests
  ) %>%
  drop_na(date) %>%
  mutate(
    date = as.Date(date, "%d/%m/%Y")
    , change_deaths_govt = daily_deaths_govt - lag(daily_deaths_govt)
    , change_deaths_govt = ifelse(is.na(change_deaths_govt), 0, change_deaths_govt)

    , change_deaths_susp = daily_deaths_suspected - lag(daily_deaths_suspected)
    , change_deaths_susp = ifelse(is.na(change_deaths_susp), 0, change_deaths_susp)
    , cum_deaths_govt = cumsum(daily_deaths_govt)
    , cum_deaths_suspected = cumsum(daily_deaths_suspected)
    , cum_tests = cumsum(total_tests)
    , cum_change_deaths_govt= cumsum(change_deaths_govt)
    , cum_change_deaths_susp = cumsum(change_deaths_susp)
  ) %>%
  arrange(
    date
  ) %>%
  gather(varname, cum_cases, cum_change_deaths_govt, cum_change_deaths_susp,
         cum_deaths_govt, cum_deaths_suspected, cum_tests,
         factor_key=TRUE)

data_last_refreshed = max(df$date)



