# Sat Mar 28 19:12:21 2020 ------------------------------
# This code is for confirmed cases -----------

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)
library(gsheet)
library(scales)

# Wed Apr  1 10:29:15 2020 ------------------------------
# Recovered to Deaths ratio

jhu_deaths_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", sep = "")
jhu_deaths_global <- read_csv(jhu_deaths_global_src)

# Recovered
jhu_recovered_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", sep = "")
jhu_recovered_global <- read_csv(jhu_recovered_global_src)


tryCatch(
  expr ={
    # Confirmed
    jhu_confirmed_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", sep = "")
    jhu_confirmed_global <- read_csv(jhu_confirmed_global_src)
    write_csv(jhu_confirmed_global, "jhu_confirmed_global.csv")
  },
  error = function(e){
    message('Caught an error!')
    print(e)
  },
  warning = function(w){
    message('Caugnt an warning!')
    print(w)
  },
  finally = {
    jhu_confirmed_global <- read_csv("jhu_confirmed_global.csv")
  }

)

# Confirmed

df_confirmed <- jhu_confirmed_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "date", values_to = "cum_cases") %>%
  mutate(
    date = mdy(date)
  ) %>%
  select(country, date, cum_cases) %>%
  group_by(country, date) %>%
  summarize(
    cum_cases = sum(cum_cases)
  ) %>%
  rename(cum_confirmed = cum_cases)


# Create working data set excluding China
df_deaths <- jhu_deaths_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "date", values_to = "cum_cases") %>%
  mutate(
    date = mdy(date)
  ) %>%
  select(country, date, cum_cases) %>%
  group_by(country, date) %>%
  summarize(
    cum_cases = sum(cum_cases)
  ) %>%
  rename(cum_deaths = cum_cases)


# Create working data set excluding China
df_recov <- jhu_recovered_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "date", values_to = "cum_cases") %>%
  mutate(
    date = mdy(date)
  ) %>%
  select(country, date, cum_cases) %>%
  group_by(country, date) %>%
  summarize(
    cum_cases = sum(cum_cases)
  ) %>%
  rename(cum_recov = cum_cases)

df <- left_join(df_deaths, df_recov, by = c("country", "date"))
df <- left_join(df, df_confirmed, by = c("country", "date"))

df <- df %>%
  mutate(
    daily_confirmed = c(0, diff(cum_confirmed))
    , daily_recov = c(0, diff(cum_recov))
    , daily_deaths = c(0, diff(cum_deaths))
    , cum_recov_death_ratio = cum_recov/cum_deaths
  )


# Last date updated or last reported date
data_last_refreshed =  max(ymd(df$date))

# df %>% filter(country == 'United Kingdom') %>%
#   group_by(country) %>%
#   summarise(
#     recov = max(cum_recov)
#     , deaths = max(cum_deaths)
#   )

