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

# Update JHU Confirmed cases data ------------

jhu_confirmed_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", sep = "")
jhu_confirmed_global <- read_csv(jhu_confirmed_global_src)

jhu_deaths_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", sep = "")
jhu_deaths_global <- read_csv(jhu_deaths_global_src)


# Create working data set excluding China
global_cases <- jhu_confirmed_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  # Exclude China
  filter(
    country != 'China'
  ) %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "Date", values_to = "cum_cases") %>%
  mutate(
    Date = mdy(Date)
  ) %>%
  select(country, Date, cum_cases) %>%
  group_by(country, Date) %>%
  summarize(
    cum_cases = sum(cum_cases)
  )


# Create working data set excluding China
global_deaths <- jhu_deaths_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  # Exclude China
  filter(
    country != 'China'
  ) %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "Date", values_to = "cum_cases") %>%
  mutate(
    Date = mdy(Date)
  ) %>%
  select(country, Date, cum_cases) %>%
  group_by(country, Date) %>%
  summarize(
    cum_deaths = sum(cum_cases)
  )

# Final global data set
global <- left_join(global_cases, global_deaths, by = c("country", "Date"))

# Function to calculate the lag-x percent changes
# March 31, 2020
global_gr_lagx <- function(df, country_list, lagx){

  a <- df %>%
    filter(country %in% country_list, cum_deaths >0) %>%
    group_by(country) %>%
    mutate(
      day = row_number()
      , case_gr_rate = (cum_cases - lag(cum_cases, lagx))/lag(cum_cases, lagx) * 100
      , death_gr_rate = (cum_deaths - lag(cum_deaths, lagx))/lag(cum_deaths, lagx) * 100
    ) %>%
    #filter(case_gr_rate <= 200,  death_gr_rate <= 200) %>%
    select(country, day, Date, case_gr_rate, death_gr_rate) %>%
    gather(metric, n, case_gr_rate, death_gr_rate, factor_key=TRUE)

  p <- ggplot(a, aes(x = day, y = n, color=metric)) +
    geom_line(size=1.1) +
    # geom_text_repel(size=5, aes(label=metric),
    #                 function(a) a[ymd(a$date) == max(a$date), ])+
    theme_minimal(base_size = 14)
  p
}


