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

# Update JHU Confirmed cases data ------------

jhu_confirmed_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", sep = "")
jhu_confirmed_global <- read_csv(jhu_confirmed_global_src)
write_csv(jhu_confirmed_global, "jhu_confirmed_global.csv")

# Create working data set excluding China
df <- jhu_confirmed_global %>%
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

# Last date updated or last reported date
data_last_refreshed =  max(ymd(df$Date))

# Confirmed for Bangladesh
df_bd <- jhu_confirmed_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  filter(
    country=='Bangladesh'
  ) %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "Date", values_to = "cum_cases") %>%
  mutate(
    Date = mdy(Date)
  ) %>%
  select(country, Date, cum_cases) %>%
  arrange(
    country, Date
  ) %>%
  group_by(country, Date) %>%
  summarize(
    cum_cases = sum(cum_cases)
  )


# Top 10 countries
n_world <- df %>%
  group_by(country) %>%
  summarise(
    max_n_cases = max(cum_cases)
  ) %>%
  arrange(desc(max_n_cases)) %>%
  top_n(5, max_n_cases)

# Top 10 countries
n_bd <- df_bd %>%
  group_by(country) %>%
  summarise(
    max_n_cases = max(cum_cases)
  )


# Combined n countries
n <- rbind(n_world, n_bd)

# Reset the origin to the first reported cases
case_summary <- df %>%
  filter(country %in% n$country, cum_cases >0) %>%
  arrange(country, Date, cum_cases) %>%
  group_by(country) %>%
  mutate(
   Day = row_number()
  )

