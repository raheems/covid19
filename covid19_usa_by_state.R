# Mon Mar 30 19:05:52 2020 ------------------------------
# COVID incidences and deaths
# USA by state

# Source:
# https://github.com/nytimes/covid-19-data

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)



nyt_us_states <- paste("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", sep = "")
nyt_us_states <- read_csv(nyt_us_states)


# Create working data set excluding China
df <- nyt_us_states %>%
  group_by(state, date) %>%
  summarize(
    cum_cases = sum(cases),
    cum_deaths = sum(deaths)
  ) %>%
  top_n(10,cum_deaths)

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

