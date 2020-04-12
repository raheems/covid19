# Sun Apr  5 16:16:34 2020 ------------------------------

# Unified data set for global cases
# This includes Banglaedesh unofficial data

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)
library(gsheet)
library(scales)


# Confirmed cases --------------

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

# Deaths-----

tryCatch(
  expr ={
    # Confirmed
    jhu_deaths_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", sep = "")
    jhu_deaths_global <- read_csv(jhu_deaths_global_src)
    write_csv(jhu_deaths_global, "jhu_deaths_global.csv")
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
    jhu_deaths_global <- read_csv("jhu_deaths_global.csv")
  }

)

# Recovered cases

tryCatch(
  expr ={
    # Recovered
    jhu_recovered_global_src <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", sep = "")
    jhu_recovered_global <- read_csv(jhu_recovered_global_src)
    write_csv(jhu_recovered_global, "jhu_recovered_global.csv")
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
    jhu_recovered_global <- read_csv("jhu_recovered_global.csv")
  }

)

# Bangladesh unofficial data ------


tryCatch(
  expr ={
    # Update Bangladesh unofficial and official data
    sheet_4 = 'https://docs.google.com/spreadsheets/d/1nlAQffAvqChLtvGiJvJPnNOJJKBu_uzmnKdpAJXuPwM/edit#gid=336445634'
    df_gogle_sheet = gsheet2tbl(sheet_4)
    write_csv(df_gogle_sheet, 'covid19_bd.csv')

  },
  error = function(e){
    message('Caught an error!')
    print(e)
  },
  warning = function(w){
    message('Caught an warning!')
    #print(w)
  },
  finally = {
    df_bd_unoff <- read_csv('covid19_bd.csv', skip = 1)
  }

)


# Confirmed
global_confirmed <- jhu_confirmed_global %>%
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
  )


# Create working data set excluding China
global_deaths <- jhu_deaths_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "date", values_to = "cum_deaths") %>%
  mutate(
    date = mdy(date)
  ) %>%
  select(country, date, cum_deaths) %>%
  group_by(country, date) %>%
  summarize(
    cum_deaths = sum(cum_deaths)
  )


# Create working data set excluding China
global_recov <- jhu_recovered_global %>%
  rename(province = "Province/State",
         country = "Country/Region") %>%
  pivot_longer(-c(province, country, Lat, Long),
               names_to = "date", values_to = "cum_recov") %>%
  mutate(
    date = mdy(date)
  ) %>%
  select(country, date, cum_recov) %>%
  group_by(country, date) %>%
  summarize(
    cum_recov = sum(cum_recov)
  )

global <- left_join(global_deaths, global_confirmed, by = c("country", "date"))
global <- left_join(global, global_recov, by = c("country", "date"))

# Join Bangladesh (Unoff) data to it

bd_unoff <- df_bd_unoff %>%
  mutate(
    country = "Bangladesh(unoff)"
  ) %>%
  rename(
    date = Date
    , daily_deaths_govt = "দৈনিক মৃত্যু (সরকারি)"
    , daily_deaths_suspected = "দৈনিক মৃত্যু (বেসরকারি)"
    , total_tests = `Total Tests`
    , daily_cases = `Daily new cases`
  ) %>%
  select(
    country, date, daily_deaths_suspected, daily_cases
  ) %>%
  drop_na(date) %>%
  mutate(
    date = as.Date(date, "%d/%m/%Y")
    , daily_deaths_suspected = replace_na(as.numeric(daily_deaths_suspected), 0)
    , cum_deaths = cumsum(daily_deaths_suspected)
    , cum_cases = cumsum(daily_cases)
    , cum_recov = NA
  ) %>%
  select(country, date, cum_deaths, cum_cases, cum_recov)

head(bd_unoff)
head(global)

# Global combined data set ----------
# global <- rbind(as.data.frame(global), as.data.frame(bd_unoff))

global <- global %>%
  group_by(country) %>%
  mutate(
    daily_cases = c(0, diff(cum_cases))
    , daily_deaths = c(0, diff(cum_deaths))
    , daily_recov = c(0, diff(cum_recov))

    # Replace NA
    , daily_cases = replace_na(daily_cases, 0)
    , daily_deaths = replace_na(daily_deaths, 0)
    , daily_recov = replace_na(daily_recov, 0)
    , cum_cases = replace_na(cum_cases, 0)
    , cum_deaths = replace_na(cum_deaths, 0)
    , cum_recov = replace_na(cum_recov, 0)
  )


# summary(df)

# head(global)
#
# df %>%
#   filter(country == 'Bangladesh(unoff)') %>%
#   head()

# summary(global)
# summary(df)

# Last date updated or last reported date
data_last_refreshed =  max(ymd(global$date))

# Additional custom function
# To print the default color pallette

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

global %>%
  filter(country == 'Bangladesh(unoff)')
