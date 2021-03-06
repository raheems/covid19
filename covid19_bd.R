# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial and official

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)
library(scales)
library(gsheet)

# sheet_3 = 'https://docs.google.com/spreadsheets/d/1nlAQffAvqChLtvGiJvJPnNOJJKBu_uzmnKdpAJXuPwM/edit#gid=336445634'

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
    df <- read_csv('covid19_bd.csv', skip = 1)
  }

)

# names(df)

df <- df %>%
  rename(
    date = Date
    , daily_deaths_govt = "দৈনিক মৃত্যু (সরকারি)"
    , daily_deaths_suspected = "দৈনিক মৃত্যু (বেসরকারি)"
    , total_tests = `Total Tests`
      ) %>%
  select(
    date, daily_deaths_govt, daily_deaths_suspected, total_tests
  ) %>%
  mutate(
    daily_deaths_govt = replace_na(as.numeric(daily_deaths_govt), 0)
    , daily_deaths_suspected = replace_na(as.numeric(daily_deaths_suspected), 0)
    , total_tests = replace_na(as.numeric(total_tests), 0)
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
    , cum_change_deaths_govt= cumsum(change_deaths_govt)
    , cum_change_deaths_susp = cumsum(change_deaths_susp)
  ) %>%
  arrange(
    date
  )

df_ <- df %>%
  gather(varname, cum_cases, cum_change_deaths_govt, cum_change_deaths_susp,
         cum_deaths_govt, cum_deaths_suspected, factor_key=TRUE)

data_last_refreshed = max(df_$date)

#############################
# Second data source
#############################

source2 = "https://docs.google.com/spreadsheets/d/1irrhpkFxx7zA0DxqYHDYgbwDwFyITbdkLQnyO4P688I/edit#gid=0"
df2 = gsheet2tbl(source2)



