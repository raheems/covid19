# Wed Apr  1 18:38:47 2020 ------------------------------


# RUN covid_update_data.R to update the data
us_state_data <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
df <- read_csv(us_state_data)

df <- df %>%
  arrange(state, date) %>%
  mutate(
    daily_cases = c(0, diff(cases))
    , daily_deaths = c(0, diff(deaths))
  )

ggplot(df, aes(x= lag(daily_cases, 6), y = daily_deaths ))+
  geom_point() + geom_smooth()


state_list = c("New York", "New Jersey")
df_ny <- df %>% filter(state %in% state_list) %>%
 arrange(state, date) %>%
  mutate(
    daily_cases = c(0, diff(cases))
    , daily_deaths = c(0, diff(deaths))
  )

df %>%
  gather(metric, n, daily_cases, daily_deaths, factor_key = TRUE) %>%
  ggplot(aes(x=date, y=n, color=metric)) +
  geom_line() + geom_smooth()

ggplot(df_ny, aes(x= lag(daily_cases, 1), y = daily_deaths ))+
  geom_point() + geom_smooth()

