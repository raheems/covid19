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


state_list = c("New York", "New Jersey")
df_ny <- df %>% filter(state %in% state_list) %>%
 arrange(state, date) %>%
  mutate(
    daily_cases = c(0, diff(cases))
    , daily_deaths = c(0, diff(deaths))
  )

df_ny %>%
  gather(metric, n, daily_cases, daily_deaths, factor_key = TRUE) %>%
  ggplot(aes(x=date, y=n, color=metric)) +
  geom_line() + geom_smooth()

ggplot(df_ny, aes(x= lag(daily_cases, 8), y = daily_deaths ))+
  geom_point() + geom_smooth()

summary(df_ny$daily_cases)
boxplot(df_ny$daily_cases)


# Recovery to death ratio
filter(country %in% c('US', 'Bangladesh'))

a <- df %>%
  mutate(
    daily_recov = c(0, diff(cum_recov))
    , daily_deaths = c(0, diff(cum_deaths))
    , daily_recov_death_ratio = daily_recov/daily_deaths
  )


%>%
  group_by(
    date
  ) %>%
  summarise(
    cum_recov_death_ratio = sum(cum_recov_death_ratio, na.rm = TRUE)
    , daily_recov_death_ratio = sum(daily_recov_death_ratio, na.rm = TRUE)
  ) %>%
  gather(metric, n, cum_recov_death_ratio, daily_recov_death_ratio, factor_key = TRUE)

ggplot(a, aes(x=date, y = n, color=metric)) +
  geom_line()


# Moving average plot

library(zoo)

# Recovery to death ratio
ctry = c('US', 'Bangladesh')

a <- df %>% filter(country %in% ctry) %>%
  mutate(
    cum_recov = c(0, diff(cum_recov))
    , cum_deaths = c(0, diff(cum_deaths))
    # , daily_recov_death_ratio = daily_recov/daily_deaths
  ) %>%
  gather(metric, n, cum_recov, cum_deaths, factor_key = TRUE)


a <- df %>%
  mutate(
    daily_confirmed = c(0, diff(cum_confirmed))
    , daily_recov = c(0, diff(cum_recov))
    , daily_deaths = c(0, diff(cum_deaths))
  ) %>%
  group_by(date) %>%
  summarise(
    daily_confirmed = sum(daily_confirmed)
    , daily_recov = sum(daily_recov)
    , daily_deaths = sum(daily_deaths)
    , daily_recov_death_ratio = (daily_recov/daily_deaths)
    # , daily_recov_death_ratio = daily_recov/daily_deaths
  ) %>%
  gather(metric, n, daily_recov, daily_confirmed, daily_deaths, factor_key = TRUE)


ggplot(a, aes(x=date, y=n, color=metric))+
  # geom_point(aes(y=rollmean(n, 7, na.pad=TRUE))) +
  geom_point() +
  geom_smooth()
