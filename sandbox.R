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




country_list = c("Bangladesh", "US", "United Kingdom", "Canada", "France",
                 "Italy", "India", "Pakistan", "")

df <- global %>%
  filter(country %in% country_list, cum_deaths >0) %>%
  group_by(country, date) %>%
  summarise(
    cum_case_fatality_rate = (sum(cum_deaths)/sum(cum_cases)) * 100
    , daily_case_fatality_rate = (sum(daily_deaths)/sum(cum_cases)) * 100
  ) %>%
  group_by(country) %>%
  mutate(
    day = row_number()
  ) %>%
  gather(metric, n, cum_case_fatality_rate, daily_case_fatality_rate, factor_key = TRUE)


p <- df %>%
  ggplot(aes(x=day, y=n, color=metric))+
  geom_point() +
  geom_smooth() +
  theme_minimal(base_size = 14) +
  theme(legend.position="top") +
  ggtitle("GLOBAL case fatality rate", data_last_refreshed ) +
  xlab("Day since first death") +
  ylab("Case fatality rate (%)")+
  labs(
    title= "GLOBAL case fatality rate",
    caption = "SOURCE: Johns Hopkins Coronavirus Resource Center"
  )
p = p + scale_y_continuous(labels=comma, expand=c(0.5, 0.5)) +
  scale_color_discrete(name="Metric",
                       breaks=c("cum_case_fatality_rate",
                                "daily_case_fatality_rate"),
                       labels=c("Cumulative case fatality",
                                "Daily case fatality")) +
  guides(fill=guides(colour=guide_legend(ncol=3,nrow=1,byrow=TRUE))) +
  facet_wrap( ~ country, ncol = 2)

suppressMessages(print(p))


# Fri Apr 17 08:27:36 2020 ------------------------------

# 3.9% ascertainment rate


a1 <- fit_seir(country = "Bangladesh(unoff)", af=.04, nfuture = nfuture)
p1<- a1$fitted_cases %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_line(aes(y = E), colour = "blue") +
  # geom_line(aes(y = R), colour = "darkgreen") +
  geom_point(aes(y = cum_cases), colour = "orange") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Cumulative incidence",
       title = paste("R0 =", format(a1$R0, digit=3),"and","assumsed reporting =", a1$af*100,"%" ))

