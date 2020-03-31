# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial

# install.packages('gsheet')
library(tidyverse)


# RUN covid_update_data.R to update the data
us_state_data <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
df <- read_csv(us_state_data)

df_ <- df %>%
  rename(
    cum_cases = cases
    , cum_deaths = deaths
  ) %>%
  gather(metric, n, cum_cases, cum_deaths,
         factor_key=TRUE) %>%
  arrange(
    state, date
  )


# Top 5 state by new  cases
us_top5_death <- df_ %>%
  filter(!state %in% c('New York', 'New Jersey')) %>%
  filter(metric=="cum_deaths") %>%
  group_by(state) %>%
  summarise(
    max_n_cases = max(n)
  ) %>%
  arrange(desc(max_n_cases)) %>%
  top_n(5, max_n_cases)

us_new_cases <- df_ %>%
  filter(n>0, state %in% us_top5_death$state, metric=='cum_cases') %>%
  arrange(state, date, n)

data_last_refreshed = max(us_new_cases$date)
data_last_refreshed



#
#
#
# ## NEW CASES IN NY AND NJ -------------------
#
# # Top 5 state by new  cases
# n5_new_cases <- df_ %>%
#   filter(state %in% c('New York', 'New Jersey')) %>%
#   group_by(state) %>%
#   summarise(
#     max_n_cases = max(n)
#   ) %>%
#   arrange(desc(max_n_cases)) %>%
#   top_n(5, max_n_cases)
#
# us_new_cases <- df_ %>%
#   filter(n>0, state %in% n5_new_cases$state, metric=='cum_cases') %>%
#   arrange(state, date, n)
#
# data_last_refreshed = max(us_new_cases$date)
# data_last_refreshed
#
# ggplot(us_new_cases, aes(x = date, y = n, color=state)) +
#   geom_line(size=1.1) +
#   geom_text_repel(size=5, aes(label=state),
#                   function(us_new_cases) us_new_cases[ymd(us_new_cases$date) == data_last_refreshed, ])+
#   theme_minimal(base_size = 14) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none") +
#   # theme_void() +
#   theme(legend.position="none") +
#   ggtitle("COVID19 NEW CASES since the first case \n(New York and New Jersey)", data_last_refreshed ) +
#   xlab("Dates since first confirmed case") +
#   ylab("Cumulative cases")
#
# # p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#
#
#
#
