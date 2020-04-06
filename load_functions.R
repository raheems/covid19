# Sun Apr  5 17:44:26 2020 ------------------------------
# global functions

case_fatality <- function(data){
  data %>%
    group_by(date) %>%
    summarise(
      cum_case_fatality_rate = (sum(cum_deaths)/sum(cum_cases)) * 100
      , daily_case_fatality_rate = (sum(daily_deaths)/sum(cum_cases)) * 100
    ) %>%
    gather(metric, n, cum_case_fatality_rate, daily_case_fatality_rate, factor_key = TRUE)
}


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
    select(country, day, date, case_gr_rate, death_gr_rate) %>%
    gather(metric, n, case_gr_rate, death_gr_rate, factor_key=TRUE)

  p <- ggplot(a, aes(x = day, y = n, color=metric)) +
    geom_line(size=1.1) +
    # geom_text_repel(size=5, aes(label=metric),
    #                 function(a) a[ymd(a$date) == max(a$date), ])+
    theme_minimal(base_size = 14)
  p
}
