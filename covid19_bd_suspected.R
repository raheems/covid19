# Mon Mar 30 19:53:38 2020 ------------------------------
# Bangladesh unofficial

# install.packages('gsheet')
library(gsheet)
library(tidyverse)

update = FALSE
# update = TRUE

if (update){
  sheet_2 = 'https://docs.google.com/spreadsheets/d/1nlAQffAvqChLtvGiJvJPnNOJJKBu_uzmnKdpAJXuPwM/edit#gid=0'
  df = gsheet2tbl(sheet_2)
  write_csv(df, 'covid19_bd_unofficial.csv')
}


df <- read_csv('covid19_bd_unofficial.csv')

names(df)

df <- df %>%
  rename(
    sl_no = `মৃত্যুর ক্রমসংখ্যা`,
    date = "তারিখ"  ,
    locality = "এলাকা",
    symptom = "লক্ষণ",
    other_info = "অন্যান্য তথ্য",
    news_url = "সোর্স- লিংক"
  ) %>%
  drop_na(date) %>%
  mutate(
    deaths = 1,
    cum_deaths = cumsum(deaths)
    , date = as.Date(date, "%d/%m/%Y")
  ) %>%
  select(sl_no:news_url, deaths, cum_deaths) %>%
  arrange(
    date
  )

head(df)

# Cumulative Cases since first identified case
ggplot(df, aes(x = date, y = cum_deaths)) +
  geom_line(size=1.1) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  # theme_void() +
  theme(legend.position="none") +
  ggtitle("COVID19 cumulative cases since first case \n(top 5 countries excluding China)") +
  xlab("Days since first confirmed case") +
  ylab("Cumulative cases")

p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


