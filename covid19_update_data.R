# Mon Mar 30 19:53:38 2020 ------------------------------
# Data update script

# Load packages
require(ggplot2)
require(ggrepel)
require(tidyverse)
require(lubridate)
require(knitr)
require(kableExtra)

library(gsheet)


# Update Bangladesh unofficial and official data

sheet_3 = 'https://docs.google.com/spreadsheets/d/1nlAQffAvqChLtvGiJvJPnNOJJKBu_uzmnKdpAJXuPwM/edit#gid=336445634'
df = gsheet2tbl(sheet_3)
write_csv(df, 'covid19_bd.csv')

