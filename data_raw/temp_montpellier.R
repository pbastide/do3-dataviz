library(here)

temp_all <- read.csv(here("data_raw", "temp_montpellier.txt"), skip = 19)

## Rename
temp_all <- temp_all[colnames(temp_all) != c("SOUID", "Q_TG")]
colnames(temp_all) <- c("date", "temperature")

## Temperature (in 0.1 °C)
temp_all$temperature <- temp_all$temperature / 10

## Split date
library(tidyr)
temp_all <- temp_all %>%  separate(date, into = c("year", "month", "day"), sep = c(4, 6))

## Months and days
library(dplyr)
month_names <- c(
  "01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun",
  "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"
)
month_days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
month_days_cum <- c(0, cumsum(month_days))

temp_all <- temp_all %>%
  mutate(month_name = month_names[month]) %>%
  mutate(month_name = factor(month_name, levels = unname(month_names))) %>%
  mutate(day_of_year = month_days_cum[as.numeric(month)] + as.numeric(day))

## Filter years
temp_all <- temp_all %>%
  filter(year >= 1950) %>%
  filter(year <= 2019)

## Save
write.csv(temp_all, here("data", "temp_montpellier.csv"), row.names = FALSE)
