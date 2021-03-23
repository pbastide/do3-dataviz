library(here)

temp_all <- read.csv(here("data_raw", "temp_all.txt"))

## Rename
temp_all <- temp_all[colnames(temp_all) != "Q_TG"]
colnames(temp_all) <- c("city", "date", "temperature")

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

## Average
temps_average <- temp_all %>%
  group_by(city, day_of_year) %>%
  summarize(month = month[1],
            temperature = mean(temperature), .groups = "keep")

## Save
write.csv(temps_average, here("data", "temp_average.csv"), row.names = FALSE)

## Plot all
library(ggplot2)
ggplot(temp_all, aes(x = day_of_year, y = temperature, color = city)) +
  geom_line(size = 1)

ggplot(temps_average, aes(x = day_of_year, y = temperature, color = city)) +
  geom_line(size = 1) +
  scale_x_continuous(name = "month",
                     breaks = month_days_cum[-13] + 1,
                     labels = month_names) +
  theme_light()
