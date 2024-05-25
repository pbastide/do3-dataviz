library(ggplot2)

## select number of nurses and shifts
n_shifts <- 365*3 # 3 shifts per day in a year
n_nurses <- 38    # total number of nurses
n_on_shift <- 13  # 13 nurses on duty for each shift

## Schedule nurses on shifts
shift_table <- matrix(0, ncol = n_shifts, nrow = n_nurses)
for (shift in 1:n_shifts) {
  nurse_on_shift <- sample(1:n_nurses, n_on_shift, replace = TRUE)
  shift_table[nurse_on_shift, shift] <- 1
}
shift_table[1, ]

## For each shift, randomly assign deaths
p_death <- 0.14 # probability that there is a death on a shift
deaths_shifts <- sample(c(0, 1), n_shifts, replace = TRUE,
                        prob = c(1 - p_death, p_death))

## Find out when there was a death on a nurse shift
death_table <- matrix(NA, ncol = n_shifts, nrow = n_nurses)
for (nurse in 1:n_nurses) {
  for (shift in 1:n_shifts) {
    death_table[nurse, shift] <- shift_table[nurse, shift] * deaths_shifts[shift]
  }
}

## plot all nurses and shifts
death_table_long <- reshape2::melt(death_table)
colnames(death_table_long) <- c("nurses", "shifts", "death")
death_table_long$nurses <- as.factor(death_table_long$nurses)
death_table_long$shifts <- as.factor(death_table_long$shifts)
death_table_long$death <- as.factor(death_table_long$death)

ggplot(death_table_long, aes(x = shifts, y = nurses)) +
  geom_raster(aes(fill = death)) +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  theme_bw() +
  scale_x_discrete(breaks = c(1, 85, 170))

## Pick a nurse at random
random_nurse <- sample(1:n_nurses, 1)

## Keep only shifts with death for random nurse
deaths_random_nurse <- which(death_table[random_nurse, ] == 1)
death_table_nurse <- death_table[, deaths_random_nurse]

## Plot selected data
order_nurses <- c((1:n_nurses)[-random_nurse], random_nurse)
death_table_long <- reshape2::melt(death_table_nurse)
colnames(death_table_long) <- c("nurses", "shifts", "death")
death_table_long$nurses <- factor(death_table_long$nurses, levels = order_nurses)
death_table_long$shifts <- as.factor(death_table_long$shifts)
death_table_long$death <- as.factor(death_table_long$death)

ggplot(death_table_long, aes(x = shifts, y = nurses)) +
  geom_raster(aes(fill = death)) +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  theme_bw()
