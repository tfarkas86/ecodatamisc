
# An attempt at flowering over time plots using generalized additive models to fit data

# Packages
library("readr")
library("tidyverse")
library("mgcv")
library("lubridate")
library("tidymv")
library("ecodatamisc")

# import and manipulate
fc_dat <- read_csv("./Data/Output/Flower_Cherelle.csv") %>%
  mutate(julian_day = yday(Observation_Date), .after = "Observation_Date") %>%
  mutate(across(Year, ~as.factor(.x)),
         y1 = Total_Flowers_Above_2m + Total_Flowers_Under_2m)

# select 2018 only
fc_2018 <- fc_dat %>%
  filter(Year == "2018")

# fit gam
gam2018 <- gam(y1 ~ s(julian_day), data = fc_2018, method = "REML")
summary(gam2018)

# plot with plot_gam
gamplot(gam2018, ylab = "Total Flowers", xlab = "Julian Day (2018)", ylims = c(0, 400))
