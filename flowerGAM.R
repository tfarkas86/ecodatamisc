
# An attempt at flowering over time plots using generalized additive models to fit data

# Packages
library("readr")
library("tidyverse")
library("mgcv")
library("lubridate")
library("tidymv")
library(ecodatamisc)

# Data
# Originally "Total_Flowers_Under.2m" and	"Total_Flowers_Above.2m"
# From Flower_Cherelle_Monitoring excel documents
# Cleaned data that is also joined across years is unpooled

# Pooled data
fc_dat <- read_csv("./Data/Output/Flower_Cherelle.csv") %>%
  mutate(julian_day = yday(Observation_Date), .after = "Observation_Date") %>%
  mutate(across(Year, ~as.factor(.x)),
         y1 = Total_Flowers_Above_2m + Total_Flowers_Under_2m)

# k = number of knots
gam1 <- gam(y1 ~ s(x1), method = "REML")

# Diagnostic test for something important, I don't remember
# I think if P > 0.05, the time component if fully explained by the GAM
resid(gam1) %>% Box.test()

summary(gam1)


# make model predictions the same length as the original dataset (so you can map on raw data/dates)
stick <- length(x1)
model_p <- predict_gam(gam1, length_out = stick)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x1, fit)) +
  geom_smooth_ci(ci_alpha=.3)
pgam_plot

# plots points and labels for the correct original data
gam1_plot <- pgam_plot +
  geom_point(aes(y=y1), alpha=0.1) +
  scale_x_continuous(name="Julian day (all three years included)") +
  scale_y_continuous(name="# of flowers observed on all branches", limits = c(0,500)) +
  theme_bw(base_size=12)

gam1_plot




# GAM for just one year and one treatment #####
fc_2018 <- fc_dat %>%
  filter(Year == "2018")


gam2 <- gam(y1 ~ s(julian_day), data = fc_2018, method = "REML")


# make model predictions the same length as the original dataset (so you can map on raw data/dates)
stick <- length(x1)
model_p <- predict_gam(gam1, length_out = stick)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x1, fit)) +
  geom_smooth_ci(ci_alpha=.3)
pgam_plot

# plots points and labels for the correct original data
gam1_plot <- pgam_plot +
  geom_point(aes(y=y1), alpha=0.1) +
  scale_x_continuous(name="Julian day (2018)") +
  scale_y_continuous(name="# of flowers observed on all branches", limits = c(0,400)) +
  theme_bw(base_size=12)

gam1_plot

pol_dat <- read_csv("../MarsPollinator/Data/Output/Hand_Pollination.csv")

pol_dat %>%
  group_by(Pollination_Timing) %>%
  summarize(mindate = min(day(Pollination_Date)),
            maxdate = max(day(Pollination_Date)))





  pol_dat %>%
    select(Pollination_Timing, Pollination_Date) %>%
    filter(Pollination_Timing %in% c("early", "middle", "late")) %>% View



