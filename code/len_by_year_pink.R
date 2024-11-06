# This is the plotting & analysis script for pink salmon length by year

# Libraries
library(patchwork)
library (MuMIn) 
library(tidyverse)

# Load the previous script
source("C:/Users/alask/Documents/Git/pinksalmon/code/LW_data_import.R")

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### PLOTTING ####
head(pinkLen)

##LF for all years, all months, all bays
ggplot(pinkLen, aes(length, fill=year)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,250)+
  facet_wrap(~year)

##instead plot geom_histogram

ggplot(pinkLen, aes(length, fill=month)) +
  geom_histogram(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,250)+
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "cyan",
                               "purple","grey", "black"))+
  theme_bw() +
  facet_wrap(~year)

##filter data so no May and max length < 155
pinkLen1 <- filter(pinkLen, month != "May")
pinkLen1 <- filter(pinkLen1, length <= 155)
distinct(pinkLen1,year)


##plot length by julian date instead of histograms, as per Mike's idea 12/2/22
pinkLen1 <- pinkLen1 %>%
  mutate(year_fac = as.factor(year))
#note the large 2018 fish at day 200 are ALL from Kujulik. remove this bay as only sampled in 2018
pinkLen1 <- filter(pinkLen1, bay != "Kujulik")
distinct(pinkLen1,bay)

ggplot(pinkLen1, aes(J_Date, length, color = year_fac)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = F)

ggsave("./figs/length_by_date_noMay.png", width = 6, height = 4, units = 'in')


head(pinkLen1)
mod1 <- mgcv::gam(length ~ s(J_Date, k = 6, by = year_fac), data = pinkLen1)
mod2 <- mgcv::gam(length ~ s(J_Date, k = 6), data = pinkLen1)
#MuMIn::AICc(mod1, mod2) # different curves for different years = better model
AIC(mod1,mod2)
summary(mod1)


ggplot(data = pinkLen1, aes(x = J_Date, y = length, color = year_fac)) +
  geom_point() +
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom") +
  theme_bw()+
  facet_wrap(~year)
#very low sample size in 2020