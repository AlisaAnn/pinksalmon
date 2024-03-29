# This is the plotting & analysis for pink salmon LW data
#it includes the KRAA otolith results from 2022

# Libraries
library(patchwork)
library(tidyverse)
library(lubridate)
library(ggplot2)
.libPaths()

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# This is the data import script
# Read in the data and rename any columns that need renaming
pinkLW <- read_csv("C:/Users/alask/Documents/Git/pinksalmon/data/pink_LW_oto.csv")
head(pinkLW)
tail(pinkLW)

distinct(pinkLW, year) #both 2021 and 2022
distinct(pinkLW, hatchery_wild)
#3 categories: wild, hatchery, unknown

library(mgcv)
library(gamm4)

pinkLW <- pinkLW %>%
  mutate(year_fac = as.factor(year))
head(pinkLW)


head(pinkLW)
#### PLOTTING ####
plot1 <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = year_fac)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  xlab("pink smolt Length 2021 and 2022")

plot1
ggsave(plot1, filename = "output/pink_LW_scatter_year.png", width = 6.5, 
       height = 6, units = "in")


plota <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = hatchery_wild)) +
  geom_point(size = 2, alpha = 0.6) +
  theme_minimal() +
  xlab("pink smolt Length 2021 and 2022")

plota
ggsave(plota, filename = "output/pink_LW_scatter_otolith.png", width = 6.5, 
       height = 6, units = "in")

plotA <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = hatchery_wild)) +
  geom_point(size = 2, alpha = 0.6) +
  theme_bw() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)+
  xlab("pink smolt Length 2021 and 2022")
plotA
ggsave(plotA, filename = "output/pink_LW_scatter_origin.png", width = 6.5, 
       height = 8, units = "in")



plot2 <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = year_fac)) +
  geom_point(size = 3, alpha = 0.8) +
  xlab("pink smolt Length 2021 and 2022")+
  facet_wrap(~hatchery_wild)+
  theme_bw()+
  theme(legend.position = "bottom")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
  
plot2
# Save your plot
ggsave(plot2, filename = "output/pink_oto_LW_scatter.png", width = 6.5, 
       height = 6, units = "in")

##going to remove 'unknown' fish and replot
pinkLW_HW <- filter(pinkLW, hatchery_wild == "wild" | hatchery_wild == "hatchery")
distinct(pinkLW_HW, hatchery_wild)

plotb <- pinkLW_HW %>%
  ggplot(aes(x = Length, y = Weight, color = hatchery_wild)) +
  geom_point(size = 1.5, alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)+
  xlab("pink smolt Length 2021 and 2022")

plotb
ggsave(plotb, filename = "output/pink_LW_SCAT1_otoHW.png", width = 6.5, 
       height = 8, units = "in")


##use a GAM model to test if LW curve different for hatchery or wild fish
#using dataset with 'unknown pinks' removed and only 2 values for hatchery_wild

mod1 <- gam(Weight ~ s(Length, k = 4) + hatchery_wild, data = pinkLW_HW,
             family = gaussian)

summary(mod1)
gam.check(mod1)
concurvity(mod1,full = TRUE)
##can see that LW curve is significant but not different for hatcher/wild fish.

library(patchwork)
library(ggplot2)
library("ggpubr")

pinkLW21 <- filter(pinkLW, year == 2021) 
distinct(pinkLW21, year)
pinkLW22 <- filter(pinkLW, year == 2022)
distinct(pinkLW22, year)

plot3 <- pinkLW21 %>%
  ggplot(aes(x = Length, y = Weight)) +
  geom_point(size = 3, alpha = 0.8) +
  xlab("pink smolt Length 2021")+
  facet_wrap(~hatchery_wild)+
  theme_bw()+
  theme(legend.position = "bottom")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


plot3

plot4 <- pinkLW22 %>%
  ggplot(aes(x = Length, y = Weight)) +
  geom_point(size = 3, alpha = 0.8) +
  xlab("pink smolt Length 2022")+
  facet_wrap(~hatchery_wild)+
  theme_bw()+
  theme(legend.position = "bottom")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot4

library(ggplot2)
library("ggpubr")
FAfigure <- ggarrange(plot3, plot4,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
FAfigure
ggsave("./output/LWpink_oto_year.png", width = 6, height = 6, units = 'in')

##going to run age-0 length weight regression

modall <- gam(log(Weight) ~ s(Length) + hatchery_wild, data = pinkLW_HW)
#this model treats every fish as an individual observation, and maybe the seine should be the observation
summary (modall)
plot(modall)
str(modall) #this tells us everything that is saved in model object
str(summary(modall))

# Read in the data and rename any columns that need renaming
PLW <- read_csv("C:/Users/alask/Documents/Git/pinksalmon/data/pink_LW.csv")
head(PLW)
tail(PLW)

plotM <- PLW %>%
  ggplot(aes(x = FL_mm, y = Wgt_g)) +
  geom_point(size = 1.5, alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)+
  xlab("pink smolt Length 2021 and 2022")

plotM
ggsave(plotM, filename = "output/pink_LW_month.png", width = 6.5, 
       height = 8, units = "in")
distinct(PLW, Year)
