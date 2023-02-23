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


#### PLOTTING ####
plot1 <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = year_fac)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  xlab("pink smolt Length 2021 and 2022")

plot1

plot2 <- pinkLW %>%
  ggplot(aes(x = Length, y = Weight, color = year_fac)) +
  geom_point(size = 3, alpha = 0.8) +
    xlab("pink smolt Length 2021 and 2022")+
  facet_wrap(~hatchery_wild)+
  theme_bw()+
  theme(legend.position = "bottom")
  
plot2
# Save your plot
ggsave(plot2, filename = "output/pink_oto_LW_scatter.png", width = 6.5, 
       height = 6, units = "in")


##these scatter plots and results with Kdry and HSIdry for age-0 only
##show no relationship and so can test Kdry and HSI independently.
a <- lm(formula = HSIdry~Kdry, data = codcond1)
summary (a)
##R2 = 0.0033, n=217) no good relationship

library(ggplot2)
library("ggpubr")

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = K_wet, color = Month)) +
  geom_point()+
  theme_minimal()
plot1


head(codcond1)
##using as.factor(age) made the legend be a category so it used only whole numbers
plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color= as.factor(age))) +
  geom_point()+
  theme_minimal()

plot1

# Save your plot
ggsave(plot1, filename = "output/pinkLW_scatter.png", width = 6.5, height = 6, units = "in")
plot1

##going to run age-0 length weight regression

library (mgcv) #library for running gams
mod1 <- gam(wgt_total ~ s(TL) + month, data = codcond1)
##s means smooth length. putting month into the gam
#this model treats every fish as an individual observation, and maybe the seine should be the observation

summary (mod1)
plot(mod1)
str(mod1) #this tells us everything that is saved in model object
str(summary(mod1))

ggplot(data = codcond1,
       aes(x = TL,
           y = wgt_total,
           color = sex,
           shape = sex)) +
  geom_point(size = 1.5,
             alpha = 0.8)

library(mgcv)
library(gamm4)

codcond1 <- codcond1 %>%
  mutate(year_fac = as.factor(year),
         site_fac = as.factor(site),
         day_fac = as.factor(Julian_date))


ggplot(codcond1, aes(TL, HSI_wet, color = year_fac))+
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)




## liver FA models## -----------------------

# set up things!
# Libraries
library(patchwork)


#### PLOTTING ####
head(codgrosslipid)

##this renames columns and makes new dataframe 'codFA' 
codFA <- rename(codgrosslipid,"HSIwet" = "HIS_wet") 
codFA <- rename(codFA,"liverFA" = "per_liver_FA") 
codFA <- rename(codFA,"muscleFA" = "per_musc_FA") 
head(codFA)

library(mgcv)
library(gamm4)

codFA <- codFA %>%
  mutate(year_fac = as.factor(Year),
         site_fac = as.factor(`site #`),
         day_fac = as.factor(J_date)) 




##seems that results same for HSI wet and HSI dry. Try linear model
linear_mod <- lm (formula = HSI_wet~ HSIdry, data = codcond1)
summary(linear_mod)
ggplot(linear_mod, aes(HSIdry, HSI_wet)) +
  geom_point() +
  theme_minimal

 
library(ggplot2)
library("ggpubr")

L <- ggplot(codFA, aes(J_date, liverFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Liver Fatty Acids", x = "Day of Year") +
  theme(legend.position = c(0.2, 0.8))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
plot(L)

TL <- ggplot(data = codFA,
             aes(x = TL,
                 y = liverFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  labs(y = "% Liver Fatty Acids", x = "Total Length (mm)") 

plot(TL)  

TLM <- ggplot(data = codFA,
              aes(x = TL,
                  y = muscleFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  labs(y = "% Muscle Fatty Acids", x = "Total Length (mm)") 
plot(TLM) 

M <- ggplot(codFA, aes(J_date, muscleFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Muscle Fatty Acids", x = "Day of Year") +
  theme(legend.position = c(0.2, 0.2))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot(M)


FAfigure <- ggarrange(L, TL, M, TLM,
                      labels = c("A", "C", "B", "D"),
                      ncol = 2, nrow = 2)
FAfigure
ggsave("./Figs/liverFA_muscleFA.png", width = 6, height = 6, units = 'in')
#this is FIGURE 7




