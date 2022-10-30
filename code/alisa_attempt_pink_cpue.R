## modifiying Mike's script 
##to estimate pink salmon abundance by year for WGOA sampling
## working in Github

library(tidyverse)
library(chron)
# Libraries
library(patchwork)


# Load the previous script
source("code/LW_data_import.R")


#  view WGOA data
View(PC)
KDSP_pink <-PC
View(KDSP_pink)

# add a measured column
#d3$Measured <- NA

#unique(d2$Species)
#unique(d3$species) # looks good

#  combine with d2
#d3 <- d3 %>%
#  select(Station, species, Measured, CPUE)

#names(d3) <- c("Station", "Species", "Measured", "Total.catch")

#names(d2)

# combine
#dat <- rbind(d2, d3)

#dat <- dat %>%
#  filter(Species=="pink salmon")

# now need to add year, julian day, site, and bay!
#d4 <- read.csv("data/2018 2020 site.csv")

#head(d4)

# retain only the keeper sets

## NB! changing 115 to cpue==no!
#d4$use.for.CPUE[d4$Station==115] <- "no"

#d4 <- d4 %>%
#  filter(use.for.CPUE=="yes")

##Alisa: in order to get my columns to match Mike's change KDSP_pink and rename as pink##
head(KDSP_pink)
rename(KDSP_pink,"Date" = "date")
head(KDSP_pink)
pink<- rename(KDSP_pink,"Station" = "station", "Date" = "date", "Site" = "site",
                "Bay" = "bay", "Total.catch" = "cpue", "Species" = "species")
View(pink)
##had trouble w julian date, so making a second year column. copy it
pink1<- pink %>% mutate(Year = year)
View(pink1)
head(pink1)

##try this. going to rename dataframe 'd4' and have to drop column Temp that mike used
d4 <- pink1 %>%
  select(Date, Station, Site, year, Bay, Total.catch, Year)
View(d4)

# calculate Julian day. if gives error may need to load chron package
d4$Date <- dates(as.character(d4$Date))
d4$julian <- lubridate::yday(d4$Date)
d4$year <- years(d4$Date)

head(d4)

## this select is saying to drop Date column
d4 <- d4 %>%
  select(-Date)

names(d4)[1] <- "Station"
d4$Station <- as.factor(d4$Station)

#dat$Station <- as.factor(dat$Station)
#AA need to remove junk column 'year'
d4 <- d4 %>%
  select(-year)
#d4 <- left_join(d4, dat)
View(d4)
names(d4)[1:3] <- c("Station", "Site", "Bay")
names(d4)[4] <- "pink"
View(d4)

##now need to rename columns back to what they were to follow code
##maybe not. perhpas ignorne
#d4<- rename(d4,"Station" = "station", "Date" = "date", "site" = "Site",
#             "bay" = "Bay")

d4 <- d4 %>%
  select(Year, julian, Site, Bay, pink)

# drop Sanak and Kujulik (only sampled once!)
d4 <- d4 %>% 
  filter(Bay != "NE Harbor") %>%
  filter(Bay != "Caton Harbor") %>%
  filter(Bay != "Kujulik")
d4[d4$Bay == "Cook",]
distinct(d4,Bay)


# set regional codes
unique(d4$Bay)
d4$region <- ifelse(d4$Bay %in% c("Ugak", "Kiliuda", "Kaiugnak", "Japanese", "Rodmans Reach"),
                    "East side",
                    "Peninsula")
View(d4)


pink.dat <- d4
View(pink.dat)

change <- is.na(pink.dat$pink)
pink.dat$pink[change] <- 0

pink.dat$log.pink <- log(pink.dat$pink+1, 10)
View(pink.dat)

summary <- pink.dat %>%
  group_by(Year, region) %>%
  dplyr::summarise(mean=mean(log.pink),
            se=sd(log.pink)/sqrt(n()),
            n=n())



rlang::last_error()

## now estimate annual abundance

## Model up seine recruitment estimates for each year
## and compare with stock assessment model esimated recruitment

library(ggplot2)
library(plyr)
library(mgcv)
library(rstan)
library(brms)
library(bayesplot)

#source("./scripts/stan_utils.R")
source("code/stan_utils.R")

##it is ok if R says these were in previous version
##I don't have the source stan_utils.R, and that's OK also
## set up data --------------------------------------------
pink.dat$bay_fac <- as.factor(pink.dat$Bay)
pink.dat$year_fac <- as.factor(pink.dat$Year)
pink.dat$region_fac <- as.factor(pink.dat$region)
change <- is.na(pink.dat$pink)
pink.dat$pink[change] <- 0

ggplot(filter(pink.dat, pink>0), aes(julian,pink)) +
  geom_point() +
  geom_smooth()

##pink.dat <- pink.dat %>%
##  filter(region == "East side")
## comment-out above because thru 2021 Mike selected only East side,
#but that model not good w 2022. not sure why. but birch wants all areas anyway.
##going to remove region filter and run w all

##brms: setup ---------------------------------------------

## Define model formulas
pink_formula <-  bf(pink ~ s(julian, k=4) + year_fac,
                    zi ~ s(julian, k=4) + year_fac)


## Set model distributions
zinb <- zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit")

## fit: zero-inflated --------------------------------------
pink_zinb <- brm(pink_formula,
                 data = pink.dat,
                 family = zinb,
                 cores = 4, chains = 4, iter = 4000,
                 save_pars = save_pars(all = TRUE),
                 control = list(adapt_delta = 0.99, max_treedepth = 10))
##ALisa and Mike stopped here. 2018-2022 east side only wasn't a good fit.
##going to re-run some diagnostics and possibly not limit to East Side. 
#Prior to 2022, model was run with only East side.

#saveRDS(pink_zinb, file = "pink_zinb.rds")
#pink_zinb  <- add_criterion(pink_zinb,c("loo", "bayes_R2"), moment_match = TRUE)
##loo is leave one out, and for model comparison
saveRDS(pink_zinb, file = "output/pink_zinb.rds")

pink_zinb <- readRDS("./output/pink_zinb.rds")
check_hmc_diagnostics(pink_zinb$fit)
##can not have any divergences. best if Tree depth = 0
neff_lowest(pink_zinb$fit)
#want these over 1000 iterations
rhat_highest(pink_zinb$fit)
#a good model has all values less than 1.05
summary(pink_zinb)
pink_table <- summary(pink_zinb)
View(pink_table)
bayes_R2(pink_zinb)
#this is the model object fit in brooms
plot(pink_zinb$criteria$loo, "k")

conditional_effects(pink_zinb)
## makes a nice figure


plot(pink_zinb, ask = FALSE)
y <- pink.dat$pink
yrep_pink_zinb  <- fitted(pink_zinb, scale = "response", summary = FALSE)
ppc_dens_overlay(y = y, yrep = yrep_pink_zinb[sample(nrow(yrep_pink_zinb), 25), ]) +
  xlim(0, 500) +
  ggtitle("pink_zinb")
pdf("output/trace_pink_zinb.pdf", width = 6, height = 4)
trace_plot(pink_zinb$fit)
dev.off()

## second model - add Bay factor---------------------------
## Define model formulas
pink_formula <-  bf(pink ~ s(julian, k=4) + year_fac + bay_fac,
                    zi ~ s(julian, k=4) + year_fac + bay_fac)


## Set model distributions
zinb <- zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit")

## fit: zero-inflated --------------------------------------
pink_zinb2 <- brm(pink_formula,
                 data = pink.dat,
                 family = zinb,
                 cores = 4, chains = 4, iter = 5000,
                 save_pars = save_pars(all = TRUE),
                 control = list(adapt_delta = 0.99, max_treedepth = 16))
## doesn't fit!

## third model - bay_fac as a group-level (random term)
## Define model formulas
pink_formula <-  bf(pink ~ s(julian, k=4) + year_fac + (1 | bay_fac),
                    zi ~ s(julian, k=4) + year_fac + (1 | bay_fac))

# doesn't fit!

## Set model distributions
zinb <- zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit")

## fit: zero-inflated --------------------------------------
pink_zinb3 <- brm(pink_formula,
                  data = pink.dat,
                  family = zinb,
                  cores = 4, chains = 4, iter = 5000,
                  save_pars = save_pars(all = TRUE),
                  control = list(adapt_delta = 0.99, max_treedepth = 16))


##ALisa and Mike stopped here. 2018-2022 east side only wasn't a good fit.
##going to re-run some diagnostics and possibly not limit to East Side. 
#Prior to 2022, model was run with only East side.

#saveRDS(pink_zinb, file = "pink_zinb.rds")
#pink_zinb  <- add_criterion(pink_zinb,c("loo", "bayes_R2"), moment_match = TRUE)
##loo is leave one out, and for model comparison
saveRDS(pink_zinb, file = "output/pink_zinb.rds")

pink_zinb <- readRDS("./output/pink_zinb.rds")
check_hmc_diagnostics(pink_zinb2$fit)
##can not have any divergences. best if Tree depth = 0
neff_lowest(pink_zinb$fit)
#want these over 1000 iterations
rhat_highest(pink_zinb$fit)
#a good model has all values less than 1.05
summary(pink_zinb)
pink_table <- summary(pink_zinb)
View(pink_table)
bayes_R2(pink_zinb)
#this is the model object fit in brooms
plot(pink_zinb$criteria$loo, "k")

conditional_effects(pink_zinb)
## makes a nice figure


plot(pink_zinb, ask = FALSE)
y <- pink.dat$pink
yrep_pink_zinb  <- fitted(pink_zinb, scale = "response", summary = FALSE)
ppc_dens_overlay(y = y, yrep = yrep_pink_zinb[sample(nrow(yrep_pink_zinb), 25), ]) +
  xlim(0, 500) +
  ggtitle("pink_zinb")
pdf("output/trace_pink_zinb.pdf", width = 6, height = 4)
trace_plot(pink_zinb$fit)
dev.off()



## Predicted effects ---------------------------------------

## Year predictions ##

## 95% CI
ce1s_1 <- conditional_effects(pink_zinb, probs = c(0.025, 0.975))
mod.95 <- ce1s_1$year_fac %>%
  select(year_fac, estimate__, lower__, upper__)
names(mod.95)[3:4] <- c("ymin.95", "ymax.95")
mod.95$year <- as.numeric(as.character(mod.95$year_fac))

theme_set(theme_bw())

g1 <- ggplot(mod.95) +
  aes(x = year, y = estimate__) +
  geom_ribbon(aes(ymin = ymin.95, ymax = ymax.95), fill = "grey90", alpha=0.8) +
  # geom_ribbon(aes(ymin = ymin.90, ymax = ymax.90), fill = "grey85") +
  # geom_ribbon(aes(ymin = ymin.80, ymax = ymax.80), fill = "grey80") +
  geom_line(size = 0.5, color = "red3") +
 #geom_hline(yintercept = 0.9, lty=2) +
  theme(axis.title.x = element_blank()) +
  ylab("CPUE") +
  scale_x_continuous(breaks=seq(1980, 2040, 10)) 

print(g1)
## AA didn't like the g1 plot, mike helped make g2 plot w same data
g2 <- ggplot(mod.95) +
  aes(x = year_fac, y = estimate__) +
  geom_errorbar(aes(ymin = ymin.95, ymax = ymax.95)) +
  # geom_ribbon(aes(ymin = ymin.90, ymax = ymax.90), fill = "grey85") +
  # geom_ribbon(aes(ymin = ymin.80, ymax = ymax.80), fill = "grey80") +
  geom_point() +
  #geom_hline(yintercept = 0.9, lty=2) +
  theme(axis.title.x = element_blank()) +
  ylab("CPUE") 

print(g2)
ggsave("output/cpue_year.jpg")
## this was best linear model. but i need to plot data to make sure best
#maybe i should have smooth model and not linear fit 10/14/22
View(pink.dat)
ggplot(data = pink.dat,
       aes(x = year_fac,
           y = pink,
           color = year_fac)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)

