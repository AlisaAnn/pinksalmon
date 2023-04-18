# This is the plotting & analysis script

# Libraries
library(patchwork)

# Load the previous script
source("C:/Users/alask/Documents/pinksalmon_analysis/code/LW_data_import.R")
head(PC)
distinct(PC, bay)
distinct(PC,year)
distinct(PC,month)
tail(PC)

##going to plot only 2022 and compare months##
##show that most pinks caught in May"
KDSP_P22 <- filter(PC, year == "2022")
distinct(KDSP_P22,year)
str(KDSP_P22)

#### Now can only 2022 with months ##PLOTTING ####
ggplot(data = KDSP_P22,
       aes(x = (as.factor(month)),
           y = cpue)) +
  geom_point(size = 1.5)+
  geom_jitter(alpha = 0.3)+
  theme_bw()+
  xlab("Month")+
  ylab("Relative abundance")+
  theme(legend.position = "bottom")

ggsave("./output/pink_CPUE_2022_month.png", width = 6, height = 4, units = 'in')

##resume Mike's script for cpue by year##
### view PC dataframe where PC is for Pink CPUE##
head(PC)
distinct(PC, bay)
distinct(PC,year)
distinct(PC,month)
tail(PC)

##going to remove May and June so years are comparable##
KDSP_pink <- filter(PC, month != "5")
KDSP_pink <- filter(KDSP_pink, month != "6")
distinct(PC,month)
distinct(KDSP_pink,month)
str(KDSP_pink)

#### Now can plot with only July and Aug data ##PLOTTING ####
ggplot(data = KDSP_pink,
       aes(x = (as.factor(year)),
           y = cpue,
           color = (as.factor(year)))) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom")+
  facet_wrap(~bay)

##going to remove Sanak sites and Kujulik because only sampled in one year##
distinct(KDSP_pink,bay)
KDSP_pink <- filter(KDSP_pink, bay != "Caton Harbor")
KDSP_pink <- filter(KDSP_pink, bay != "NE Harbor")
KDSP_pink <- filter(KDSP_pink, bay != "Kujulik")
KDSP_pink <- filter(KDSP_pink, bay != "ALB")
KDSP_pink <- filter(KDSP_pink, bay != "Chief Cove")
distinct(KDSP_pink,bay)

#### Now can plot with only July and Aug data and only bays sampled many years 
ggplot(data = KDSP_pink,
       aes(x = (as.factor(year)),
           y = cpue,
           color = (as.factor(year)))) +
         geom_boxplot(width = 0.3)+
         geom_jitter(alpha = 0.5)+
         theme(legend.position = "bottom")+
         facet_wrap(~bay)


# set regional codes
unique(KDSP_pink$bay)
KDSP_pink$region <- ifelse(KDSP_pink$bay %in% c("Ugak", "Kiliuda", "Kaiugnak", "Japanese", "Rodmans Reach"),
                    "East side",
                    "Peninsula")
View(KDSP_pink)


ggplot(data = KDSP_pink,
       aes(x = region, y = cpue, color = (as.factor(year))))+
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom")+
  facet_wrap(~year)

####new plots w cpue #######
head(KDSP_pink)
KDSP_pink %>%
  ggplot(aes(x = bay, y = cpue, fill = as.factor(year))) +
  geom_col()+
  theme_minimal()+
  ylab("pink salmon smolt")


KDSP_pink %>%
  ggplot(aes(x = year, y = cpue, fill = as.factor(bay))) +
  geom_col()+
  theme_minimal()+
  ylab("pink salmon smolt")

KDSP_pink %>%
  ggplot(aes(x = year, y = cpue, fill = as.factor(region))) +
  geom_col()+
  theme_minimal()+
  ylab("pink salmon smolt")
