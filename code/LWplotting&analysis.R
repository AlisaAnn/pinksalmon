# This is the plotting & analysis script

# Libraries
library(patchwork)

# Load the previous script
source("C:/Users/alask/Documents/Git/pinksalmon/code/LW_data_import.R")


#### PLOTTING ####
str(pinkLW)
names(pinkLW)[10] <- "Wgt"
names(pinkLW)[11] <- "hatcher_wild"

plot1 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = Month)) +
  geom_point()+
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021-2024 by month")
plot1

pinkLW$Year <- as.factor(pinkLW$Year)
plot2 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = Year)) +
  geom_point()+
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021-2024 by year")
plot2

##### ANALYSIS of plot 1 for all 4 years #####
lm_model <- lm(Wgt ~ Length + Year, data = pinkLW)
lm_model

summary(lm_model)

##more scatter plots##
plot3 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = hatcher_wild)) +
  geom_point()+
  theme(legend.position = "bottom")+
  labs(title = "Pink Salmon LW 2021 - 2024")
plot3

distinct(pinkLW,Year)
distinct(pinkLW,hatcher_wild)
distinct(pinkLW,Location)

plot3 <- pinkLW %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = Year)) +
  geom_point()+
  geom_smooth(method = "gam")+
  theme(legend.position = "bottom")+
  labs(title = "Pink Salmon LW by year")

plot3

plot4 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = Year)) +
  geom_point()+
  geom_smooth(method = "gam", color = "black")+
  theme(legend.position = "bottom")+
  labs(title = "Pink Salmon LW by year")

plot4


plot5 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "gam", color = "black")+
  theme(legend.position = "bottom")+
  labs(title = "Pink Salmon LW by year")

plot5

##### ANALYSIS of plot 3 for 2021 separate from 2022 #####
### probably don't want this model, tho, because exponential curve 
###and this is a linear model.
#pinkLW2021 <- filter(pinkLW,Year=="2021")
#lm_model2021 <- lm(Wgt_g ~ FL_mm, data = pinkLW2021)
#lm_model2021

#summary(lm_model2021)


#pinkLW2022 <- filter(pinkLW,Year=="2022")
#lm_model2022 <- lm(Wgt_g ~ FL_mm, data = pinkLW2022)
#lm_model2022

#summary(lm_model2022)

##### more plots #####

ggplot(data = pinkLW,
       aes(x = Length,
           y = Wgt,
           color = Year,
           shape = Month)) +
  geom_point(size = 1.5,
             alpha = 0.8)
##are pinks bigger in july 2021 and not present in aug 2022
## are 2021 july pinks heavier?

ggplot(data = pinkLW,
       aes(x = Month,
           y = Length,
           color = Year)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)

ggplot(data = pinkLW,
       aes(x = Month,
           y = Length,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)

##going to make a pivot table (temp) to see the count (n) of different samples in each month/year
temp <- pinkLW %>%
  group_by(Year, Month) %>%
  summarise(n())
temp
View(temp)

############ try length freq histograms ########
ggplot(data = pinkLW,
       aes(x = Length, fill = Year))+
geom_histogram(alpha = 0.5)+
  theme_minimal()

ggplot(data = pinkLW,
       aes(x = Length, fill = Month))+
  geom_histogram(alpha = 0.5)+
  theme_minimal()

ggplot(data = pinkLW,
       aes(x = Length, fill = Year))+
  geom_density(alpha = 0.5)+
  theme_minimal()

ggplot(data = pinkLW,
       aes(x = Length, fill = Month))+
  geom_density(alpha = 0.5)+
  labs(title = "pink salmon smolt 2021 - 2024")
  theme_minimal()

  ggplot(data = pinkLW,
         aes(x = Length, fill = Month))+
    geom_density(alpha = 0.5)+
    labs(title = "pink salmon smolt 2021 - 2024")+
    theme(legend.position = "bottom")+
    facet_wrap(~Year)

  ggplot(data = pinkLW,
         aes(x = Length, fill = Month))+
    geom_density(alpha = 0.5)+
    labs(title = "pink salmon smolt 2021 - 2024")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
 
##do not need May data LF to clutter plot
  ##know that all May fish are wild, so will not choose those for otoliths
  ##plot June, July, Aug only
  
 # pink_woMay <- filter(pinkLW, Month != "May")
  #ggplot(data = pink_woMay, 
   #      aes(x = FL_mm, fill = Month))+
    #geom_density(alpha = 0.5)+
    #labs(title = "pink salmon smolt 2021 and 2022")+
    #theme(legend.position = "bottom")+
    #facet_wrap(~Location)  
  
  ##do not need May data LF to clutter plot
  ##know that all May fish are wild, so will not choose those for otoliths
  ##plot June, July, Aug only and also only 2022
  
#  pink_woMay <- filter(pinkLW, Month != "May", Year == 2022)
#  ggplot(data = pink_woMay, 
     #    aes(x = FL_mm, fill = Month))+
    #geom_density(alpha = 0.5)+
   # labs(title = "pink salmon smolt 2022")+
  #  theme(legend.position = "bottom")+
 #   facet_wrap(~Location) 
  
  #wonder if there were any pinks in Kaiugnak
#  temp <- pink_woMay %>%
    #group_by(Year, Month, Location) %>%
   # summarise(n())
  #temp
  
  #sample size for no May and only 2022 by Bay
#  temp <- pink_woMay %>%
 #   group_by(Month, Location) %>%
#    summarise(n())
 # temp
  
 # View(temp)
  
######hold with 2024 analysis#######
#################  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "Cook")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_density(alpha = 0.5)+
    labs(title = "pink salmon smolt Cook Bay 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location) 
  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "Agripina")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_histogram(alpha = 0.5)+
    labs(title = "pink salmon smolt Agripina Bay 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "ALB")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_histogram(alpha = 0.5)+
    labs(title = "pink salmon smolt Anton Larsen Bay 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "Fox")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_histogram(alpha = 0.5)+
    labs(title = "pink salmon smolt Fox Bay 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "Mitrofania")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_histogram(alpha = 0.5)+
    labs(title = "pink salmon smolt Mitrofania Bay 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
  
  ##select bays with large number pinks; May and 2021 excluded ##
  pink_bays_2022 <- filter(pink_woMay, Location == "Port Wrangall")
  View(pink_bays_2022)
  
  ggplot(data = pink_bays_2022, 
         aes(x = FL_mm, fill = Month))+
    geom_histogram(alpha = 0.5)+
    labs(title = "pink salmon smolt Pt Wrangell 2022")+
    theme(legend.position = "bottom")+
    facet_wrap(~Location)
 