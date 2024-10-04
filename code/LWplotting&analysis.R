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

##### ANALYSIS of weight~length for all years combined #####
lm_model <- lm(log(Wgt) ~ log(Length), data=pinkLW)
lm_model

summary(lm_model)
##very good adjusted R2 = 0.9794, n = 1681
##residuals appear normally distributed and centered at zero
hist(lm_model$residuals)
shapiro.test(lm_model$residuals)
resid <- resid(lm_model)

##now want to test if residuals of LW relationship vary by bay, year, or bay/year
##first need to add residuals to datafile  

pink1 <- mutate(pinkLW, resid)
head(pink1)  
  
lm_pink <- lm(resid ~ Year + Bay, data=pink1)
summary(lm_pink)
###stop here for now. something not right that so many positive estimates for bay but neg for year
#maybe julian day is more important than year.

lm_pink <- lm(resid ~ Year, data=pink1)
summary(lm_pink)
##really low adj R2 = 0.0869;  2021 more positive than 2022, 2023, or 2024


###Isolate known hatchery/wild pinks. certainity is yes
## now test if LW residuals are diff between hatch/wild origin 
distinct(pink1, hatcher_wild)
pink2 <- filter(pink1, certain == "y")
distinct(pink2, hatcher_wild)

lm_pink2 <- lm(resid ~ hatcher_wild, data=pink2)
summary(lm_pink2)
##n=941 but R2 very low

pink2.may <- filter(pink2, Month == "May")
lm_pink2_may <- lm(resid ~ hatcher_wild, data=pink2.may)
summary(lm_pink2_may)
#n = 714, low R2, sig dif that wild + resid and hatchery - residuals

pink2.julyAug <- filter(pink2, Month == "July" | Month == "Aug")
lm_pink2_julyAug <- lm(resid ~ hatcher_wild, data=pink2.julyAug)
summary(lm_pink2_julyAug)
#n=209, not significant overall in July/Aug between hatchery and wild 


ggplot(data = pink2,
       aes(x = Month,
           y = Length,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)

#this plot looks like no diff in July but yes, hatchery surpass wild by august.
#let's test it here

pink2.Aug <- filter(pink2, Month == "Aug")
lm_pink2_Aug <- lm(resid ~ hatcher_wild, data=pink2.Aug)
summary(lm_pink2_Aug)
#smaller sample size n = 38, low R2, and p not significant.







###below here are all previous script. not sure if they apply


plot2 <- pinkLW %>%
  ggplot(aes(x = Length, y = Wgt, color = Year)) +
  geom_point()+
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021-2024 by year")
plot2


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
 
  
  ggplot(data = pinkLW,
         aes(x = Length, fill = Month))+
    geom_density(alpha = 0.5)+
    labs(title = "pink salmon smolt 2021 - 2024")+
    theme(legend.position = "bottom")+
    facet_wrap(~hatcher_wild)
  
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
 