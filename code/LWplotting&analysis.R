# This is the plotting & analysis script

# Libraries
library(patchwork)

# Load the previous script
source("C:/Users/alask/Documents/Git/pinksalmon/code/LW_data_import.R")


#### PLOTTING ####
str(pinkLW)
names(pinkLW)[11] <- "Wgt"
names(pinkLW)[12] <- "hatcher_wild"

plot1 <- pinkLW %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = Month)) +
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

##Mikes says stronger to put variable into model instead of 
##do not use/plotting residuals of one model as response variable in another model.
lm_model <- lm(log(Wgt) ~ log(Length) + Year, data=pinkLW)
lm_model
summary(lm_model)
coef(summary(lm_model))
##find out how to extract the estimate for 2021 and can plot that.


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

pink2 <- filter(pinkLW, certain == "y")
distinct(pink2, hatcher_wild)

#lm_pink2 <- lm(resid ~ hatcher_wild, data=pink2)
#summary(lm_pink2)
##n=941 but R2 very low

##log length and origin and compare w and w/o year
#because we know there were strong year effects (from above)

lm_model_origin <- lm(log(Wgt) ~ log(Length) + hatcher_wild + Year, data=pink2)
lm_model_origin
summary(lm_model_origin)

lm_model_origin2 <- lm(log(Wgt) ~ log(Length) + hatcher_wild, data=pink2)
lm_model_origin2
summary(lm_model_origin2)
#now do AICc to see which model is better
distinct(pink2, Year)
str(pink2)
summary <- pink2 %>%
  select(Month, Year, hatcher_wild, Wgt) %>%
  group_by(Year, Month, hatcher_wild) %>%
  summarise(count = n())

summary
#so drop 2024 because only May wild
pink3 <- filter(pink2, Year == "2021" | Year == "2022" | Year == "2023")
distinct(pink3, Year)

plot3 <- pink3 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021-2023 by year")
plot3


lm_model_origin3 <- lm(log(Wgt) ~ log(Length) + hatcher_wild, data=pink3)
lm_model_origin3
summary(lm_model_origin3)
# above: wild fish are bigger

##below is the plot for the paper
plotP3 <- pink3 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Body condition of known origin pink salmon in 2021-2023")+
  labs(y = "Log Whole Body Weight", x = "Log Fork Length")
plotP3
summary(lm_model_origin3)


#here decide to look only at 2023 because that was only year with all months
pink4 <- filter(pink2, Year == "2023")
distinct(pink4, Year)

plot4 <- pink4 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2023")
plot4

lm_model_origin4 <- lm(log(Wgt) ~ log(Length) + hatcher_wild*Month, data=pink4)
lm_model_origin4
summary(lm_model_origin4)
##of course we know length increases with month, so remove month interaction
lm_model_origin5 <- lm(log(Wgt) ~ log(Length) + hatcher_wild, data=pink4)
lm_model_origin5
summary(lm_model_origin5)

###############
##plot length by julian date instead of histograms, as per Mike's idea

ggplot(pinkLW, aes(J_date, Length, color = Year)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "gam", formula = log(y) ~ s(x, k = 8), se = F)

ggplot(pink4, aes(J_date, Length, color = hatcher_wild)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "gam", formula = log(y) ~ s(x, k = 8), se = F)

distinct(pink3,Year)

distinct(pink3,hatcher_wild) 
#use pink3 since only known origin fish, and 2021-2023 (2024 removed)

mod1 <- lm(log(Wgt) ~ log(Length) + J_date + Year + hatcher_wild, data = pink3)
mod2 <- lm(log(Wgt) ~ log(Length) + J_date + Year, data = pink3)
mod3 <- lm(log(Wgt) ~ log(Length) + J_date, data = pink3)

AIC(mod1,mod2,mod3) # lowest value for AIC is best fit, and mod1 has lowest AIC
summary(mod1)

plot5 <- pink3 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021 - 2023") +
  facet_wrap(~Year)
plot5
###########this makes me think I should take out 2021
pink5 <- filter(pink3, Year != "2021")
distinct(pink5,Year)
#now pink 5 is only years 2022 and 2023 and only known origin

mod1 <- lm(formula=log(Wgt) ~ log(Length) + Year + hatcher_wild, data = pink5)
mod2 <- lm(log(Wgt) ~ log(Length) + Year, data = pink5)
#mod3 <- lm(log(Wgt) ~ log(Length) + Year:hatcher_wild, data = pink5)
mod4 <- lm(log(Wgt) ~ log(Length), data = pink5)
AIC(mod1,mod2,mod4) # lowest value for AIC is best fit, and mod3 has lowest AIC

summary(mod3)
#1 coefficient not defined because of singularities. remember we have no samples
#in 2022 of may fish from hatchery. cannot rally run model 3. therefore do not use mod3
#next best model based on AIC is mod1

AIC(mod1,mod2,mod4) # model 1 is lowest (but not by a lot)
summary(mod1)
library(visreg)
visreg(mod1)

plot6 <- pink5 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2022 - 2023") +
  facet_wrap(~Year)
plot6
###########################################
##########making plot here to show LW diff by origin and month
###stats to go with this figure are below
pink.mayaug <- filter(pink5, Year == "2023")
pink.mayaug <- filter(pink.mayaug, Month == "May" | Month == "Aug")
distinct(pink.mayaug,Year)
distinct(pink.mayaug,Month) ##pink.mayaug is for months may and only in  2023

plotMA23 <- pink.mayaug %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2023") +
  facet_wrap(~Month)
plotMA23

##now the stats are below


pink.may <- filter(pink5, Month == "May")
pink.may <- filter(pink.may, Year == "2023")
distinct(pink.may,Year)
distinct(pink.may,Month)
mod.may <- lm(formula=log(Wgt) ~ log(Length) + hatcher_wild, data = pink.may)
summary(mod.may)
visreg(mod.may)
#this shows that in 2023, May, the hatchery fish are heavier at length

#now plot august only only in 2023
pink.aug <- filter(pink5, Month == "Aug")
pink.aug <- filter(pink.aug, Year == "2023")
distinct(pink.aug,Year)
distinct(pink.aug,Month)
mod.aug <- lm(formula=log(Wgt) ~ log(Length) + hatcher_wild, data = pink.aug)
summary(mod.aug)
visreg(mod.aug)
#wild fish in august 2023 had higher weight!

#now need to combine into one model and see if interaction
#but tried and see that unequal sample size in May and AUg makes it not possible to look at interaction
#better to run the 2 separate LM for 2023 above with May and with Aug to see if diff in hatch/wild
pink.earlylate <- filter(pink5, Month == "May" | Month == "Aug")
pink.earlylate <- filter(pink.earlylate, Year == "2023")
distinct(pink.earlylate,Year)
distinct(pink.earlylate,Month)
mod.2023EL <- lm(formula=log(Wgt) ~ log(Length) + Month*hatcher_wild, data = pink.earlylate)
summary(mod.2023EL)
mod.2023EL$coefficients
#visreg(mod.2023EL)
#this shows that in 2023, May, the hatchery fish are heavier at length

#distinct(pink3,Year)
##### ANALYSIS of weight~length for all 2023 by Month * origin #####
#pink.2023 <- filter(pink3, Year == "2023")
#lm_model.2023 <- lm(log(Wgt) ~ log(Length), data=pink.2023)
#lm_model.2023

#summary(lm_model.2023)
##very good adjusted R2 = 0.9831, n = 458
##residuals appear normally distributed and centered at zero
#hist(lm_model.2023$residuals)
#shapiro.test(lm_model.2023$residuals)
#resid <- resid(lm_model.2023)

##now want to test if residuals of LW relationship from 2023 vary by origin in month
##first need to add residuals to datafile  
#head(pink.2023)
#pink.2023resid <- mutate(pink.2023, resid)
#head(pink.2023resid)  

#lm_pink <- lm(resid ~ Month * hatcher_wild, data=pink.2023resid)
#summary(lm_pink)
######## above with only year 2023. model not good.



distinct(pink2,Year)

plot15 <- pink2 %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2021 - 2024 by Month") +
  facet_wrap(~Month)

plot15
plot16 <- pink.2023resid %>%
  ggplot(aes(x = log(Length), y = log(Wgt), color = hatcher_wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2023 by Month") +
  facet_wrap(~Month)

plot16

ggplot(data = pink.2023resid,
       aes(x = Month,
           y = resid,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon mean length by month 2023")

###############################
#############This looks good as a way to show stats in May and Aug 2023
pink.mayaug.resid <- filter(pink.2023resid, Month =="May" | Month == "Aug")

ggplot(data = pink.mayaug.resid,
         aes(x = Month,
             y = resid,
             color = hatcher_wild)) +
  theme_bw()+
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon condition in May and August 2023 by origin")




ggplot(data = pink.2023,
       aes(x = Month,
           y = Length,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon mean length by month 2023")

ggplot(data = pink.2023,
       aes(x = Month,
           y = Wgt,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon mean length by month 2023")






#lm_pink2_may <- lm(resid ~ hatcher_wild, data=pink2.may)
#summary(lm_pink2_may)
#n = 714, low R2, sig dif that wild + resid and hatchery - residuals

#pink2.julyAug <- filter(pink2, Month == "July" | Month == "Aug")
#lm_pink2_julyAug <- lm(resid ~ hatcher_wild, data=pink2.julyAug)
#summary(lm_pink2_julyAug)
#n=209, not significant overall in July/Aug between hatchery and wild 

distinct(pink2,Year)
distinct(pink2,hatcher_wild)
View(pink2)

ggplot(data = pink2,
       aes(x = Month,
           y = Length,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon mean length by month 2021 - 2024")

#########Plot below is best because 2021-2023. omit 2024 as only has may wild data. did not get May pinks from kitoi
#try this plot with only 2021, 2022 and 2023 due to sample unbalance
distinct(pink3,Year)
distinct(pink3,hatcher_wild)
head(pink3)

ggplot(data = pink3,
       aes(x = Month,
           y = Length,
           color = hatcher_wild)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_bw() +
  labs(title = "Pink salmon length by month 2021 - 2023", y = "Fork length (mm)")

###March 25,2025 use this above plot for paper, although it looks the same as 2021 - 2024
len.origin.month <- lm(formula= Length ~ Month * hatcher_wild, data = pink3)
summary(len.origin.month)


#birch asked which hatchery they are from . Maybe pws fish older bc released earlier and that explains diff
#in size by month. 
pink3.hatchery <- filter(pink3, hatcher_wild == "hatchery")
distinct(pink3.hatchery,hatcher_wild)

ggplot(data = pink3.hatchery,
       aes(x = Month,
           y = Length,
           color = Year)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon hatchery origin, length by month 2021 - 2023")

ggplot(data = pink3.hatchery,
       aes(x = Month,
           y = Length,
           color = hatch.site)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon hatchery origin, length by month color for hatchery")
#this plot shows that the larger fish in august are from kitoi, so it is not that they are older PWS fish


##should I smooth by julian date? unsure. 


#pink2.Aug <- filter(pink2, Month == "Aug")
#lm_pink2_Aug <- lm(resid ~ hatcher_wild, data=pink2.Aug)
#summary(lm_pink2_Aug)
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

lm_model3 <- lm(log(Wgt) ~ log(Length) + Year, data=pinkLW)
lm_model3
summary(lm_model3)



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
  labs(title = "pink salmon smolt 2021 - 2024")+
  theme_minimal()

##same plot but with years 2021 - 2023.   year 2024 removed
##use this below plot for paper as Figure 2
distinct(pinkLW,Year)
pinkLW2123 <- filter(pinkLW, Year == 2021 | Year == 2022 | Year == 2023)
distinct(pinkLW2123,Year)

ggplot(data = pinkLW2123,
       aes(x = Length, fill = Month))+
  geom_density(alpha = 0.5)+
  labs(title = "Pink salmon smolt 2021 - 2023 (n = 1364)", x = "Fork Length (mm)") +
  theme_bw()
##


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
  
  ###############Growth exponent for hatchery fish 2021  2022 and 2023
  head(pinkgrow)
  str(pinkgrow)
  distinct(pinkgrow,Year)
  growth.model <- mgcv::gam(growth_rate ~ Year + Area, data = pinkgrow)
  growth.model
summary(growth.model) 


ggplot(data = pinkgrow,
       aes(x = Area,
           y = growth_rate,
           color = Year)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)
