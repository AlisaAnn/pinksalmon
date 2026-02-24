# This is the plotting & analysis script

# Libraries
library(patchwork)

# Load the previous script
source("C:/Users/alask/Documents/Git/pinksalmon/code/LW_data_import.R")

#these data are LW for KNOWN origin fish in 2021 - 2023
#### PLOTTING ####
str(pink.k)
head(pink.k)
pink.k<- filter(pink.k, Year == 2022| Year == 2023)
distinct(pink.k, Year)
#removed 2021 due to samples only in August of that year

plot1 <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), color = Year)) +
  geom_point()+
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2022-2023 by year")
plot1

pink.k$Year <- as.factor(pink.k$Year)
head(pink.k)
##Mikes says stronger to put variable into model instead of 
##do not use/plotting residuals of one model as response variable in another model.
lm_model <- lm(Fultons.K ~ hatch.wild + J_date + Bay, data=pink.k)
lm_model
summary(lm_model)
coef(summary(lm_model))

lm_model <- lm(Fultons.K ~ hatch.wild + J_date, data=pink.k)
lm_model
summary(lm_model)
coef(summary(lm_model))


##### ANALYSIS of weight~length for all years combined #####
lm_model <- lm(Fultons.K ~ hatch.wild, data = pink.k)
lm_model
summary(lm_model)
coef(summary(lm_model))

##poor adjusted R2 = 0.0, n = 857
##residuals appear normally distributed and centered at zero
hist(lm_model$residuals)
shapiro.test(lm_model$residuals)



lm_pink <- lm(Fultons.K ~ Bay, data=pink.k)
summary(lm_pink)
##really low adj R2 = 0.305;  

##below is good LW plot by origin. 
plot3 <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), color = hatch.wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon growth during 2022 - 2023 by origin")
plot3

#####below is best LW plot by origin. 
####use this because 95% Confidence region for regression fit
plot3c <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), color = hatch.wild)) +
  geom_point()+
  stat_smooth(method = "lm", se = T) +
  theme_bw()+
  theme(legend.position = "bottom") +
  theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  theme(legend.background = element_rect(color = "black")) +
  theme(legend.text = element_text(lineheight = 2.8), legend.key.height = unit(1, "cm")) +
  labs(y = "log (body weight)", x = "log (fork length)") +
  labs(color = "  Origin") +
  guides(color = guide_legend(reverse = TRUE))
plot3c
ggsave("./output/lm_pink_LW_2022_3_by_origin.png", width = 6, height = 4, units = 'in')

#####below try again and get the CI to match color. 
####use this because 95% Confidence region for regression fit
plot3d <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), color = hatch.wild)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, aes(fill = hatch.wild))+
  theme_bw()+
  labs(y = "log (Body weight)", x = "log (Fork length)") +
  theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  theme(legend.background = element_rect(color = "black")) +
  theme(legend.text = element_text(lineheight = 2.8), legend.key.height = unit(1, "cm")) +
  theme(legend.title = element_blank())  
 #labs(color = "  Origin") +
  #guides(color = guide_legend(reverse = TRUE))
plot3d
##getting closer but now the legend title is no longer 'Origin"!

pink.k <- pink.k %>%
  mutate("Origin" = "hatch.wild")
head(pink.k)

plot3e <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), group = Origin)) +
  geom_point(aes(color = Origin)) +
  geom_smooth(method = "lm", se = T, aes(fill = Origin, color = Origin))+
  theme_bw()+
  labs(y = "log (body weight)", x = "log (fork length)") +
  theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  theme(legend.background = element_rect(color = "black")) +
  theme(legend.text = element_text(lineheight = 2.8), legend.key.height = unit(1, "cm")) 

plot3e

ggsave("./output/lm_pink_LW_2022_3_by_origin2.png", width = 6, height = 4, units = 'in')


lm_model_origin <- lm(log(weight) ~ log(Length) + hatch.wild, data=pink.k)
lm_model_origin
summary(lm_model_origin)
# above: wild fish weight more at a given length R2 = 0.9773, n = 858

##try some other stats to compare linear lines
lm_model_ori <- lm(log(weight) ~ log(Length) * hatch.wild, data=pink.k)
lm_model_ori
summary(lm_model_ori)
# above: the interaction of length:origin says that no sig diff between wild and hatchery
###the slope of wild is not diff than the slope of hatchery R2 = 0.9773, n = 858
##as of oct 23 this is the best model and the one I will use in short comm.

MuMIn::AICc(lm_model_origin, lm_model_ori)

##now redo plot
pinkkw <- filter(pink.k, hatch.wild =="wild")
pinkkh <- filter(pink.k, hatch.wild =="hatchery")

plot4a <- pink.k %>%
  ggplot(aes(x = Length, y = weight, color = hatch.wild)) +
  geom_point()+
  theme(legend.position = "bottom")+
  theme_bw()+
  labs(title = "Growth curves of known origin pink salmon in 2022-2023")+
  labs(y = "Whole Body Weight (g)", x = "Fork Length (mm)")
plot4a


plot4 <- pink.k %>%
  ggplot(aes(x = Length, y = weight, color = hatch.wild)) +
  geom_point()+
  geom_smooth(method = "loess", data=pinkkh, color = "red")+
  geom_smooth(method = "loess", data=pinkkw, color = "darkgreen")+
  theme(legend.position = "bottom")+
  theme_bw()+
  labs(title = "Growth curves of known origin pink salmon in 2022-2023")+
  labs(y = "Whole Body Weight (g)", x = "Fork Length (mm)")
plot4
ggsave("./output/pink_LW_2022_3_by_origin.png", width = 6, height = 4, units = 'in')

ggplot(data = pink.k,
       aes(x = (as.factor(hatch.wild)),
           y = Fultons.K,
           color = (as.factor(Year)))) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom")


ggplot(data = pink.k,
       aes(x = (as.factor(hatch.wild)),
           y = Fultons.K)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom")+
  facet_wrap(~Bay)

ggplot(data = pink.k,
       aes(x = (as.factor(hatch.wild)),
           y = Fultons.K)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom") +
  labs(title = "Pink salmon Fultons K in 2022-2023")
  
###############
##for known hatchery origin fish, plot length by julian date instead of histograms

ggplot(pinkkh, aes(J_date, Length, color = Year)) +
  geom_point() +
  theme_bw()+
  labs(title = "Length of known hatchery origin pink salmon in 2022-2023")+
  geom_smooth(method = "gam", formula = log(y) ~ s(x, k = 8), se = F)

ggplot(pink.k, aes(J_date, Length, color = hatch.wild)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "gam", formula = log(y) ~ s(x, k = 8), se = F)


plot5 <- pink.k %>%
  ggplot(aes(x = log(Length), y = log(weight), color = hatch.wild)) +
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  labs(title = "Pink salmon LW in 2022 - 2023") +
  facet_wrap(~Year)
plot5
###############################
#############This looks good as a way to show stats in May and Aug 2023
pink.mayaug.resid <- filter(pink.2023resid, Month =="May" | Month == "Aug")

ggplot(data = pink.k,
       aes(x = Month,
           y = Fultons.K,
           color = hatch.wild)) +
  theme_bw()+
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon condition in 2022 and 2023 by Month and origin")


#birch asked which hatchery they are from . Maybe pws fish older bc released earlier and that explains diff
#in size by month. 
pink.hatchery <- filter(pink.k, hatch.wild == "hatchery")
distinct(pink.hatchery,hatch.wild)

ggplot(data = pink.hatchery,
       aes(x = Month,
           y = Length,
           color = Year)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon hatchery origin, length by month 2022 - 2023")

ggplot(data = pink.hatchery,
       aes(x = Month,
           y = Length,
           color = hatch.site)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  labs(title = "Pink salmon hatchery origin, length by month color for hatchery")
#this plot shows that the larger fish in august are from kitoi, so it is not that they are older PWS fish


###below here are all previous script. not sure if they apply
############ try length freq histograms ########

ggplot(data = pink.k,
       aes(x = Length, fill = Year))+
  geom_histogram(alpha = 0.5)+
  theme_minimal()

ggplot(data = pink.k,
       aes(x = Length, fill = hatch.wild))+
  geom_histogram(alpha = 0.5,binwidth = 1)+
  theme_minimal() +
  labs(title = "Length of known origin pink salmon in 2022-2023")+
  labs(y = "Count", x = "Fork Length (mm)")

ggsave("./output/pink_LF_2022_3_by_origin.png", width = 6, height = 4, units = 'in')

#now going to try to make LF as an inset into the LW regression plot called plot 3c
#will remove title and change legend to LF plot first
lf <- ggplot(data = pink.k,
             aes(x = Length, fill = hatch.wild))+
  geom_histogram(alpha = 0.5,binwidth = 1)+
  theme_classic() +
  #theme(panel.border = element_rect(color = "black"), fill = NA, size = 2) +
  theme(legend.position = c(0.7,.8))+
  theme(legend.title = element_blank()) +
  #theme(legend.background = element_rect(color = "black")) +
  labs(y = "log (body weight)", x = "log (fork length)") +
  labs(fill = "Origin") +
  guides(fill = guide_legend(reverse = FALSE)) +
  #scale_fill_discrete(guide = FALSE) +
  labs(y = "Count", x = "Fork Length (mm)")
plot(lf)

plot3d
final.plot <- plot3d + inset_element(lf, left = 0.6, bottom = 0.009, right = 0.98, top = 0.45)
final.plot  #note that w=6, H=4 the regression lines look blurred and the same. Increase Height.
ggsave("./output/pink_LF_overlay_LW_regress.png", width = 6, height = 6, units = 'in')



ggplot(data = pink.k,
       aes(x = Length, fill = Month))+
  geom_histogram(alpha = 0.5)+
  theme_minimal()

ggplot(data = pink.k,
       aes(x = Length, fill = Year))+
  geom_density(alpha = 0.5)+
  theme_minimal()

ggplot(data = pink.k,
       aes(x = Length, fill = Month))+
  geom_density(alpha = 0.5)+
  labs(title = "pink salmon smolt 2022 - 2023")+
  theme_minimal()


ggplot(data = pink.k,
       aes(x = Length, fill = hatch.wild))+
  geom_density(alpha = 0.5)+
  labs(title = "pink salmon smolt 2022 - 2023")+
  theme(legend.position = "bottom")+
  facet_wrap(~Year)

ggplot(data = pink.k,
       aes(x = Length, fill = Year))+
  geom_density(alpha = 0.5)+
  labs(title = "pink salmon smolt 2022 - 2023")+
  theme(legend.position = "bottom")+
  facet_wrap(~hatch.wild)


###############
# Read in the hatchery data (2022 - 2023) with LW and capture site
## and rename any columns that need renaming
pink.dist <- read_csv("data/hatch_dist_fulton.csv")

head(pink.dist)
str(pink.dist)
tail(pink.dist)
##to check that last rows are real data. if need to remove these 3 rows
lm_model_dist <- lm(Fultons.K ~ as.numeric(km_travel) + as.numeric(days_at_sea),
                    data = pink.dist)

lm_model_dist
summary(lm_model_dist) 


ggplot(data = pink.dist,
       aes(x = as.numeric(km_travel),
           y = Fultons.K)) +
        geom_jitter(alpha = 0.5)
#6 rows removed for missing data. you can see there's no pattern

ggplot(data = pink.dist,
       aes(x = as.numeric(days_at_sea),
           y = Fultons.K)) +
  geom_jitter(alpha = 0.5)
#6 rows removed for missing data. you can see there's no pattern 

ggplot(data = pink.dist,
       aes(x = as.numeric(km_travel),
           y = as.numeric(days_at_sea))) +
  geom_jitter(alpha = 0.5)
#6 rows removed for missing data. you can see there's no pattern
#days at sea 30-60 days travel all distances.

ggplot(data = pink.dist,
       aes(x = as.numeric(days_at_sea),
           y = Length)) +
  geom_jitter(alpha = 0.5)
#Just shows that the longest fish >80 mm were at sea the longest. but possible in ALB whole time

ggplot(data = pink.dist,
       aes(x = as.numeric(km_travel),
           y = Length, color = hatch.area)) +
  geom_point(size = 3.0)+
  geom_jitter(alpha = 3.5)+
  theme_bw()+
  labs(title = "Distance traveled and size of hatchery pink salmon smolt", 
       y = "Fork length (mm)", x = "Distance Traveled (km)")
#need to remove NA and also how to jitter so see all points (even overlapping ones)  
#6 rows removed for missing data because these were unknown hatchery site

pink.dist.na <- filter(pink.dist, !is.na(hatch.area))
head(pink.dist.na)
ggplot(data = pink.dist.na,
       aes(x = as.numeric(km_travel),
           y = Length, shape = as.factor(Year), color = hatch.area)) +
  geom_point(position = position_dodge(0.2), size = 3.5)+
  theme_bw()+
  labs(color = "Hatchery Area", shape = "Year") +
  labs(title = "Distance traveled and size of hatchery pink salmon smolt", 
       y = "Fork length (mm)", x = "Distance Traveled (km)") 

ggsave("./output/hatch_pink_dist_len.png", width = 6, height = 4, units = 'in')

##add label with number of days at sea
install.packages("ggrepel")
library(ggrepel)  #installed package but couldn't get geom_text_repel to run

ggplot(data = pink.dist.na,
       aes(x = as.numeric(km_travel),
           y = Length, shape = as.factor(Year), color = hatch.area)) +
  geom_point(position = position_dodge(0.7), size = 3.5)+
  theme_bw()+
  labs(color = "Hatchery Area", shape = "Year") +
  labs(title = "Distance traveled and size of hatchery pink salmon smolt", 
       y = "Fork length (mm)", x = "Distance Traveled (km)") +
  geom_text(aes(x = as.numeric(km_travel) + 30, label = days_at_sea), size = 3, vjust = 2.2)
ggsave("./output/hatch_pink_dist_len_annote_daysatsea.png", width = 6, height = 4, units = 'in')
