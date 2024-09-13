# This is the data import script

library(tidyverse)
library(lubridate)



# Read in the data and rename any columns that need renaming
pinkLW <- read_csv("data/Pink_LW.csv")
head(pinkLW)


tail(pinkLW)
##to check that last rows are real data. if need to remove these 3 rows
pinkLW <- filter(pinkLW, !is.na(Length))
head(pinkLW)
tail(pinkLW) #you can see it worked 

#need to remove years 2017 and 2021
#codcond1<-filter(codcond1,year=="2018"|year=="2019" | year == "2020")
#codcond1
#distinct(codcond1, year)


distinct(pinkLW,Year) #years 2021 - 2024 
distinct(pinkLW,Month)
# When we boxplot this, we notice that the factors in month are alphabetical. 
#we want them to be sequential, so need to create a new column "Month" w ordered factor
pinkLW <- mutate(pinkLW, Month = fct_relevel(Month, c("May", "June", "July", "Aug")))
# We use fct_relevel to explicitly define the factor order for column "Month" 

str(pinkLW) # we can see now that the factors are ordered correctly 

##  CAN ADD CPUE DATA FOR PINKS NEXT
#PC <- read_csv("data/pink_cpue.csv")
#head(PC)
#distinct(PC, bay)
#distinct(PC,year)
#distinct(PC,month)
#tail(PC)

