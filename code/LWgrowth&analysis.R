# This is the plotting & analysis script from Ingrid to compare exponential growth curves
# Ingrid used it for cod age-0 genetics 5/21/25

# Libraries
library(patchwork)

# Load the previous script
source("C:/Users/alask/Documents/Git/pinksalmon/code/LW_data_import.R")

#### PLOTTING ####
head(pinkLW)
str(pinkLW)

library(ggplot2)
pink23 <- filter(pinkLW, Year ==2023)
distinct(pink23,Year)
distinct(pink23,hatch.wild)
pink23 <- filter(pink23, hatch.wild !='unknown')
distinct(pink23,hatch.wild)
#so now, pink23 is only 2023 and only the ones we know are hatchery or wild)

#might need to make hatch.wild into factor instead of character?
#if so, use this <pink23$hatch.wild=as.factor(pinke23$hatch.wild)

ggplot(data=pink23)+geom_point(aes(x=Length,y=weight,col=hatch.wild),size=3)

model1= nls(weight ~ a * Length^b, data = pink23[which(pink23$hatch.wild=='hatchery'),], start = list(a = 1, b = .1))
model2= nls(weight ~ a * Length^b, data = pink23[which(pink23$hatch.wild=='wild'),], start = list(a = 1, b = .1))

model1_lm= lm(weight ~ Length, data = pink23[which(pink23$hatch.wild=='hatchery'),])
model2_lm= lm(weight ~ Length, data = pink23[which(pink23$hatch.wild=='wild'),])

summary(model1)

length=seq(20,120,1)

a1=summary(model1)$parameters[1,1]
b1=summary(model1)$parameters[2,1]
a2=summary(model2)$parameters[1,1]
b2=summary(model2)$parameters[2,1]

a1_lm=as.numeric(model1_lm$coefficients[1])
b1_lm=as.numeric(model1_lm$coefficients[2])
a2_lm=as.numeric(model2_lm$coefficients[1])
b2_lm=as.numeric(model2_lm$coefficients[2])

anova(model1,model2)
#anova(model1_lm,model2_lm)  error in anova.lmlist as models not all fitted to same size

library(FlexParamCurve)
extraF.nls(model1, model2)
#extraF.nls(model1_lm, model2_lm)
anova(model1,model2)

Line1=a1 * pink23$Length^b1
Line2=a2*pink23$Length^b2

Line_lm1=a1_lm+b1_lm*pink23$Length
Line_lm2=a2_lm+b2_lm*pink23$Length

pink23$Line1=Line1
pink23$Line2=Line2
pink23$Line1_lm=Line_lm1
pink23$Line2_lm=Line_lm2
head(pink23)
#colnames(pink23)=c("weight","Length","hatch.wild","Line1","Line2","Line1_lm","Line2_lm")



ggplot(data=pink23)+geom_point(aes(x=Length,y=weight,col=hatch.wild),size=3)+
  geom_line(aes(x=Length,y=Line1),col="red")+
  geom_line(aes(x=Length,y=Line2),col="darkgreen")+
  xlab("Length (mm)")+ylab("weight (g)")+
  guides(theme(legend.key = element_rect(fill = "white")))

ggplot(data=pink23)+geom_point(aes(x=Length,y=weight,col=hatch.wild),size=4)+
  geom_line(aes(x=Length,y=Line1_lm),col="red")+
  geom_line(aes(x=Length,y=Line2_lm),col="darkgreen")+
  xlab("Length (mm)")+ylab("weight (g)")+
  guides(theme(legend.key = element_rect(fill = "white")))


#model <- lm(weight ~ Length * hatch.wild, data = pink23)
#interaction <- interaction(model)
#interaction_pvalue <- interaction$interaction_pvalue

library(lsmeans)
#lsmeans(model, "Gentype")

# Compare slopes using lsmeans::lstrends (if needed)
#Gentype lsmean   SE df lower.CL upper.CL
#1         98.9 1.28 41     96.4      102
#2         96.2 1.38 41     93.4       99

#anova(model <- lm(Age ~ Length * Gentype, data = A3))

#> pars=matrix(c(a1,a2,b1,b2),2,2)
#> colnames(pars)=c("a","b")
#> rownames(pars)=c("Gentype 1","Gentype 2")

#Agez$Gentype=as.factor(Agez$Gentype)
#ggplot(data=Agez)+geom_point(aes(x=TL_mm,y=predicted.age,col=Gentype),size=2)

#ggplot(data=Agez)+geom_point(aes(y=TL_mm,x=predicted.age,col=as.factor(year)),size=2)

#t.test(Agez$predicted.age[which(Agez$Gentype==1)],Agez$predicted.age[which(Agez$Gentype==2)])

#t.test(Agez$Date.Captured[which(Agez$Gentype==1)],Agez$Date.Captured[which(Agez$Gentype==2)])

ggplot(data=pink23)+geom_boxplot(aes(x=hatch.wild,y=Length))
t.test(pink23$Length[which(pink23$hatch.wild=="hatchery")],pink23$Length[which(pink23$hatch.wild=="wild")])

#now t test by month may or august
pink23.may <- filter(pink23,Month == 'May')
pink23.aug <- filter(pink23, Month =="Aug")
t.test(pink23.may$weight[which(pink23.may$hatch.wild=="hatchery")],pink23.may$weight[which(pink23.may$hatch.wild=="wild")])
ggplot(data=pink23.may)+geom_boxplot(aes(x=hatch.wild,y=weight))

t.test(pink23.aug$weight[which(pink23.aug$hatch.wild=="hatchery")],pink23.aug$weight[which(pink23.aug$hatch.wild=="wild")])
ggplot(data=pink23.aug)+geom_boxplot(aes(x=hatch.wild,y=weight))

##but I don't want to test the length difference, I want to test the LW residual difference
##changed code so now t test of weight, and it is better. 
#it's interesting because the august wild fish are not as long, but they weigh more
#and the may wild fish are more variable in length but they don't weigh as much as hatchery fed fish