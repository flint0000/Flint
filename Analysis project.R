#House keeping

rm(list=ls())
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
library(nlme)
library(MASS)
library(tidyverse)
library(ggplot2)
library(nlme)
library(knitr)
library(kableExtra)
library(expss)
library(pander)
library(stargazer)
library(dplyr)
library(gmodels)
library(xtable)
library(survMisc)


#Load the data

setwd("C:/Users/user/Desktop/flint/MSc Statistics/GLM 503/Project")
thedat <- read.table("toenail.dat",row.names=NULL)

dim(thedat)
#The original study consisted of 1200 observations and 6 variables
head(thedat)
#The original variables are not properly named and the values for categorical variables are not labeled
summary(thedat)
#from the looks of things, V2, V3 and V6 are binary variables.


#Data manupulation 

#renaming variables
colnames(thedat) <- c("id","healthclub","gender","month","length","trt")
dat<-thedat
head(thedat)

length(unique(thedat$id))

unique(thedat$healthclub) # 0 1
unique(thedat$gender)     # 1 0
unique(thedat$month)      # 0 1  2  3  6 12
unique(thedat$trt)        # 1 2   


#adding labels to variable values
thedat<-thedat %>%
  mutate(Treatment=ifelse(trt==1,"terbinane","itraconazole")) %>%
  mutate(Health_club=ifelse(healthclub==1,"more than once a week","once a week or less")) %>%
  mutate(Gender=ifelse(gender==1,"Male","Females")) %>%
  mutate(Month=ifelse(month==0, "Base month", 
                      ifelse(month==1,"1st month",
                             ifelse(month==2,"2nd month", 
                                    ifelse(month==3,"3rd month",
                                           ifelse(month==6,"6th month", "12th month"))))))

dat1<-                       
  filter(thedat, Month=="Base month"| Month=="12th month")
unique(dat1$Month)

data_wide <- reshape(dat1, idvar = "id", timevar = "Month", direction = "wide")
summary(data_wide)
data_wide<-data_wide %>%
  mutate(differance=`length.12th month`- `length.Base month`) %>%
  mutate(success=ifelse(differance>=5,"Success","Failure"))

count1<-data_wide %>%
  count(success) %>%
  mutate("%" = n / sum(n)*100)

count1  %>%   
  kable("html" , caption = 'Outcome Descriptives') %>%   
  kable_styling()



#Graphical visualization
p1<-ggplot(data=thedat, aes(x=month,y=length,group=id),alpha=0.90)+
  geom_point(size=2,shape=18,color='navy blue') +
  geom_line(size=1,color='navy blue') +
  facet_wrap(~Treatment, nrow=1) +
  theme(strip.text.x = element_text(size =15, colour = "navy blue")) +
  scale_x_continuous("Time (Month)",limits=c(0,12),breaks=thedat$month) +
  scale_y_continuous("Length (mm)",limits=c(0,20))
p1

#Analytical 
#table7<- data_wide %>%  table(`Treatment.12th month`, success) 

table1<- 
  table(data_wide$`Treatment.12th month`, data_wide$success) 

df <-
  data.frame(table1)

CrossTable(data_wide$`Treatment.12th month`, data_wide$success)



#Modelling Success against treatment


mod1=glm(factor(success)~1,family = binomial(link=logit),data=data_wide) ###---Intercept only model

mod2=glm(factor(success)~factor(`Treatment.12th month`),family = binomial(link=logit),data=data_wide) ###---Model with age as only covariate

summary(mod2)




mod3=glm(factor(success)~factor(`Treatment.12th month`)+ factor(`healthclub.12th month`)+
           factor(`gender.12th month`)+ `length.Base month`  ,family = binomial(link=logit),data=data_wide)
summary(mod3)



