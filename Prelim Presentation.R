setwd("E:/Grad School/2019-2020/Masters Thesis/Prelim Data Presentation/")
UHC2<-read.csv("UHCPrelimclean.csv")

library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(plyr)
library(data.table)
library(car)
library(tidyverse)
library(reshape2)
library(lme4)
library(lmerTest)
library(data.table)

##

UHC2_long<-melt(setDT(UHC2),measure=patterns("^POST1", "^POST2", "^POST3", "^POST4"), 
               value.name=c("P1", "P2", "P3", "P4"), variable.name="condition")
#Gives me the melt I want
UHC2_long<-UHC2_long[!(is.na(UHC_long$P1) | UHC_long$P1==""), ]
#deletes the empty rows created by the melt i want

#renaming our conditions
UHC_long$condition<-revalue(UHC_long$condition, c("1"="A", "2"="B", "3"="C"))

#Make sure to turn subject column into a factor
UHC_long$SUBJECT<-as.factor(UHC_long$SUBJECT)


#Revaluing our 'word items' into scores we can use
UHC_long$PRE1<-revalue(UHC_long$PRE1, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$PRE1<-as.numeric(as.character(UHC_long$PRE1))

UHC_long$PRE2<-revalue(UHC_long$PRE2, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$PRE2<-as.numeric(as.character(UHC_long$PRE2))

#reverse scored
UHC_long$PRE3<-revalue(UHC_long$PRE3, c("Strongly Disagree"= 7, "Disagree"= 6, "Somewhat Disagree" = 5, "Neither Agree or Disagree" = 4, "Somewhat Agree" =3, "Agree" =2, "Strongly Agree" =1))
UHC_long$PRE3<-as.numeric(as.character(UHC_long$PRE3))

UHC_long$PRE4<-revalue(UHC_long$PRE4, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$PRE4<-as.numeric(as.character(UHC_long$PRE4))

UHC_long$P1<-revalue(UHC_long$P1, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$P1<-as.numeric(as.character(UHC_long$P1))

UHC_long$P2<-revalue(UHC_long$P2, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$P2<-as.numeric(as.character(UHC_long$P2))

#reverse scored
levels(UHC_long$P3)
UHC_long$P3<-revalue(UHC_long$P3, c("Strongly Disagree"= 7, "Disagree"= 6, "Somewhat Disagree" = 5, "Neither Agree or Disagree" = 4, "Somewhat Agree" =3, "Agree" =2, "Strongly Agree" =1))
UHC_long$P3<-as.numeric(UHC_long$P3)

UHC_long$P4<-revalue(UHC_long$P4, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
UHC_long$P4<-as.numeric(UHC_long$P4)

#Calculating our rowmeans for our prescore and postscore
UHC_long$PRESCORE<-rowMeans(UHC_long[,c(2,3,4,5)], na.rm=FALSE)
UHC_long$POSTSCORE<-rowMeans(UHC_long[,c(7,8,9,10)], na.rm=FALSE)

#need to melt it so we have scores and time by subj as our predictor
UHC_final<-melt(UHC_long, id = c("SUBJECT","PRE1","PRE2","PRE3","PRE4","condition", "P1","P2","P3","P4"))

####
m1<-lmer(value ~ SUBJECT + condition + (1|SUBJECT), data    = UHC_final) 
summary(m1)
m2<-lmer(value ~ condition + (1|SUBJECT), data    = UHC_final)
#what i want to consider the effects of time
m3<-lmer(value ~ condition*variable + (1|SUBJECT), data    = UHC_final)
summary(m3)#use this :)

m3.1<-lmer(value ~ condition + variable + (1|SUBJECT), data    = UHC_final)
summary(m3.1)


#m4<-lmer(value ~ condition*variable + (variable|SUBJECT), data    = UHC_final)
#errored
m5<-lmer(value ~ condition*variable + (1|SUBJECT) + (1|variable), data    = UHC_final)
m6<-lmer(value ~ condition + (condition|variable) + (1|SUBJECT), data    = UHC_final)
summary(m5)

summary(m6)
```

library(tidyr)

library(ggplot2)
#plot1<-ggplot2.scatterplot(data=UHC_final, xName='variable',yName='value', 
#                    groupName='cyl', size=3,
#                    backgroundColor="white", setColorByGroupName=FALSE) 

plot2<-ggplot(UHC_final, aes(x=variable, y=value, shape=condition, color=condition)) +
  geom_point(position = position_jitter(w = 0.05, h = .05))
plot2 + facet_wrap(~ condition)

plot3<-ggplot(UHC_final, aes(x=variable, y=value, shape=condition, color=condition)) +
  geom_boxplot()
plot3 + facet_wrap(~ condition)

##
install.packages("tidyverse")
install.packages("tidyr")
install.packages("reshape2")
library(reshape2)
library(tidyr)
library(tidyverse)
#brms work for cumulative logistic regression
#need to go wide to long first
#try directly from UHC_long
UHC_final_long<-gather(UHC_long, item, measurement, c(PRE1, PRE2, PRE3, PRE4, P1, P2, P3, P4), factor_key = TRUE)

#make two extra columns, one for item, and one for pre or post

#use vectorized switch fxn, or we can try sapply as well
     
UHC_final_long$time<-sapply(UHC_final_long$item, switch, "PRE1" = "pre", "PRE2" = "pre", "PRE3" = "pre", "PRE4" = "pre", "P1" = "post", "P2" = "post", "P3" = "post", "P4" = "post")
class(UHC_final_long$time)
UHC_final_long$time<-as.factor(UHC_final_long$time)
#we did it with sapply!
UHC_final_long$question<-sapply(UHC_final_long$item, switch, "PRE1" = "1", "PRE2" = "2", "PRE3" = "3", "PRE4" = "4", "P1" = "1", "P2" = "2", "P3" = "3", "P4" = "4")
class(UHC_final_long$question)
UHC_final_long$question<-as.factor(UHC_final_long$question)
     
     
library(brms)
     
myprior<-prior(normal(0,2.5), class ="Intercept")
m7<-brm(measurement ~ condition + time + question + (1|SUBJECT), data = UHC_final_long, family = cumulative("logit"), prior = myprior)
     
summary(m7)

m8<-brm(measurement ~ condition*time + (1|SUBJECT), data = UHC_final_long, family = cumulative("logit"), prior = myprior)
summary(m8)

m9<-brm(measurement ~ condition*time + (condition|SUBJECT), data = UHC_final_long, family = cumulative("logit"), prior = myprior)
#unreliable?
summary(m9)

marginal_effects(m8, "condition", categorical = TRUE)

### Preference to use by condition

prefscore<-read.csv("Wouldyouuse_test.csv")
prefscore2<-select(prefscore, 28,44 )

prefscore_long<-gather(prefscore2, item, measurement, c(Q22.1_5, Q33.1_5), factor_key = TRUE)
prefscore_long<-prefscore_long[!(is.na(prefscore_long$measurement) | prefscore_long$measurement==""), ]
prefscore_long$item<-sapply(prefscore_long$item, switch, "Q22.1_5" = "B", "C")
class(prefscore_long$item)
prefscore_long$item<-as.factor(prefscore_long$item)
library(plyr)
prefscore_long$measurement<-revalue(prefscore_long$measurement, c("Strongly Disagree"= 1, "Disagree"= 2, "Somewhat Disagree" = 3, "Neither Agree or Disagree" = 4, "Somewhat Agree" =5, "Agree" =6, "Strongly Agree" =7))
prefscore_long$measurement<-as.numeric(as.character(prefscore_long$measurement))

t.test(measurement ~ item, prefscore_long, var.equal=TRUE)
#no difference b/w the two groups wrt to likelihood of accepting the health plan

plot4<-ggplot(prefscore_long, aes(x=item, y=measurement)) 
plot4+ geom_point(position = position_jitter(w = 0.05, h = .1), color = prefscore_long$measurement)
plot4+geom_boxplot()  
  