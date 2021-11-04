covid_data<-read.csv("longitudinal survey data/covid 3 waves pred+measure")
covid_data$wave<-as.factor(covid_data$wave)
wav1_full<-read.csv("longitudinal survey data/wave1_pred_measure")
wav2_full<-read.csv("longitudinal survey data/wave2_pred_measure")
wav3_full<-read.csv("longitudinal survey data/wave3_pred_measure")

library(tidyverse)
library(plyr)
library(ggplot2)
library(psych)
library(texreg)
library(lme4)
library(lmerTest)
levels(covid_data$wave)
str(covid_data)
covid_data$wave <- ordered(covid_data$wave, levels = c("1", "2", "3"))
#set our time var of wave as an ordered factor

#change wave to month for better visual acuity, modify dataset to make sure we only have repeated info

testvec<-table(covid_data$pid)==3
library(tidyverse)
#lets ref the PID from wave 3 itself then
wav3_pid<-wav3_full$pid

#use the %in% command to filter entries in a vec
all3waves_data<-covid_data %>% filter(pid %in% wav3_pid)
sum(table(all3waves_data$pid)==3)

index<-which(all3waves_data$pid==1557535043)
all3waves_data[index,]
#clean ver w/ NA's omitted
all3waves_data_naless<-na.omit(all3waves_data)
#sure this is fine
all3waves_data_naless$wave


levels(all3waves_data_naless$wave) <- c("March", "April","June")
table(all3waves_data_naless$wave)





library(ggplot2)
library(dplyr)

## Effect of Trust on guideline eval
trust_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=trust,y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Trust", y = "Score",title= "Guideline Evaluation Measure", subtitle = "Lower trust is correlated with belief that guidelines are 'too much'") 
trust_eval
## Effect of Trust on comply
trust_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=trust,y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Trust", y = "Score",title= "Intended Compliance Measure", subtitle = "Lower trust is correlated with lower intention to comply") 
trust_comply
# what does this need?

## Effect of Affect on guideline eval
aff_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(aff, factor =15),y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Affect", y = "Score",title= "Guideline Evaluation Measure", subtitle = "Effect of COVID-19 worry changes as time passes") 
aff_eval
#Wave 1, higher worry = concern guidelines are too much, Wave 2-3, not worried = concern guidelines are too much

## Effect of Affect on comply
aff_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(aff,factor=15),y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Affect", y = "Score",title= "Intended Compliance Measure", subtitle = "Higher COVID-19 worry correlated with greater intention to comply") 
aff_comply

## Effect of CRA on guideline eval
cra_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(cra, factor =1),y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Cognitive Risk Assessment", y = "Score",title= "Guideline Evaluation Measure", subtitle = "Effect of CRA changes as time passes") 
cra_eval
#Wave 1, greater belief in getting covid and greater severity = concern guidelines are too much, Wave 2-3, greater CRA = concern guidelines are too much

## Effect of CRA on comply
cra_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(cra,factor=1),y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Cognitive Risk Assessment", y = "Score",title= "Intended Compliance Measure", subtitle = "Greater perceived likelihood of getting COVID-19 correlated with greater intention to comply") 
cra_comply

#clones w/o subtitles
## Effect of Trust on guideline eval
trust_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=trust,y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Distrust", y = "Score",title= "Guideline Evaluation Measure") 
trust_eval
## Effect of Trust on comply
trust_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=trust,y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Distrust", y = "Score",title= "Intended Compliance Measure") 
trust_comply
## Effect of Affect on guideline eval
aff_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(aff, factor =15),y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Affect", y = "Score",title= "Guideline Evaluation Measure") 
aff_eval
#Wave 1, higher worry = concern guidelines are too much, Wave 2-3, not worried = concern guidelines are too much
## Effect of Affect on comply
aff_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(aff,factor=15),y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Affect", y = "Score",title= "Intended Compliance Measure") 
aff_comply
## Effect of CRA on guideline eval
cra_eval<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(cra, factor =1),y=guide_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3)+ scale_color_brewer(palette = "Set1")+ labs( x = "Cognitive Risk Assessment", y = "Score",title= "Guideline Evaluation Measure") 
cra_eval
#Wave 1, greater belief in getting covid and greater severity = concern guidelines are too much, Wave 2-3, greater CRA = concern guidelines are too much
## Effect of CRA on comply
cra_comply<-ggplot(data = all3waves_data_naless, mapping = aes(x=jitter(cra,factor=1),y=comply_meas, color = wave))+
  geom_point(alpha=.3)+
  geom_smooth(method = "lm", alpha=.3 )+ scale_color_brewer(palette = "Set1")+ labs( x = "Cognitive Risk Assessment", y = "Score",title= "Intended Compliance Measure") 
cra_comply



#demographics?
wav1<-read.csv("longitudinal survey data/wave1_data")
#try to do it by cheesin
wav1_demog<-wav1[,c("age","sex","race"),]
wav1_demog$pid<-wav1_full$pid
#subset wav1 by only PID's in wave 3
wav3_pid<-wav3$pid
mod_wav1_demog<-wav1_demog %>% filter(pid %in% wav3_pid)
(unique(wav3_pid))

library(gtable)
library(gtsummary)
library(tidyverse)
wav1_demog

colnames(wav1_demog)<-c("Age","Sex","Race")

wav1_demog<-mod_wav1_demog[,1:3]
wav1_demog$sex<-as.factor(wav1_demog$sex)
levels(wav1_demog$sex) <- c("Male", "Female","Other","Other")
table(wav1_demog$sex)
wav1_demog$race<-as.factor(wav1_demog$race)
levels(wav1_demog$race) <- c("American Indian/Alaskan Native", "Asian/Asian American","Black/African American","Native Hawaiian/Pacific Islander", "White/European American","Other")
table(wav1_demog$race)



wav1_demog %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% bold_labels()

#demographics for
wav3<-read.csv("longitudinal survey data/wave3_data")
wav3_demog<-wav3[,c("age","sex","race"),]
length(wav3_pid)

library(gtable)
library(gtsummary)
library(tidyverse)
wav1_demog

wav1_demog$sex<-as.factor(wav1_demog$sex)
levels(wav1_demog$sex) <- c("Male", "Female","Other","Other")
table(wav1_demog$sex)
wav1_demog$race<-as.factor(wav1_demog$race)
levels(wav1_demog$race) <- c("American Indian/Alaskan Native", "Asian/Asian American","Black/African American","Native Hawaiian/Pacific Islander", "White/European American","Other")
table(wav1_demog$race)

colnames(wav1_demog)<-c("Age","Sex","Race")

wav1_demog %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% bold_labels()


#time to figure out this weird demographic shit w/ my table
truvec<-table(wav3_full$pid)==2
index<-which(wav3_full==truvec)
wav3_full[index,]
#well haven't figured it out, but some PID's in wave 3 are dupes apparantly
