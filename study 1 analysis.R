### Study 1 Analysis 

study_1_raw<-read.csv("Study_1_Raw.csv")
study_1_raw<-study_1_raw[-c(1,2),]
#
library(tidyverse)
demog<-select(.data = study_1_raw,c(30:36,49:52, 65:68))
demog<-demog[,4:15]
d_l<-c("age1","sex1", "race1", "year1", "age2","sex2", "race2", "year2", "age3","sex3", "race3", "year3")
names(demog)<-d_l

library(reshape2)
library(data.table)
demog_long<-melt(setDT(demog),measure=patterns("age", "sex", "race", "year"), 
               value.name=c("Age", "Sex", "Race", "Year"), variable.name="condition")

demog_clean<-demog_long[,]

#demog_clean <- demog_clean[!apply(is.na(demog_clean) | demog_clean == "", 1, all),]
?is.na
demog_clean<-demog_clean[!(is.na(demog_clean$Age) | demog_clean$Age==""), ]

#save our prepped demog from study 1 table
levels(demog_clean$condition) <- c("No Intervention", "Active Intervention","Passive Intervention")
write.csv(demog_clean,'study_1_demog.csv')
library()