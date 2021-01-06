## For victoria, to format from wide to tall, as well as scoring and tabulation ##

getwd()
setwd("C:/Users/Sean/Desktop/Notes/R")

patientsurvey<-read.csv("patient_survey_clean.csv", header = TRUE)

library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(plyr)
library(data.table)
library(car)

#First lets score the BNT
#make a column w/ 0 or 1 for each correct answer, then sum the values of the columns
patientsurvey$berlinscore1<-ifelse(patientsurvey$berlin.1 == "25", 1,0)
patientsurvey$berlinscore1[is.na(patientsurvey$berlinscore1)]<-0

patientsurvey$berlinscore2<-ifelse(patientsurvey$berlin.2a == "30", 1,0)
patientsurvey$berlinscore2[is.na(patientsurvey$berlinscore2)]<-0

patientsurvey$berlinscore3<-ifelse(patientsurvey$berlin.2b == "20", 1,0)
patientsurvey$berlinscore3[is.na(patientsurvey$berlinscore3)]<-0

patientsurvey$berlinscore4<-ifelse(patientsurvey$berlin.3 == "50", 1,0)
patientsurvey$berlinscore4[is.na(patientsurvey$berlinscore4)]<-0

patientsurvey$berlintotal<-(patientsurvey$berlinscore1+patientsurvey$berlinscore2+patientsurvey$berlinscore3+patientsurvey$berlinscore4)

#Lets do the scoring for the SNS
#We have to convert extremely into a 6. and and not at all good into a 1
#note, we have to reverse the score for q7
#note, turning factors into numbers, we have to use as.numeric(as.character()) or the values will be shifted

patientsurvey$Q184_1<-revalue(patientsurvey$Q184_1, c("Extremely good\n6\n"= 6, "Not at all good\n\n1\n"= 1))
patientsurvey$Q184_1<-as.numeric(as.character(patientsurvey$Q184_1))

patientsurvey$Q184_2<-revalue(patientsurvey$Q184_2, c("Extremely good\n6\n"= 6, "Not at all good\n\n1\n"= 1))
patientsurvey$Q184_2<-as.numeric(as.character(patientsurvey$Q184_2))

patientsurvey$Q184_3<-revalue(patientsurvey$Q184_3, c("Extremely good\n6\n"= 6, "Not at all good\n\n1\n"= 1))
patientsurvey$Q184_3<-as.numeric(as.character(patientsurvey$Q184_3))

patientsurvey$Q184_4<-revalue(patientsurvey$Q184_4, c("Extremely good\n6\n"= 6, "Not at all good\n\n1\n"= 1))
patientsurvey$Q184_4<-as.numeric(as.character(patientsurvey$Q184_4))

patientsurvey$Q186<-revalue(patientsurvey$Q186, c("Extremely helpful\n6\n"= 6, "Not at all helpful\n\n1\n"= 1))
patientsurvey$Q186<-as.numeric(as.character(patientsurvey$Q186))

patientsurvey$Q188<-revalue(patientsurvey$Q188, c("Always prefer numbers\n6\n"= 6, "Always prefer words\n1\n"= 1))
patientsurvey$Q188<-as.numeric(as.character(patientsurvey$Q188))

patientsurvey$Q190<-revalue(patientsurvey$Q190, c("Always prefer words\n6\n"= 6, "Always prefer percentages\n1\n"= 1))
patientsurvey$Q190<-as.numeric(as.character(patientsurvey$Q190))
#this bit below is just reverse coding the question
patientsurvey$Q190<-recode(patientsurvey$Q190, "6 = 1 ; 5 = 2 ; 4 = 3; 3 = 4; 2 = 5; 1 = 6")

patientsurvey$Q192<-revalue(patientsurvey$Q192, c("Very often\n6\n"= 6, "Never\n1\n"= 1))
patientsurvey$Q192<-as.numeric(as.character(patientsurvey$Q192))

patientsurvey$SNS<-rowMeans(patientsurvey[c('Q184_1','Q184_2','Q184_3','Q184_4','Q186','Q188','Q190','Q192')], na.rm = TRUE)
patientsurvey$SNSability<-rowMeans(patientsurvey[c('Q184_1','Q184_2','Q184_3','Q184_4')], na.rm = TRUE)
patientsurvey$SNSpreference<-rowMeans(patientsurvey[c('Q186','Q188','Q190','Q192')], na.rm = TRUE)

#scoring the SILS is simply valuing word answers into either a 0 or a 1
patientsurvey$SILS<-revalue(patientsurvey$Q194, c("Never\n1\n"= 1, "Rarely\n2\n"= 1,"Always\n5\n"= 0, "Often\n4\n"= 0,"Sometimes\n3\n"= 0 ))


#Ok, lets code the graph literacy scale! I've read the paper, and the correct answers aren't written down in it (so I'm assuming I have the right ones)
#Also, how it was scored wasn't particularly specified, so i mimicked the scoring for the BNT

patientsurvey$graphscore1<-ifelse(patientsurvey$GL1 == "35", 1,0)

patientsurvey$graphscore2<-ifelse(patientsurvey$GL2 == "15", 1,0)

patientsurvey$graphscore3<-ifelse(patientsurvey$GL3 == "25", 1,0)

patientsurvey$graphscore4<-ifelse(patientsurvey$GL4 == "25", 1,0)

patientsurvey$graphscore5<-ifelse(patientsurvey$GL5 == "20", 1,0)

patientsurvey$graphscore6<-ifelse(patientsurvey$GL6 == "Increase was the same in both intervals", 1,0)

patientsurvey$graphscore7<-ifelse(patientsurvey$GL7 == "25", 1,0)

patientsurvey$graphscore8<-ifelse(patientsurvey$GL8 == "40", 1,0)

patientsurvey$graphscore9<-ifelse(patientsurvey$GL9 == "20", 1,0)

patientsurvey$graphscore10<-ifelse(patientsurvey$GL10 == "They are equal", 1,0)

patientsurvey$graphscore11<-ifelse(patientsurvey$GL11 == "Can't say", 1,0)

patientsurvey$graphscore12<-ifelse(patientsurvey$GL12 == "Tiosis", 1,0)

patientsurvey$graphscore13<-ifelse(patientsurvey$GL13 == "5", 1,0)

patientsurvey$graphtotal<-(patientsurvey$graphscore1 + patientsurvey$graphscore2 + patientsurvey$graphscore3 + patientsurvey$graphscore4 + patientsurvey$graphscore5 + patientsurvey$graphscore6 + patientsurvey$graphscore7 + patientsurvey$graphscore8 + patientsurvey$graphscore9 + patientsurvey$graphscore10 + patientsurvey$graphscore11 + patientsurvey$graphscore12 + patientsurvey$graphscore13)

#now lets score the health literacy 3 items thing

patientsurvey$Q230_1<-revalue(patientsurvey$Q230_1, c("\n 1\n Always"= 1, "\n\t2\n\tOften\n"= 2, "\n\t3\n\tSometimes\n" = 3, "\n\t4\n\tOccasionally\n" = 4, "\n\t5\n\tNever\n" =5))
patientsurvey$Q230_1<-as.numeric(as.character(patientsurvey$Q230_1))

patientsurvey$Q230_2<-revalue(patientsurvey$Q230_2, c("\n 1\n Always"= 1, "\n\t2\n\tOften\n"= 2, "\n\t3\n\tSometimes\n" = 3, "\n\t4\n\tOccasionally\n" = 4, "\n\t5\n\tNever\n" =5))
patientsurvey$Q230_2<-as.numeric(as.character(patientsurvey$Q230_2))

patientsurvey$Q230_3<-revalue(patientsurvey$Q230_3, c("\n 1\n Always"= 1, "\n\t2\n\tOften\n"= 2, "\n\t3\n\tSometimes\n" = 3, "\n\t4\n\tOccasionally\n" = 4, "\n\t5\n\tNever\n" =5))
patientsurvey$Q230_3<-as.numeric(as.character(patientsurvey$Q230_3))

## Lastly, we need to do a melt, with multiple columns melted at the 'same time' so they dont create overlap

patient_long<-melt(setDT(patientsurvey),measure=patterns("control$", "understand$", "medication$", "alarm$"), 
                   value.name=c("control", "understand", "medication", "alarm"), variable.name="condition")
levels(patient_long$condition)

patient_long$condition<-revalue(patient_long$condition, c("1"="M130SD15_raw", "2"="M130SD25_raw", "3"="M145SD15_raw", "4"="M145SD25_raw","5" = "M130SD15_table", "6" = "M145SD15_table", "7"= "M130SD25_table", "8" = "M145SD25_table", "9"="M130SD15_smooth", "10"="M130SD25_smooth", "11"="M145SD15_smooth", "12"="M145SD25_smooth"))

write.csv(patient_long, file ="patient_long.csv")
