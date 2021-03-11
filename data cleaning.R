#Data cleaning for our initial uhc file
UHC<-read.csv("UHC_fall_2020.csv")
#if we want to repl the blanks w/ NA's over the whole DF #, na.strings=c(""," ","NA")
library(tidyverse)

#start by deleting unnecessary columns and rows
#delete first two rows (b/c filler info)
UHC<-UHC[-c(1,2),]

#remove all rows that haven't finished
UHC$Finished<-as.numeric(UHC$Finished)
UHC<-UHC[!(UHC$Finished==0),]

#remove all columns w/ timing elements in them (can re-add later)
UHC<-UHC %>% select(-contains("Time"))

#delete unnecessary front and back columns
UHC<-UHC[,-c(1:17)]
UHC<-UHC[,-53]

#add a col w/ subject counter
UHC$Subject<-seq(from =1, to = length(UHC$RNS_1))

#see if we can find our code from the other bit to mash the two halves together
library(data.table)
#we need to rename our pre cols so they don't get aggregated it
names(UHC)[1:7] <- c("Pre_P1", "Pre_P2", "Pre_P3","Pre_P4","Pre_Med_P5", "Pre_Med_P6", "Pre_Med_P7")
#cut out rows w/ no post response?
UHC<-UHC[-c(1:6),]
#have to rename some of our med q cols b/c they overlap w/ other post measures

#lets summate our sns and rns so we can have a single measure and del the other irrel cols?
#pull into a sep df and then sum across what we have
RNS_mat<-cbind(UHC$RNS_1==5, UHC$RNS_2==.05, UHC$RNS_3==0.1, UHC$RNS_4==10,UHC$RNS_5==500, UHC$RNS_6==20,UHC$RNS_7==100,UHC$RNS_8==.5,UHC$RNS_8==50)
RNS_score<-rowSums(RNS_mat)
RNS_score[1:68]<-NA

SNS_mat<-select(UHC, contains("SNS"))


SNS_mat<-as.data.frame(SNS_mat)
SNS_mat$SNS_1_1<-as.numeric(SNS_mat$SNS_1_1)
SNS_mat$SNS_2_1<-as.numeric(SNS_mat$SNS_2_1)
SNS_mat$SNS_3_1<-as.numeric(SNS_mat$SNS_3_1)
SNS_mat$SNS_4_1<-as.numeric(SNS_mat$SNS_4_1)
SNS_mat$SNS_5_1<-as.numeric(SNS_mat$SNS_5_1)
SNS_mat$SNS_6_1<-as.numeric(SNS_mat$SNS_6_1)
SNS_mat$SNS_7_1<-as.numeric(SNS_mat$SNS_7_1)
SNS_mat$SNS_8_1<-as.numeric(SNS_mat$SNS_8_1)
SNS_score<-rowMeans(SNS_mat, na.rm = TRUE)

#lets re-add the scores after removing our vectors of pre-items
UHC_w_score<-UHC[,-c(37:52)]
UHC_w_score$SNS_score<-SNS_score
UHC_w_score$RNS_score<-RNS_score
#set the first few 0's to NA, since it isn't actually zero!
#lets save this, and clean up our df so we can do our select and melt correctly afterwards :)
write.csv(UHC_w_score,'UHC_w_score_2020.csv')



#lets give a mod of this data.table code a shot
UHC_long<-melt(setDT(UHC),measure=patterns("1_10", "2_1", "3_1", "4_1", "Med_A_1", "Med_B_1", "Med_B_2", "Insurance"), 
               value.name=c("P1", "P2", "P3", "P4", "Post_Med_A","Post_Med_B_1","Post_Med_B_2", "ins_score"), variable.name="condition")

UHC_w_score %>% select(contains("Med_B_2"))
#1_10 fine
#2_1 dirty w/ med q
#3_1 fine
#4_1 fine
#med_a_1 fine
#med_b_1 fine
#med_b_2 fine

UHC_w_score<-read.csv("UHC_w_score_2020.csv")
names(UHC_w_score)[names(UHC_w_score) == 'Post_Ctrl_Med_B_1_10'] <- 'Post_Ctrl_Med_B_1_1'
UHC_w_score %>% select(contains("1_10"))

names(UHC_w_score)[names(UHC_w_score) == 'Post_Ctrl_Med_B_2_10'] <- 'Post_Ctrl_Med_B_2_2'
names(UHC_w_score)[names(UHC_w_score) == 'Post_Int_Med_B_2_1'] <- 'Post_Int_Med_B_2_2'
UHC_w_score %>% select(contains("2_1"))

UHC_w_score %>% select(contains("Med_B_2"))
#i think we fixed it all?

UHC_w_score %>% select(contains("1_10"))
UHC_w_score %>% select(contains("2_1"))
UHC_w_score %>% select(contains("3_1"))
UHC_w_score %>% select(contains("4_1"))
UHC_w_score %>% select(contains("Med_A_1"))
UHC_w_score %>% select(contains("Med_B_1"))
UHC_w_score %>% select(contains("Med_B_2"))
names(UHC_w_score)[names(UHC_w_score) == 'Insurance_Int_1'] <- 'Insurance_1_Int'
names(UHC_w_score)[names(UHC_w_score) == 'Insurance_Int_2'] <- 'Insurance_2_Int'
UHC_w_score %>% select(contains("Insurance_1"))
names(UHC_w_score)[names(UHC_w_score) == 'Insurance_Ctrl_1'] <- 'Insurance_1_Ctrl'
names(UHC_w_score)[names(UHC_w_score) == 'Insurance_Ctrl_2'] <- 'Insurance_2_Ctrl'
UHC_w_score %>% select(contains("Insurance_2"))

#need to only have 2 groups for ins question, probably check 2 patterns and group into 2 values

#need an additional entry in ourmelt for age, gender, race, school year,
UHC_w_score %>% select(contains("Age"))
UHC_w_score %>% select(contains("Gender"))
UHC_w_score %>% select(contains("Race"))
UHC_w_score %>% select(contains("School"))
UHC_w_score %>% select(contains("Free"))

UHC_long<-melt(setDT(UHC_w_score),measure=patterns("1_10", "2_1", "3_1", "4_1", "Med_A_1", "Med_B_1", "Med_B_2", "Insurance_1", "Insurance_2","Age","Gender","Race","School", "Free"), 
               value.name=c("P1", "P2", "P3", "P4", "Post_Med_A","Post_Med_B_1","Post_Med_B_2", "ins_score_1", "ins_score_2", "Age","Gender","Race","School_Year","Free_Response"), variable.name="condition")


#time to clean out our empty rows
ind<-which(is.na(UHC_long$P1))
UHC_long_clean<-UHC_long[!ind,]

#save this clean shit!
write.csv(UHC_long_clean,'UHC_clean_2020.csv')