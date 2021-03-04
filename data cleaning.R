#Data cleaning for our initial uhc file
UHC<-read.csv("UHC_fall_2020.csv")
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
UHC$RNS_1==5
UHC$RNS_2==.05
UHC$RNS_3==0.1
UHC$RNS_4==10
UHC$RNS_5==500 
UHC$RNS_6==20
UHC$RNS_7
#how to check for multiple correct ans? .5 or 50??
UHC$RNS_8==.5

#lets give a mod of this data.table code a shot
UHC_long<-melt(setDT(UHC),measure=patterns("1_10", "2_1", "3_1", "4_1", "Med_A_1", "Med_B_1", "Med_B_2", "Insurance"), 
               value.name=c("P1", "P2", "P3", "P4", "P5","P6","P7", "ins_score"), variable.name="condition")

UHC %>% select(contains("4_1"))
