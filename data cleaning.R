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
UHC$Subject<-seq(from =1, to = length(UHC$Pre_1_10))

#see if we can find our code from the other bit to mash the two halves together
library(data.table)
#lets give a mod of this data.table code a shot
UHC_long<-melt(setDT(UHC),measure=patterns("^1_10", "^2_1", "^3_1", "^4_1", "^Med_A_1", "^Med_B_1", "^Med_B_2"), 
               value.name=c("P1", "P2", "P3", "P4", "P5","P6","P7"), variable.name="condition")
