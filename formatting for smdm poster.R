library(lme4)
library("texreg")
library(ggplot2)
library(tidyverse)
library(gtable)
library(gtsummary)


#study 1 data
setwd("D:/Grad School/2020-2021/psych-masters")
s1_d<-read.csv("study_1_demog.csv")
s1_d<-s1_d[,-1]

UHC_final<-read.csv("study_1_final.csv")
#changing to allow the correct levels
UHC_final$variable <- factor(UHC_final$variable, levels = c("PRESCORE","POSTSCORE"), labels = c("PRE", "POST"))
#rename our condition var's to have actual names
UHC_final$condition <- factor(UHC_final$condition)
levels(UHC_final$condition) <- c("No Intervention", "Active Intervention","Passive Intervention")

study_1_m <- lmer(value ~  condition*variable + (1|SUBJECT), data    = UHC_final)
study_1_m_sum <- summary(study_1_m)
s_1_b<-lm(value ~ condition*variable, data =UHC_final)


#try cutting out some detail in order to simplify for poster
#lets see if we can summarize all the combo'd pieces as 'other'?

str(s1_d)
#ok lets turn col 1,3,4 into factors!~
s1_d[,c(1,3,4,5)] <- lapply(s1_d[,c(1,3,4,5)], factor)  ## as.factor() could also be used
is.na(s1_d$Race)
#lets make sure we have blanks replaced w/ other
s1_d$Race <- sub("^$", "Other", s1_d$Race)

#can we do this to force "other"?
s1_d$Race <- sub("African-American/Black,American Indian/Alaska Native", "Other", s1_d$Race)

s1_d$Race[140]<-"Other"


s1_d$Race <- sub("Caucasian/White,African-American/Black", "African-American/Black", s1_d$Race)
s1_d$Race <- sub("Caucasian/White,Asian/Pacific Islander", "Other", s1_d$Race)

table(s1_d$Race)
levels(s1_d$Race)

str(s1_d)
#lets try to re-order the df to see if that helps
s1_d <- s1_d[, c(2, 1, 3, 4, 5)]

s1_d[,1:4] %>%
  tbl_summary(
    by = condition,
    type = "Age" ~ "continuous", #so we can just direclty call age as continuous LOL
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% add_p() %>% bold_labels() %>% bold_p ()%>% as_gt() %>% gt::gtsave(filename = "study1_demog.png")

#study 2 d mogs?
demog<-UHC[,c(25:28,15)]

str(demog)
str(s1_d)
class(demog)

#lets see if it's b/c of factors??
table(s1_d$Age)
table(demog$Age)
## Lets rework all our shit so it's nice and neat
table(demog$Race)

demog$Race<- sub("APAC,Other", "APAC", demog$Race)
demog$Race<- sub("APAC,Hispanic/Latino", "Hispanic/Latino", demog$Race)
demog$Race<- sub("Black,American Indian/Alaska Native", "Other", demog$Race)
demog$Race<- sub("White,APAC", "Other", demog$Race)
demog$Race<- sub("White,Black,APAC", "Other", demog$Race)
demog$Race<- sub("White,Other,Hispanic/Latino,Other", "Other", demog$Race)
demog$Race<- sub("White,Other", "Other", demog$Race)
demog$Race<- sub("White,American Indian/Alaska Native", "American Indian/Alaska Native", demog$Race)
demog$Race<- sub("Black,Hispanic/Latino", "Hispanic/Latino", demog$Race)
demog$Race<- sub("White,Hispanic/Latino", "Hispanic/Latino", demog$Race)
demog$Race<- sub("White,Black", "Black", demog$Race)



demog[,c(1:3,5)] %>%
  tbl_summary(
    by = condition,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% add_p() %>% bold_labels() %>% bold_p ()%>% as_gt() %>%gt::gtsave(filename = "study2_demog.png")


#graphics?

#study 2 gfx
plot6 <- ggplot(UHC_model_long,aes(x = RNS_score, y = UHC_Support, color = condition)) +
  theme_bw() +
  labs(x = "Objective Numeracy Score",
       y = "Support for UHC",
       color = "Condition",  title = "Moderating Effect of Objective Numeracy on Support for UHC")
plot6 + scale_color_brewer(palette = "Set1")+
  geom_point(position = position_jitter(w = 0.1, h = 0), alpha = .6, size = .9) +
  geom_smooth(method = "lm", alpha =.3, se=FALSE)

plot7 <- ggplot(UHC_model_long,aes(x = SNS_score, y = UHC_Support, color = condition)) +
  theme_bw() +
  labs(x = "Subjective Numeracy Score",
       y = "Support for UHC",
       color = "Condition",  title = "Moderating Effect of Subjective Numeracy on Support for UHC")
plot7 + scale_color_brewer(palette = "Set1")+
  geom_point(alpha = .6, size = .9) +
  geom_smooth(method = "lm", alpha =.3)

plot1<-ggplot(UHC_model_long, aes(x=Time, y=UHC_Support, color=condition)) +
  geom_boxplot() 
plot1 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")+labs(x = "Time of Measurement", y = "Support for UHC", title = "Effect of Intervention on Support for UHC - Study 2")+ theme(legend.position="none")

#study 1 plots
plot3<-ggplot(UHC_final, aes(x=variable, y=value, color=condition)) +
  geom_boxplot() + labs(title="Effect of Intervention on Support for UHC - Study 1", x = "Time-point (pre or post intervention)", y = "Support for UHC") + 
  scale_color_manual(labels = c("No Intervention", "Active Intervention", "Passive Intervention"), 
                     values=c("red", "blue", "black")) 
plot3 + facet_wrap(~ condition) + theme(legend.position="none")
