### lets start by doing some good looking tables?
UHC<-read.csv("UHC_rdy_for_analysis_2020.csv")
install.packages("gt")
#gt pkg new shit!
install.packages("gtsummary")

#lets work thru the tutorial, then see how to apply it to our own data?
library(gtsummary)
head(trial)
trial2 <- trial %>% select(trt, age, grade)

#as needed, takes df as our input, and returns
#descriptive stats for each col in our df
trial2 %>% tbl_summary()

#can also split data by treatment group using by= arg
#if we have add_p we detect variable type and use appropriate stat test
trial2 %>% tbl_summary(by = trt) %>% add_p()

##customize output?
#can use tbl_summary fxn args
#can use add_* fxn for addtl data/info
#can mod summary table appearance w/ gtsummary fxn
#can modify table appearance w/ gt pkg fxn
trial2 %>%
  tbl_summary(
    by = trt,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = grade ~ "Tumor Grade",
    missing_text = "(Missing)"
  )


trial2 %>%
  tbl_summary(by = trt) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  bold_labels()
#can turn our graph into a gt function after our mods have been completed
#to add some GT package fxn into our tables

## Select Helpers
#can use {tidyselect} helpers (starts_with, contains, and everything)

### OUR DATA
# I think we should subset our data?
#lets start with that and see where we go
demog<-UHC[,c(25:28,15)]

demog %>% tbl_summary(by = condition)
table(demog$Race)
str(demog)

demog %>%
  tbl_summary(
    by = condition,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = School_Year ~ "School Year",
    missing_text = "(Missing)"
  )
#what would we change?
#consider condensing racial categories (for ease of use?)
#lets try the 'contains' feature
demog %>%
  tbl_summary(
    by = condition,
    statistic =c("Age") ~ "{mean} ({sd})")

#hmm, looks like we would have to modify the data itself?
#lets finish making sure this is well polished
demog %>%
  tbl_summary(
    by = condition,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = School_Year ~ "School Year",
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% add_p() %>% bold_labels() %>% bold_p ()

str(demog)
#alrighty, well that's something decent if nothing else!
#if we want to clone out this data using our previous analysis, we have to change our organization quite a bit
#e.g. need to melt out prescore and postscore into one col
UHC_model<-UHC[,c(11:15,23:35)]

UHC_model_long <- reshape(
  data = UHC_model,
  varying = list(c("Prescore","Postscore"),
                 c("Pre_Equality","Post_Equality"),
                 c("Pre_Understanding","Post_Understanding")),
  idvar = 'Subject',
  v.names = c('UHC_Support', 'Equality', 'Understanding'),
  timevar = 'Time',
  times = c('pre', 'post'),
  direction = 'long'
)

#lets try it w/ another thang, our study 1 data!
s1_d<-read.csv("study_1_demog.csv")
s1_d<-s1_d[,-1]

s1_d <- s1_d %>% mutate_if(is.character,as.factor)
#this is some hooooooot shiiiiiiiiiiit

as.double(s1_d$Age)
str(s1_d)
class(s1_d$Age)

bleh<-s1_d$Age
s1_d$Age<-bleh

s1_d %>% tbl_summary(by = condition)
table(demog$Race)
str(s1_d)


#we could do something better, actually we gotta
s1_d %>%
  tbl_summary(
    by = condition,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = Year ~ "School Year",
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% add_p() %>% bold_labels() %>% bold_p ()

## lets do some basic modelling work, copying what we did from an earlier era!
library(tidyverse)
library("lme4")
library("ggplot2")
library("papaja")
library("sjPlot")
library("papaja")
library("brms")
library("rethinking")
library("dplyr")
library("texreg")

##copied direct from old code
UHC_old<-read.csv("UHC_old.csv")
#changing to allow the correct levels
UHC_old$variable <- factor(UHC_old$variable, levels = c("PRESCORE","POSTSCORE"))
#rename our condition var's to have actual names
UHC_old$condition<-factor(UHC_old$condition)
levels(UHC_old$condition)<-c("Control", "Active","Passive")
#frequentist model
m3 <- lmer(value ~ 0 + condition*variable + (1|SUBJECT), data    = UHC_final)
m3_sum <- summary(m3)


#lets see if we can try this with our NEW data
m1 <- lmer(UHC_Support ~ 0 + condition*Time + (1|Subject), data    = UHC_model_long)
m1_sum <- summary(m1)

#perhaps try it as a normal lm?
m2<-lm(UHC_Support ~condition*Time, data = UHC_model_long)
summary(m2)

m3<-lm(UHC_Support ~Understanding + Equality, data = UHC_model_long)
summary(m3)


#maybe try some descriptive plots??
plot1<-ggplot(UHC_model_long, aes(x=Time, y=UHC_Support, color=condition)) +
  geom_boxplot() 
plot1 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")

#lets see if understanding changes?
plot2<-ggplot(UHC_model_long, aes(x=Time, y=Understanding, color=condition)) +
  geom_boxplot() 
plot2 + facet_wrap(~ condition) +scale_fill_distiller()

plot3<-ggplot(UHC_model_long, aes(x=Time, y=Equality, color=condition)) +
  geom_boxplot() 
plot3 + facet_wrap(~ condition)

#for xport
plot1<-ggplot(UHC_model_long, aes(x=Time, y=UHC_Support, color=condition)) +
  geom_boxplot() 
plot1 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")+labs(x = "Time of Measurement", y = "Support for UHC", title = "Effect of Intervention on Support for UHC", caption="No significant main effect of intervention condition on support for UHC")

plot2<-ggplot(UHC_model_long, aes(x=Time, y=Understanding, color=condition)) +
  geom_boxplot() 
plot2 + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+labs(x = "Time of Measurement", y = "Percieved Understanding of UHC", title = "Effect of Intervention on Understanding of UHC", caption="Understanding increases across conditions, less variance in control condition")

plot3<-ggplot(UHC_model_long, aes(x=Time, y=Equality, color=condition)) +
  geom_boxplot() 
plot3 + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+labs(x = "Time of Measurement", y = "Perception of Equity in UHC", title = "Effect of Intervention on Perception of Equity in UHC", caption="Percieved equity change only significant in control condition")

plot4<-ggplot(UHC_model_long, aes(x=SNS_score, y=UHC_Support, color=condition)) +
  geom_point()+geom_smooth(method="lm") 
plot4 + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+labs(x = "Subjective Numeracy", y = "Support for UHC", title = "Effect of Subjective Numeracy on Support for UHC",subtitle="With added linear regression line at 95% confidence interval", caption="No difference in subjective numeracy effect across conditions")

plot5<-ggplot(UHC_model_long, aes(x=RNS_score, y=UHC_Support, color=condition)) +
  geom_jitter(width=0.1) +geom_smooth(method="lm")
plot5 + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+labs(x = "Objective Numeracy", y = "Support for UHC", title = "Effect of Objective Numeracy on Support for UHC", subtitle="With added linear regression line at 95% confidence interval", caption="Objective numeracy effect significant in intervention")


#try our moderating analysis
m4<-lm(UHC_Support ~ condition*RNS_score, data = UHC_model_long)
summary(m4)
#this is good! it shows very clearly our factual interaction

UHC_model$diff<-UHC_model$Postscore-UHC_model$Prescore
m5<-lm(diff ~ condition*RNS_score, data = UHC_model)
summary(m5)

#plots for our moderating analysis
library(ggplot2)
plot6 <- ggplot(UHC_model_long,aes(x = RNS_score, y = UHC_Support, color = condition)) +
  theme_bw() +
  labs(x = "Objective Numeracy Score",
       y = "Support for UHC",
       color = "Condition",  title = "Moderating Effect of Objective Numeracy on Support for UHC",
       subtitle="With added linear regression line at 95% confidence interval",
       caption="Objective numeracy effect significant in intervention")
plot6 + scale_color_brewer(palette = "Set1")+
  geom_point(position = position_jitter(w = 0.1, h = 0), alpha = .6, size = .9) +
  geom_smooth(method = "lm", alpha =.3)

plot6 + scale_color_brewer(palette = "Set1")+
  geom_line()+geom_point(position = position_jitter(w = 0.1, h = 0), alpha = .6, size = .9)
#try code adapted from victoria
plot6 + stat_summary(fun.y = mean, geom = "line") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar")+ scale_color_brewer(palette = "Set1")+
  geom_point(position = position_jitter(w = 0.1, h = 0), alpha = .3, size = .3)
#use either this or the other oen

#mediating analysis!
#first step is linear regression of our IV (intervention or not)
#on our mediator!
UHC_model$eqd<-UHC_model$Post_Equality-UHC_model$Pre_Equality
UHC_model$und<-UHC_model$Post_Understanding-UHC_model$Pre_Understanding

med1_m<-lm(eqd~condition,UHC_model)
summary(med1_m)
med1_m<-lm(Equality~condition,UHC_model_long)
summary(med1_m)
med1_m<-lm(Equality~condition*Time,UHC_model_long)
summary(med1_m)
#yep, condition has a sigeff on equality

med2_m<-lm(und~condition,UHC_model)
summary(med2_m)
med2_m<-lm(Understanding~condition,UHC_model_long)
summary(med2_m)
med2_m<-lm(Understanding~condition*Time,UHC_model_long)
summary(med2_m)

#step 2 is fx of mediator on dept variable
#for equality
med3_m<-lm(UHC_Support~condition+Equality,UHC_model_long)
summary(med3_m)
#for understanding
med4_m<-lm(UHC_Support~condition+Understanding,UHC_model_long)
summary(med4_m)
#we see that there is a crit fx of understanding... but not a mediational relationship

#lets finish ittttt
install.packages("mediation")
library(mediation)

results<-mediate(med1_m, med3_m, treat='condition', mediator='Equality', boot=T)
summary(results)
#so mediational relationship b/w equality! but NOT understanding?

results2<-mediate(med2_m, med4_m, treat='condition', mediator='Understanding', boot=T)
summary(results2)
