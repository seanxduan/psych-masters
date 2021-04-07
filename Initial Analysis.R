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

