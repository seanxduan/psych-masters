## ----setup--------------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----pkgload, include = FALSE-------------------------------------------------
library("papaja")
r_refs("r-references.bib")
library("ggplot2")
library("lme4")
library("brms")
library("sjPlot")
library("psych")
library("bayesplot")
library("effects")
library("texreg")

## ----study1_method_setup, include = FALSE-------------------------------------
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

## ----study1_methods_demographics, echo=FALSE, message=FALSE, results="asis", tab.cap="Demographic information for Study 1"----
s1_d %>%
  tbl_summary(
    by = condition,
    type = "Age" ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = Year ~ "School Year",
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% add_p() %>% bold_labels() %>% bold_p ()

## ----study1_setup, include = FALSE--------------------------------------------
library(lme4)
library("texreg")
library(ggplot2)

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

## ----study1_tab_freq, tab.cap= "Frequentist Model of Intervention on UHC Support", echo=FALSE, results="asis"----
texreg(study_1_m, custom.model.names = "Multi Level Model - Intercept Varies by Subject",
       custom.coef.names = c("Control","Active Intervention ","Passive Intervention","Post-Measurement Effect", "Interaction between Active Intervention and Post-Measurement", "Interaction between Passive Intervention and Post-Measurement"),
       caption = "Frequentist Table of Intervention on UHC Support", label = "tab:study_1_tabs",
       ci.force = TRUE) 

## ----study1_plot1, fig.cap="Boxplot showing effect of interventions on support for UHC", echo=FALSE----

## Graphs with pre vs post data, boxplot
plot3<-ggplot(UHC_final, aes(x=variable, y=value, color=condition)) +
  geom_boxplot() + labs(title="Effect of Intervention on Support for UHC - Study 1", x = "Time-point (pre or post intervention)", y = "Support for UHC") + 
  scale_color_manual(labels = c("No Intervention", "Active Intervention", "Passive Intervention"), 
                     values=c("red", "blue", "black")) 
plot3 + facet_wrap(~ condition) + theme(legend.position="none")

## ----study2_method_setup, include = FALSE-------------------------------------
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

## ----study2_methods_demographics, echo=FALSE, message=FALSE, results="asis", tab.cap="Demographic information for Study 1"----
setwd("D:/Grad School/2020-2021/psych-masters")
UHC<-read.csv("UHC_rdy_for_analysis_2020.csv")

demog<-UHC[,c(25:28,15)]

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
#   as_gt() %>%
#  gt::gtsave(filename = "demog.html")

## ----study2_setup, include = FALSE--------------------------------------------
library(lme4)
library("texreg")
library(ggplot2)
library(qwraps2)

#study 1 data
setwd("D:/Grad School/2020-2021/psych-masters")
UHC<-read.csv("UHC_rdy_for_analysis_2020.csv")
s1_d<-read.csv("study_1_demog.csv")
s1_d<-s1_d[,-1]

UHC_final<-read.csv("study_1_final.csv")
#changing to allow the correct levels
UHC_final$variable <- factor(UHC_final$variable, levels = c("PRESCORE","POSTSCORE"), labels = c("PRE", "POST"))
#rename our condition var's to have actual names
UHC_final$condition <- factor(UHC_final$condition)
levels(UHC_final$condition) <- c("No Intervention", "Active Intervention","Passive Intervention")

library(sem)
library(lavaan)
library(semPlot)
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
UHC_2<-UHC
UHC_2$change<-UHC$Postscore-UHC$Prescore
UHC_2$change_equality<-UHC$Post_Equality-UHC$Pre_Equality
UHC_2$change_under<-UHC$Post_Understanding-UHC$Pre_Understanding

#using qwraps2 to make our table of means/summary table!~
our_summary1 <-
  list("Change in Support for UHC" =
       list("min"       = ~ min(change),
            "max"       = ~ max(change),
            "mean (sd)" = ~ qwraps2::mean_sd(change)),
       "Change in Percieved Equality" =
       list("min"       = ~ min(change_equality),
            "max"       = ~ max(change_equality),
            "mean (sd)" = ~ qwraps2::mean_sd(change_equality)),
       "Change in Understanding" =
       list("min"       = ~ min(change_under),
            "max"       = ~ max(change_under),
            "mean (sd)" = ~ qwraps2::mean_sd(change_under))
       )
by_cond <- summary_table(dplyr::group_by(UHC_2, condition), our_summary1)



## ----study2_tab1, message=FALSE, warning=FALSE, echo=FALSE, results="asis"----

m1 <- lmer(UHC_Support ~ 0 + condition*Time + (1|Subject), data    = UHC_model_long)
texreg(m1, custom.model.names = "Multi Level Model - Intercept Varies by Subject",
       custom.coef.names = c("Control","Intervention ","Post-Measurement","Interaction of Intervention and Post-Measurement"),
       caption = "Frequentist Table of Intervention on UHC Support", label = "tab:freq-table1",
       ci.force = TRUE)

## ----study2_tab2, message=FALSE, warning=FALSE, echo=FALSE, results="asis"----


by_cond

## ----study2_plot1, fig.cap="Our control condition improved support for UHC while our intervention did not",message=FALSE, warning=FALSE, echo=FALSE----

plot1<-ggplot(UHC_model_long, aes(x=Time, y=UHC_Support, color=condition)) +
  geom_boxplot() 
plot1 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")+labs(x = "Time of Measurement", y = "Support for UHC", title = "Effect of Intervention on Support for UHC - Study 2")+ theme(legend.position="none")


## ----path_dia_equity, fig.cap="Path Diagram showing the effect of condition on UHC Support and Percieved Equity",message=FALSE, warning=FALSE, echo=FALSE----


UHC_model_long$condition<-as.factor(UHC_model_long$condition)
mediation.model <- "UHC_Support ~ Equality + condition
Equality ~ condition"


med1_m<-lm(Equality~condition,UHC_model_long)
#summary(med1_m)
med3_m<-lm(UHC_Support~condition+Equality,UHC_model_long)
#summary(med3_m)

mediation.fit <- sem(mediation.model, data=UHC_model_long)

semPlot::semPaths(mediation.fit, "par", sizeMan = 8, sizeInt = 8, sizeLat = 8, edge.label.cex=1.5,fade=FALSE)

## ----study2_plot2, fig.cap="We see a clear interaction between objective numeracy and the intervention",message=FALSE, warning=FALSE, echo=FALSE----

plot6 <- ggplot(UHC_model_long,aes(x = RNS_score, y = UHC_Support, color = condition)) +
  theme_bw() +
  labs(x = "Objective Numeracy Score",
       y = "Support for UHC",
       color = "Condition",  title = "Moderating Effect of Objective Numeracy on Support for UHC")
plot6 + scale_color_brewer(palette = "Set1")+
  geom_point(position = position_jitter(w = 0.1, h = 0), alpha = .6, size = .9) +
  geom_smooth(method = "lm", alpha =.3, se=FALSE)

## ----study2_plot3, fig.cap="We see no interaction between subjective numeracy and the intervention",message=FALSE, warning=FALSE, echo=FALSE----

plot7 <- ggplot(UHC_model_long,aes(x = SNS_score, y = UHC_Support, color = condition)) +
  theme_bw() +
  labs(x = "Subjective Numeracy Score",
       y = "Support for UHC",
       color = "Condition",  title = "Moderating Effect of Subjective Numeracy on Support for UHC")
plot7 + scale_color_brewer(palette = "Set1")+
  geom_point(alpha = .6, size = .9) +
  geom_smooth(method = "lm", alpha =.3)


