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
  geom_boxplot() + labs(title="", x = "Time-point (pre or post intervention)", y = "Support for UHC") + 
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

## ----appendix_b1, echo=FALSE, fig.cap="Screenshot of Web Exercise for Intervention Condition", out.width = '100%'----
knitr::include_graphics("appendix_b_1.png")

## ----appendix_b2, echo=FALSE, fig.cap="Screenshot showing legend and web exercise for intervention condition", out.width = '100%'----
knitr::include_graphics("appendix_b_2.png")

## ----appendix_b3, echo=FALSE, fig.cap="1st Infographic for control condition", out.width = '100%'----
knitr::include_graphics("appendix_b_3.png")

## ----appendix_b4, echo=FALSE, fig.cap="2nd Infographic for control condition", out.width = '100%'----
knitr::include_graphics("appendix_b_4.png")

## ----appendix_b5, echo=FALSE, fig.cap="3rd Infographic for control condition", out.width = '100%'----
knitr::include_graphics("appendix_b_5.png")

## ----appendix_b6, echo=FALSE, fig.cap="Support for UHC Measure - Scale and Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_6.png")

## ----appendix_b7, echo=FALSE, fig.cap="Percieved Equity Measure - Scale and Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_7.png")

## ----appendix_b8, echo=FALSE, fig.cap="Percieved Understanding Measure - Scale and Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_8.png")

## ----appendix_b9, echo=FALSE, fig.cap="Subjective Numeracy Scale Part 1 - Scale and Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_9.png")

## ----appendix_b10, echo=FALSE, fig.cap="Subjective Numeracy Scale Part 2 - Scale and Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_10.png")

## ----appendix_b11, echo=FALSE, fig.cap="Rasch Numeracy Scale (Objective Numeracy) - Item wording", out.width = '100%'----
knitr::include_graphics("appendix_b_11.png")

