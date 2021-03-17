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