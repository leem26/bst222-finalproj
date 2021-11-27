Sys.time()

###############################################################################
#
# Project:     Preliminary NHANES analysis for simulation set up
# Author:      Matt Lee
# Date:        2021
# Title:       BST 222 Fall 2021 Group Project
# Description: Using NHANES data from 2017-18, obtain estimates of 
#              age, gender, and ethnicity specific BMI curves and quantify
#              amount of misclassification for each group
#
###############################################################################

rm(list=ls())
setwd("/Volumes/GoogleDrive-101809232694958266345/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj/")

# load packages
library(haven)
library(readxl)
library(survey)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)


# read in data
demo.raw  <- read_xpt("data/raw-nhanes/DEMO_J.xpt")
bmx.raw   <- read_xpt("data/raw-nhanes/BMX_J.xpt")
bpq.raw   <- read_xpt("data/raw-nhanes/BPQ_J.xpt")
bpx.raw   <- read_xpt("data/raw-nhanes/BPX_J.xpt")
dexa.raw  <- read_xpt("data/raw-nhanes/DXXAG_J.xpt")
whq.raw   <- read_xpt("data/raw-nhanes/WHQ_J.xpt")


# merge data 
nhanes <- demo.raw %>% 
    left_join(bmx.raw, by = "SEQN") %>% 
    left_join(bpq.raw, by = "SEQN") %>% 
    left_join(bpx.raw, by = "SEQN") %>% 
    left_join(dexa.raw, by = "SEQN") %>% 
    left_join(whq.raw, by = "SEQN")

names(nhanes)

# save data for future work if needed
write.csv(nhanes, "data/nhanes_all.csv")
saveRDS(nhanes, 'data/nhanes_all.Rds')

# lowercase names
names(nhanes) <- tolower(names(nhanes))


# CLEAN VARIABLES FOR ANALYSIS ----------------------------------------------

#' CODEBOOK details
#' riagendr: 1 = male, 2 = female
#' ridageyr: age in years at screening
#' ridreth3: 1 = mexican am, 2 = other hisp, 3 = nh white, 4 = nh black, 6 = nh asian, 7 = other
#' bmxbmi: measured BMI in kg/m^2
#' whd020: self reported weight in lbs
#' whd010: self reported height in inches
#' bpxsy1: systolic BP measured
#' bpxdi1: diastolic BP measured
#' bpq020: ever told high blood pressure 

# discretize age into buckets
nhanes$age4 <- cut(nhanes$ridageyr, c(0, 20, 45, 65, Inf), include.lowest = TRUE, right = FALSE)

# collapse race/ethnicity
nhanes$eth5 <- 
    ifelse(nhanes$ridreth3 %in% c(1, 2), "Hispanic", 
        ifelse(nhanes$ridreth3 == 3, "NH White", 
            ifelse(nhanes$ridreth3 == 4, "NH Black", 
                ifelse(nhanes$ridreth3 == 6, "NH Asian", 
                    ifelse(nhanes$ridreth3 == 7, "Other", NA)))))

# factor gender
nhanes$gender <- factor(nhanes$riagendr, levels = c(1, 2), labels = c("Male", "Female"))

# measured BMI 
nhanes$bmi_m <- nhanes$bmxbmi

# obesity category
nhanes$ob <- 
    ifelse(nhanes$bmxbmi <= 25, "Under/Normal", 
        ifelse(nhanes$bmxbmi < 30, "Overweight", "Obese"))

# self reported weight (in lbs)
nhanes$whd020 <- ifelse(nhanes$whd020 <= 500, nhanes$whd020, NA) # set don't know to missing

# self reported height (in inches)
nhanes$whd010 <- ifelse(nhanes$whd010 <= 100, nhanes$whd010, NA) # set d.k. or refused to missing

# generate self reported BMI 
nhanes$bmi_s <- (nhanes$whd020*0.453592) / (nhanes$whd010*0.0254)^2

# measured hypertension (just using first reading, for simplicity)
# hypertension defined as SBP >= 130 OR DBP >=80 (https://www.heart.org/en/health-topics/high-blood-pressure/understanding-blood-pressure-readings)
nhanes$htn_m <- 0
nhanes$htn_m[(nhanes$bpxsy1 >= 130 | nhanes$bpxdi1 >= 80)] <- 1
nhanes$htn_m <- ifelse((is.na(nhanes$bpxsy1) | is.na(nhanes$bpxdi1)), NA, nhanes$htn_m)

# self reported hypertension
nhanes$htn_s <- 
    ifelse(nhanes$bpq020 == 1, 1, 
        ifelse(nhanes$bpq020 == 2, 0, NA))

# ANALYSIS ----------------------------------------------

# get summaries of measured/surrogate BMI and hypertension
# bmi_error is defined as measured BMI - true BMI, since accoring to
# classical measurement model, X = T + error => X - T = error

# CASE 1: misclassification is random marginally

test = nhanes %>% 
    mutate(bmi_error = bmi_s - bmi_m)
hist(test$bmi_error)

case1_tab <- 
    nhanes %>% 
    mutate(bmi_error = bmi_s - bmi_m) %>% 
    summarize(mu.bmi_m = mean(bmi_m, na.rm = TRUE), sigma.bmi_m = sd(bmi_m, na.rm = TRUE),
              mu.bmi_s = mean(bmi_s, na.rm = TRUE), sigma.bmi_s = sd(bmi_s, na.rm = TRUE),
              mu.bmi_err = mean(bmi_error, na.rm = TRUE), sigma.bmi_err = sd(bmi_error, na.rm = TRUE),
              p.htn_m = mean(htn_m, na.rm = TRUE), p.htn_s = mean(htn_s, na.rm = TRUE),
              p.htn_misclassified = mean(htn_m != htn_s, na.rm = TRUE)) %>% 
    drop_na() %>% 
    write.csv(., "out/case1_params.csv")  

# CASE 2: misclassification depends on ethnicity alone
case2_tab <- 
    nhanes %>% 
    group_by(eth5) %>% 
    mutate(bmi_error = bmi_s - bmi_m) %>% 
    summarize(mu.bmi_m = mean(bmi_m, na.rm = TRUE), sigma.bmi_m = sd(bmi_m, na.rm = TRUE),
              mu.bmi_s = mean(bmi_s, na.rm = TRUE), sigma.bmi_s = sd(bmi_s, na.rm = TRUE),
              mu.bmi_err = mean(bmi_error, na.rm = TRUE), sigma.bmi_err = sd(bmi_error, na.rm = TRUE),
              p.htn_m = mean(htn_m, na.rm = TRUE), p.htn_s = mean(htn_s, na.rm = TRUE),
              p.htn_misclassified = mean(htn_m != htn_s, na.rm = TRUE)) %>% 
    drop_na() %>% 
    write.csv(., "out/case2_params.csv")    

# CASE 3: misclassification depends on gender alone
case3_tab <- 
    nhanes %>% 
    group_by(gender) %>% 
    mutate(bmi_error = bmi_s - bmi_m) %>% 
    summarize(mu.bmi_m = mean(bmi_m, na.rm = TRUE), sigma.bmi_m = sd(bmi_m, na.rm = TRUE),
              mu.bmi_s = mean(bmi_s, na.rm = TRUE), sigma.bmi_s = sd(bmi_s, na.rm = TRUE),
              mu.bmi_err = mean(bmi_error, na.rm = TRUE), sigma.bmi_err = sd(bmi_error, na.rm = TRUE),
              p.htn_m = mean(htn_m, na.rm = TRUE), p.htn_s = mean(htn_s, na.rm = TRUE),
              p.htn_misclassified = mean(htn_m != htn_s, na.rm = TRUE)) %>% 
    drop_na() %>% 
    write.csv(., "out/case3_params.csv")   

# CASE 4: misclassification depends on obesity alone   
case4_tab <- 
    nhanes %>% 
    group_by(ob) %>% 
    mutate(bmi_error = bmi_s - bmi_m) %>% 
    summarize(mu.bmi_m = mean(bmi_m, na.rm = TRUE), sigma.bmi_m = sd(bmi_m, na.rm = TRUE),
              mu.bmi_s = mean(bmi_s, na.rm = TRUE), sigma.bmi_s = sd(bmi_s, na.rm = TRUE),
              mu.bmi_err = mean(bmi_error, na.rm = TRUE), sigma.bmi_err = sd(bmi_error, na.rm = TRUE),
              p.htn_m = mean(htn_m, na.rm = TRUE), p.htn_s = mean(htn_s, na.rm = TRUE),
              p.htn_misclassified = mean(htn_m != htn_s, na.rm = TRUE)) %>% 
    drop_na() %>% 
    write.csv(., "out/case4_params.csv")   

# CASE 5: misclassification depends on obesity, ethnicity, and gender status
case5_tab <- 
    nhanes %>% 
    group_by(ob, eth5, gender) %>% 
    mutate(bmi_error = bmi_s - bmi_m) %>% 
    summarize(mu.bmi_m = mean(bmi_m, na.rm = TRUE), sigma.bmi_m = sd(bmi_m, na.rm = TRUE),
              mu.bmi_s = mean(bmi_s, na.rm = TRUE), sigma.bmi_s = sd(bmi_s, na.rm = TRUE),
              mu.bmi_err = mean(bmi_error, na.rm = TRUE), sigma.bmi_err = sd(bmi_error, na.rm = TRUE),
              p.htn_m = mean(htn_m, na.rm = TRUE), p.htn_s = mean(htn_s, na.rm = TRUE),
              p.htn_misclassified = mean(htn_m != htn_s, na.rm = TRUE)) %>% 
    drop_na() %>% 
    write.csv(., "out/case5_params.csv")    


# For report, plot distributions of BMI by obesity and gender
plot_dat <- 
    nhanes %>% 
    select(bmi_s, bmi_m, ob, eth5, age4, htn_m, htn_s, gender) %>% 
    drop_na()
plot_dat$ob <- factor(plot_dat$ob, levels = c("Under/Normal", "Overweight", "Obese"), labels = c("Under/Normal", "Overweight", "Obese"))

plot <- 
    ggplot(plot_dat) + 
    geom_density(aes(x = bmi_m, fill = "Measured BMI"), color = NA, alpha = .5, adjust = 1.5) +
    geom_density(aes(x = bmi_s, fill = "Self-Reported BMI"), color = NA, alpha = .5, adjust = 1.5) +
    facet_wrap(gender ~ ob, scales = "free") +
    scale_fill_jama() +
    labs(title = "Measured vs. Self-Reported BMI, by Gender and Measured Obesity Status from NHANES",
         x = "Body Mass Index (kg/m^2)", y = "Density") +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom",
          plot.title = element_text(face = "bold")) 

# save BMI plot
ggsave(plot = plot, filename = "out/fig/NHANES_bmi.pdf", device = "pdf", height = 6, width = 10, units = "in")

# For report, plot probability of hypertension by obesity and gender

tmp <- 
    nhanes %>% 
    group_by(ob, gender) %>% 
    summarize(value = mean(htn_s, na.rm = TRUE)) %>% 
    mutate(fill = "Self Reported HTN Prevalence") %>% 
    drop_na()

plot_dat <- 
    nhanes %>% 
    group_by(ob, gender) %>% 
    summarize(value = mean(htn_m, na.rm = TRUE)) %>% 
    mutate(fill = "Measured HTN Prevalence") %>% 
    drop_na()  %>% 
    bind_rows(tmp)
plot_dat$ob <- factor(plot_dat$ob, levels = c("Under/Normal", "Overweight", "Obese"), labels = c("Under/Normal", "Overweight", "Obese"))

plot <- 
    ggplot(plot_dat) + 
    geom_bar(aes(x = gender, y = value, fill = fill), position = "dodge2", stat = "identity",  color = NA) +
    facet_wrap(. ~ ob ) +
    scale_fill_jama() +
    labs(title = "Measured vs. Self-Reported Hypertension Status, by Gender and Measured Obesity Status from NHANES",
         x = "Gender", y = "Prevalence") +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom",
          plot.title = element_text(face = "bold")) 

# save htn plot
ggsave(plot = plot, filename = "out/fig/NHANES_htn.pdf", device = "pdf", height = 6, width = 10, units = "in")


















