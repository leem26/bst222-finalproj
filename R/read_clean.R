###############################################################################
#
# Project:     Read and clean NHANES 2017-18 data as needed
# Author:      Matt Lee
# Date:        2021
# Title:       BST 222 Fall 2021 Group Project
# Description: Yes
#
###############################################################################

rm(list=ls())
setwd("/Volumes/GoogleDrive/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj/")

# load packages
library(haven)
library(readxl)
library(survey)
library(dplyr)
library(PAutilities)

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

# save data
write.csv(nhanes, "data/nhanes_all.csv")
