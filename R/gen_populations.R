Sys.time()

###############################################################################
#
# Project:     Generate simulated populations
# Author:      Matt Lee
# Date:        2021
# Title:       Generated simulated populations for BST 222 Project
# Description: Using misclassification parameters from NHANES (nhanes_analysis.R),
#              set up simulation populations for each misclassification case and
#              population size 
#
#              CASES:
#               1) Random marginally
#               2) Depends on ethnicity
#               3) Depends on gender
#               4) Depends on obesity
#               5) Depends on ethnicity, gender, and obesity
#              
#              For each case, generate 100 datasets 
#
###############################################################################

# MUST SET WORKING DIRECTORY TO PROJECT ROOT
setwd("/Volumes/GoogleDrive-101809232694958266345/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj")

# METHODS FOR GENERATING ----------------------------------------------

# Generate baseline population of size n with even distributions of ethnicity, gender, and obesity
#
# @param n size of the population to generate framework for
# @return data.frame with number of rows = n, with sampled ethnicity, obesity status, and gender
sim_baseline <- function(n) {
    dat          = data.frame(id = 1:n)
    dat$eth5     = sample(x = c("Hispanic", "NH Asian", "NH Black", "NH White", "Other"), replace = TRUE, size = n) 
    dat$ob       = sample(x = c("Obese", "Overweight", "Under/Normal"), replace = TRUE, size = n) 
    dat$gender   = sample(x = c("Male", "Female"), replace = TRUE, size = n)

    return(dat)
}

# Simulate case 1 data
# 
# Case 1 corresponds to the case where individuals are misclassified completely
# at random, i.e. completely independent of eth, ob, or gender
#
# @param popsize size of the population to generate case 1 misclassified data
sim_case1 <- function(popsize) {
    dat = sim_baseline(n = popsize)
    params = read.csv("out/case1_params.csv") 
    ids = sample(dat$id, params$p.htn_misclassified*popsize) # id's to misclassify HTN for   
    for (i in 1:nrow(dat)) {
        # measured BMI
        dat[i, "bmi_m"] = rnorm(1, mean = params$mu.bmi_m, sd = params$sigma.bmi_m)

        # misclassified BMI
        dat[i, "bmi_s"] = dat[i, "bmi_m"] + rnorm(1, mean = params$mu.bmi_err, sd = params$sigma.bmi_err)

        # measured HTN status
        dat[i, "htn_m"] = rbinom(1, 1, p = params$p.htn_m)

        # misclassify HTN
        dat[i, "htn_s"] = ifelse((dat[i, "id"] %in% ids), abs(dat[i, "htn_m"] - 1), dat[i, "htn_m"])
    }
    return(dat)
}

# Simulate case 2 data
# 
# Case 2 corresponds to the case where individuals are misclassified 
# by ethnicity
#
# @param popsize size of the population to generate case 2 misclassified data
sim_case2 <- function(popsize) {
    dat = sim_baseline(n = popsize)
    params = read.csv("out/case2_params.csv") 
    
    for (ethcat in unique(params$eth5)) {
        ids = sample(dat$id[dat$eth5 == ethcat], 
                     params$p.htn_misclassified[params$eth5 == ethcat]*nrow(dat[dat$eth5 == ethcat,])) # id's to misclassify HTN for   
        for (i in dat$id[dat$eth5 == ethcat]) {
            # measured BMI
            dat[i, "bmi_m"] = rnorm(1, mean = params$mu.bmi_m[params$eth5 == ethcat], 
                sd = params$sigma.bmi_m[params$eth5 == ethcat])

            # misclassified BMI
            dat[i, "bmi_s"] = dat[i, "bmi_m"] + rnorm(1, mean = params$mu.bmi_err[params$eth5 == ethcat], 
                sd = params$sigma.bmi_err[params$eth5 == ethcat])

            # measured HTN status
            dat[i, "htn_m"] = rbinom(1, 1, p = params$p.htn_m[params$eth5 == ethcat])

            # misclassify HTN
            dat[i, "htn_s"] = ifelse(dat[i, "id"] %in% ids, abs(dat[i, "htn_m"] - 1), dat[i, "htn_m"])
        }
    }
    return(dat)
}


# Simulate case 3 data
# 
# Case 3 corresponds to the case where individuals are misclassified 
# by gender
#
# @param popsize size of the population to generate case 3 misclassified data
sim_case3 <- function(popsize) {
    dat = sim_baseline(n = popsize)
    params = read.csv("out/case3_params.csv") 
    
    for (gen in unique(params$gender)) {
        ids = sample(dat$id[dat$gender == gen], 
                     params$p.htn_misclassified[params$gender == gen]*nrow(dat[dat$gender == gen,])) # id's to misclassify HTN for   
        for (i in dat$id[dat$gender == gen]) {
            # measured BMI
            dat[i, "bmi_m"] = rnorm(1, mean = params$mu.bmi_m[params$gender == gen], 
                sd = params$sigma.bmi_m[params$gender == gen])

            # misclassified BMI
            dat[i, "bmi_s"] = dat[i, "bmi_m"] + rnorm(1, mean = params$mu.bmi_err[params$gender == gen], 
                sd = params$sigma.bmi_err[params$gender == gen])

            # measured HTN status
            dat[i, "htn_m"] = rbinom(1, 1, p = params$p.htn_m[params$gender == gen])

            # misclassify HTN
            dat[i, "htn_s"] = ifelse(dat[i, "id"] %in% ids, abs(dat[i, "htn_m"] - 1), dat[i, "htn_m"])
        }
    }
    return(dat)
}

# Simulate case 4 data
# 
# Case 4 corresponds to the case where individuals are misclassified 
# by obesity category
#
# @param popsize size of the population to generate case 3 misclassified data
sim_case4 <- function(popsize) {
    dat = sim_baseline(n = popsize)
    params = read.csv("out/case4_params.csv") 
    
    for (obcat in unique(params$ob)) {
        ids = sample(dat$id[dat$ob == obcat], 
                     params$p.htn_misclassified[params$ob == obcat]*nrow(dat[dat$ob == obcat,])) # id's to misclassify HTN for   
        for (i in dat$id[dat$ob == obcat]) {
            # measured BMI
            dat[i, "bmi_m"] = rnorm(1, mean = params$mu.bmi_m[params$ob == obcat], 
                sd = params$sigma.bmi_m[params$ob == obcat])

            # misclassified BMI
            dat[i, "bmi_s"] = dat[i, "bmi_m"] + rnorm(1, mean = params$mu.bmi_err[params$ob == obcat], 
                sd = params$sigma.bmi_err[params$ob == obcat])

            # measured HTN status
            dat[i, "htn_m"] = rbinom(1, 1, p = params$p.htn_m[params$ob == obcat])

            # misclassify HTN
            dat[i, "htn_s"] = ifelse(dat[i, "id"] %in% ids, abs(dat[i, "htn_m"] - 1), dat[i, "htn_m"])
        }
    }
    return(dat)
}

# Simulate case 4 data
# 
# Case 5 corresponds to the case where individuals are misclassified 
# by gender, eth, and obesity category
#
# @param popsize size of the population to generate case 3 misclassified data
sim_case5 <- function(popsize) {
    dat = sim_baseline(n = popsize)
    params = read.csv("out/case5_params.csv") 
    
    for (ethcat in unique(params$eth5)) {
        for (gen in unique(params$gender)) {
            for (obcat in unique(params$ob)) {
                ids = sample(dat$id[dat$eth5 == ethcat & dat$gender == gen & dat$ob == obcat], 
                     params$p.htn_misclassified[params$eth5 == ethcat & params$gender == gen & params$ob == obcat]*nrow(dat[dat$eth5 == ethcat & dat$gender == gen & dat$ob == obcat,])) # id's to misclassify HTN for   
                for (i in dat$id[dat$eth5 == ethcat & dat$gender == gen & dat$ob == obcat]) {
                    # measured BMI
                    dat[i, "bmi_m"] = rnorm(1, mean = params$mu.bmi_m[params$eth5 == ethcat & params$gender == gen & params$ob == obcat], 
                        sd = params$sigma.bmi_m[params$eth5 == ethcat & params$gender == gen & params$ob == obcat])

                    # misclassified BMI
                    dat[i, "bmi_s"] = dat[i, "bmi_m"] + rnorm(1, mean = params$mu.bmi_err[params$eth5 == ethcat & params$gender == gen & params$ob == obcat], 
                        sd = params$sigma.bmi_err[params$eth5 == ethcat & params$gender == gen & params$ob == obcat])

                    # measured HTN status
                    dat[i, "htn_m"] = rbinom(1, 1, p = params$p.htn_m[params$eth5 == ethcat & params$gender == gen & params$ob == obcat])

                    # misclassify HTN
                    dat[i, "htn_s"] = ifelse(dat[i, "id"] %in% ids, abs(dat[i, "htn_m"] - 1), dat[i, "htn_m"])
                }
            }
        }
    }
    return(dat)
}


# GENERATE POPULATIONS ----------------------------------------------

npops <- 100
sizes <- c(100, 500, 1000, 5000, 10000)

j <- 0
while (j < npops) {
    for (i in sizes) {
        fn1 = paste0("data/simdata/case1/n", i, "/pop_", j, ".csv")
        case1 = sim_case1(i); write.csv(x = case1, file = fn1)

        fn2 = paste0("data/simdata/case2/n", i, "/pop_", j, ".csv")
        case2 = sim_case2(i); write.csv(x = case2, file = fn2)

        fn3 = paste0("data/simdata/case3/n", i, "/pop_", j, ".csv")
        case3 = sim_case3(i); write.csv(x = case3, file = fn3)

        fn4 = paste0("data/simdata/case4/n", i, "/pop_", j, ".csv")
        case4 = sim_case4(i); write.csv(x = case4, file = fn4)

        fn5 = paste0("data/simdata/case5/n", i, "/pop_", j, ".csv")
        case5 = sim_case5(i); write.csv(x = case5, file = fn5)
    }
    # increment j
    j <- j + 1
}









