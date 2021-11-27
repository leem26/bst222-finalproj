
library(dplyr)
library("Rlab")   
setwd("C:/Users/user/OneDrive - Harvard University/GSAS/PHS/Fall 2021/BST 222/Project/Analysis")
setwd("/Volumes/GoogleDrive-101809232694958266345/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj/") # matt path

# nhanes_all<-read.csv("nhanes_all.csv",header = T)
nhanes_all<-read.csv("data/nhanes_all.csv",header = T)

#"BMX": Body measures exam, including measured height and weight
#"BPQ": Blood pressure and cholesterol self-report questionnaire
#"BPX": Blood pressure measured exam
#"DEMO": Demographics files
#"DXXAG": DEXA (gold standard fat mass measure) measures
#"WHQ": Weight history questionnaire, includes self-reported height and weight

# (measured BMI) BMXBMI (>=25) - x_val
# (self-reported obesity) WHQ030 - How do you consider your weight (1 Overweight)
# ... but to simulate, we need self-reported BMI first (continuous)?
# (self-reported BMI) - xs_non
# WHD010 - Current self-reported height (inches) (7777 Refused, 9999 Don't know)
# WHD020 - Current self-reported weight (pounds) (7777 Refused, 9999 Don't know)

# (measured HTN) BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg (>=130)
#               & BPXDI1 - Diastolic: Blood pres (1st rdg) mm Hg (>=80) - y_val
# (self-reported HTN) BPQ020 - Ever told you had high blood pressure (1 Yes, 2 No) - ys_non

nhanes<-nhanes_all %>% select(c("BMXBMI","WHQ030","BPQ020","BPXSY1","BPXDI1",
                                "WHD010","WHD020"))
summary(nhanes)

###########
# m-measured vs. s-self-reported
###########
# BMI
nhanes$bmi_m<-nhanes$BMXBMI

nhanes$WHD020[nhanes$WHD020>=7777]=NA
nhanes$wt_s<-nhanes$WHD020/2.205 #kg
nhanes$WHD010[nhanes$WHD010>=7777]=NA
nhanes$ht_s<-nhanes$WHD010/39.37 #meter

nhanes$bmi_s<-(nhanes$wt_s)/(nhanes$ht_s)^2

# 1 overweight vs. 0 not overweight (incl. refused, don't know for self-reported)
nhanes$overwt_m<-cut(nhanes$BMXBMI,c(0,25,Inf),
                   labels=c(0,1))
nhanes$overwt_s<-cut(nhanes$WHQ030,c(0,1,Inf),
                     labels=c(1,0))

# 1 HTN vs. 0 not HTN (incl. refused, don't know for self-reported)
nhanes$htn_m=0
nhanes$htn_m[nhanes$BPXSY1>=130 & nhanes$BPXDI1>=80]=1
#table(nhanes$htn_m)

nhanes$htn_s<-cut(nhanes$BPQ020,c(-Inf,1,Inf),
                     labels=c(1,0))
#table(nhanes$htn_s)

#----------------------
# simulate - bmi from Normal, htn from Bernoulli
bmi_m <- rnorm(n=1000, mean=mean(nhanes$bmi_m,na.rm=T), sd=sd(nhanes$bmi_m,na.rm=T)) #(1,1000)
bmi_s <- rnorm(n=1000, mean=mean(nhanes$bmi_s,na.rm=T), sd=sd(nhanes$bmi_s,na.rm=T)) #(1,1000)

htn_m <- rbern(n=1000,prob=mean(as.numeric(nhanes$htn_m),na.rm=T))
htn_s <- rbern(n=1000,prob=sum(nhanes$htn_s==1,na.rm=T)/sum(!is.na(nhanes$htn_s)))

#x_val <- as.numeric(bmi_m[1:400]) #reference (gold-standard, measured)
#xs_non <- bmi_s[401:1000] #surrogate (self-reported)
#xs_val <- bmi_s[1:400]
# or x_val %>% bmi_m(.4) - w/ replacement

x_val <- cbind(x1 = bmi_m[1:400], x2 = bmi_s[1:400]) #reference (gold-standard, measured)

xs_non <- cbind(x1s = bmi_m[401:1000], x2 = bmi_s[401:1000]) #surrogate (self-reported)

xs_val <- cbind(x1s = bmi_m[1:400], x2 = bmi_s[1:400])

y_val <- htn_m[1:400] # true in validation (gold-standard, measured)

ys_non <- htn_s[401:1000] # surrogate in non validation (self-reported)

ys_val <- htn_s[1:400] # surrogate in validation

#----------------------
# Analyze the data
brn.me = meerva.fit(x_val, y_val, xs_val, ys_val, xs_non, ys_non)
summary(brn.me) #--error

