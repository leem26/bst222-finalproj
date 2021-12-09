
library(meerva)
library(tidyr)

# setwd("C:/Users/user/Documents/GitHub/bst222-finalproj/data/simdata")
# setwd("/Volumes/GoogleDrive/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj/data/simdata") # ml path
setwd("/Volumes/GoogleDrive-101809232694958266345/My Drive/Matt/School/PhD /2021-2022/BST 222/Project/bst222-finalproj/data/simdata") # laptop
#case1_100_0<-read.csv("case1/n100/pop_9.csv",header = T)

set.seed(123)

sim_bias <- NULL
output_sim <- NULL
for (i in 1:5) {
  for (j in c(100,500,1000,5000,10000)) {
    for (k in 0:99) {
      simdat<-read.csv(paste0("case",i,"/n",j,"/pop_",k,".csv"),header = T)
      
      #-- estimatie true beta
      #-- (1) beta_full data
      m1 <- glm(htn_m ~ bmi_m + factor(eth5) + factor(ob) + factor(gender), data=simdat, family="binomial")
      beta_full<-m1$coefficients[["bmi_m"]]

      #-- (2) Split the data into val and non-val (4:6)
      val_obs <- round(0.4 * nrow(simdat))
      nonval_obs <- nrow(simdat) - val_obs
      
      val_nonval_vec <- c(rep("val", val_obs),rep("nonval", nonval_obs))
      val_nonval_vec <- sample(val_nonval_vec, nrow(simdat), replace = FALSE)
      val_data <- simdat[which(val_nonval_vec == "val"),]
      nonval_data <- simdat[which(val_nonval_vec == "nonval"),]
      
      #-- (3)
      #-- beta_validation
      m2 <- glm(htn_m ~ bmi_m + factor(eth5) + factor(ob) + factor(gender), data=val_data, family="binomial")
      beta_val<-m2$coefficients[["bmi_m"]]
      
      #-- (4) gamma_nonvalidation
      m3 <- glm(htn_s ~ bmi_s + factor(eth5) + factor(ob) + factor(gender), data=nonval_data, family="binomial")
      gamma_nonval<-m3$coefficients[["bmi_s"]]
      
      #-- (5) meerva
      x_val <- cbind(x1 = val_data$bmi_m, x2 = val_data$eth5_NH.Asian, x3 = val_data$eth5_NH.Black, x4 = val_data$eth5_NH.White, x5 = val_data$eth5_Other, 
                     x6 = val_data$gender_Female, x7 = val_data$ob_Overweight, x8 = val_data$ob_Obese) #reference (gold-standard, measured)
      xs_non <- cbind(x1s = nonval_data$bmi_s, x2 = nonval_data$eth5_NH.Asian, x3 = nonval_data$eth5_NH.Black, x4 = nonval_data$eth5_NH.White, x5 = nonval_data$eth5_Other, 
                     x6 = nonval_data$gender_Female, x7 = nonval_data$ob_Overweight, x8 = nonval_data$ob_Obese) #surrogate (self-reported)
      xs_val <- cbind(x1s = val_data$bmi_s, x2 = val_data$eth5_NH.Asian, x3 = val_data$eth5_NH.Black, x4 = val_data$eth5_NH.White, x5 = val_data$eth5_Other, 
                     x6 = val_data$gender_Female, x7 = val_data$ob_Overweight, x8 = val_data$ob_Obese)
      y_val <- val_data$htn_m # true in validation (gold-standard, measured)
      ys_non <- nonval_data$htn_s # surrogate in non validation (self-reported)
      ys_val <- val_data$htn_s # surrogate in validation
      
      brn.me = meerva.fit(x_val = x_val, y_val = y_val, xs_val = xs_val, ys_val = ys_val, xs_non = xs_non, ys_non = ys_non)
      beta_aug <- brn.me$coef_beta[1,2] 
      
      #-- calculate bias
      bias_val <- beta_val - beta_full
      bias_nonval <- gamma_nonval - beta_full 
      bias_aug <- beta_aug - beta_full 
      
      out <- c(bias_val, bias_nonval, bias_aug, beta_val, gamma_nonval, beta_aug)
      sim_bias <- rbind(sim_bias, out)
    }
    
    #-- calculate mean_bias and mse/var
    mean_bias_val<-mean(sim_bias[,1], na.rm = TRUE)
    mean_bias_nonval<-mean(sim_bias[,2], na.rm = TRUE)
    mean_bias_aug<-mean(sim_bias[,3], na.rm = TRUE)
    
    mse_val<-mean(sim_bias[,1]^2, na.rm = TRUE)
    mse_nonval<-mean(sim_bias[,2]^2, na.rm = TRUE)
    mse_aug<-mean(sim_bias[,3]^2, na.rm = TRUE)

    var_val <- var(sim_bias[,4], na.rm = TRUE)
    var_nonval <- var(sim_bias[,5], na.rm = TRUE)
    var_aug <- var(sim_bias[,6], na.rm = TRUE)
    
    output <- c(i,j,mean_bias_val,mean_bias_nonval,mean_bias_aug,
                mse_val,mse_nonval,mse_aug, var_val, var_nonval, var_aug)

    output_sim <- rbind(output_sim,output)

    colnames(output_sim) <- c("case","n","mean_bias_val","mean_bias_nonval",
                              "mean_bias_aug","mse_val","mse_nonval","mse_aug", 
                              "var_val", "var_nonval", "var_aug")
  }
}


#------ 
# plot
#------
#-- wide to long
output_sim_bias <- melt(as.data.frame(output_sim[,1:5]), id.vars = c("case","n"), variable.name = "estimator")
output_sim_mse  <- melt(as.data.frame(output_sim[,c(1:2,6:8)]), id.vars = c("case","n"), variable.name = "estimator")
output_sim_var  <- melt(as.data.frame(output_sim[,c(1:2,9:11)]), id.vars = c("case","n"), variable.name = "estimator")

# # empirical variance
# output_sim_long <- melt(as.data.frame(output_sim), id.vars = c("case","n"), variable.name = "estimator")
# estimator_var <- data.table(output_sim_long)[, .(var_est = var(value, na.rm = TRUE)), by = c("case", "n", "estimator")]

#...error - don't know why..

# Plot of bias, facet by case
biasplot <- output_sim_bias %>% 
  ggplot(aes(x = n, y = value, color = estimator)) + 
  geom_point() + 
  geom_line(alpha = 0.2) + 
  geom_abline(slope = 0, intercept = 0, alpha = 0.2) + 
  facet_wrap(~case) + 
  labs(x = "Sample size", y = "Mean bias") + 
  theme_classic() + 
  theme(legend.title = element_blank()) 

ggsave(biasplot, filename = "../../out/fig/bias_plot.pdf", device = cairo_pdf, width = 8.5, height = 5, units = "in")

# Plot of variance, facet by case
varplot <- output_sim_var %>% 
  ggplot(aes(x = n, y = value, color = estimator)) + 
  geom_point() + 
  geom_line(alpha = 0.2) + 
  geom_abline(slope = 0, intercept = 0, alpha = 0.2) + 
  facet_wrap(~case) + 
  labs(x = "Sample size", y = "Sampling Variance") + 
  theme_classic() + 
  theme(legend.title = element_blank()) 

ggsave(varplot, filename = "../../out/fig/var_plot.pdf", device = cairo_pdf, width = 8.5, height = 5, units = "in")

# Plot of mse, facet by case
mseplot <- output_sim_mse %>% 
  ggplot(aes(x = n, y = value, color = estimator)) + 
  geom_point() + 
  geom_line(alpha = 0.2) + 
  scale_shape_discrete("case") + 
  geom_abline(slope = 0, intercept = 0, alpha = 0.1) + 
  facet_wrap(~case) + 
  labs(x = "Sample size", y = "MSE") + 
  theme_classic() + 
  theme(legend.title = element_blank()) 

ggsave(mseplot, filename = "../../out/fig/mse_plot.pdf", device = cairo_pdf, width = 8.5, height = 5, units = "in")

