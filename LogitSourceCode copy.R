rm(list = ls())
library(glmnet)
library(glmnetUtils)
library(plotROC)

#********************LOGISTIC REGRESSION********************

#Read in data 
data1 <- read.csv("~/Desktop/Econometrics/ERS Final Project/COMPILED DATA.csv", stringsAsFactors = FALSE)


#These two functions will give you the type of data that's in the date column and it's structure
class(data1$date)
str(data1$date)

#Converts date data from a character vector into a "Date" vector
data1$date <- as.Date(data1$date,format = "%m/%d/%Y")
class(data1$date)




#Create Binary/Classification Variable to split good returns and bad returns
logit_data <- select(data1,c(date, 
                             WTI_return, 
                             ppi_oil, 
                             inflation_rate, 
                             import_opec,
                             primary_energy_cons,
                             primary_energy_import_net,
                             tb_rate,
                             nasdaq_comp,
                             steel_prod,
                             b_com,
                             us_rig,
                             n_gas))
class(logit_data$date)


#Split returns into good and bad returns based off of median value
summary(logit_data$WTI_return)
logit_data <- logit_data %>% mutate(good_return = ifelse(WTI_return > 1.7150,1,0))


#********************GENERATE MODEL AND PREDICTIONS********************

logit_mod <- glm(good_return ~ date + 
                         ppi_oil + 
                         inflation_rate + 
                         import_opec + 
                         primary_energy_cons + 
                         primary_energy_import_net + 
                         tb_rate +
                         nasdaq_comp + 
                         steel_prod + 
                         b_com + 
                         us_rig + 
                         n_gas,
                       family = binomial,
                       data = logit_data)

summary(logit_mod)
library(jtools)
summ(logit_mod, exp = TRUE)


logit_data <- data.frame(
  scores_logit = predict(logit_mod, data = logit_data, type = "response"),
  logit_data)
logit_data[1:5,1:3]

#Decided to use a cut-off of .5...meaning that if you have over a 50% chance of being a good_return...then you will be a positive
logit_data <- logit_data %>% mutate(PosNeg05 = 
                                              ifelse(scores_logit > 0.5 & good_return == 1, "TP",
                                                     ifelse(scores_logit > 0.5 & good_return == 0, "FP",
                                                            ifelse(scores_logit <= 0.5 & good_return == 0, "TN",
                                                                   ifelse(scores_logit <= 0.5 & good_return == 1, "FN",0)))))

logit_data <- data.frame(class_pred05 = ifelse(logit_data$scores_logit
                                                   >.5,"Predicted Yes","Predicted No"), logit_data)


#********************ROBUSTNESS CHECKS: ACCURACY, ROC PLOT, AUC********************
matrix_logit <- table(logit_data$class_pred05,
                      logit_data$good_return)
matrix_logit

train_accuracy <- data.frame(
  sensi_TPrate = matrix_logit [2,2]/(matrix_logit[2,2]+matrix_logit[1,2]),
  speci_TNrate = matrix_logit [1,1]/(matrix_logit[1,1]+matrix_logit[2,1]),
  FP_rate = matrix_logit[2,1]/(matrix_logit[1,1]+matrix_logit[2,1]),
  accuracy = (matrix_logit[2,2]+matrix_logit[1,1])/nrow(logit_data)
)
train_accuracy

#********************ROC PLOTS********************

#This will show you how your cut-off probability affects the FP rate and the TP rate
logit_ROC <- ggplot(logit_data,
                    aes(m = scores_logit,
                        d = good_return)) + 
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.3,.1,0)) + labs(title = "Receiver Operator Curve: Good Returns")
plot(logit_ROC)


#Area Under Curve...Our Model is actually decent! It's not the best, but it's acceptable
#.7-.8 are acceptable....85-.9 is very good...and anything above .9 is excellent
calc_auc(logit_ROC)

#I would adjust cut-off to .6





