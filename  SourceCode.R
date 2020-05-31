#ERS Final Project Rough Code
#Julian Murillo
#2301366
rm(list = ls())
library(tidyverse)
library(caret)
library(lmtest)
library(xts)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)
library(ggplot2)
library(plotly)
library(glmnet)
library(glmnetUtils)
library(partykit)
library(magrittr)
library(leaps)
library(ggridges)

#Read-in data
data1 <- read.csv("~/Desktop/Econometrics/ERS Final Project/COMPILED DATA.csv", stringsAsFactors = FALSE)

#Convert Data to Time-Series using ts function
#Run this code to see which columns are numeric and which are not 
sapply(data1, is.numeric)

#These two functions will give you the type of data that's in the date column and it's structure
class(data1$date)
str(data1$date)

#Converts date data from a character vector into a "Date" vector
data1$date <- as.Date(data1$date,format = "%m/%d/%Y")
class(data1$date)




#Format: c(year, month, day)
#WTI Return
data.ts <- ts(data1$WTI_return, start = c(2000,1,1), end = c(2019,8,1), frequency = 12)
print(data.ts)


#Price: NOT USING AS Y
#data.ts2 <- ts(data1$price, start = c(2000,1,1), end = c(2019,8,1), frequency = 12)
#print(data.ts2)

#nsdiffs(data.ts2)
#data.diff <- diff(data.ts2)
#plot(data.diff, main = "Differenced Price Data", xlab = "Time (Monthly", ylab = "Price (Differenced)")

#data.ts.diff <- diff(data.ts, lag = frequency(data.ts), differences = 1)
#just in case he wants us to specify lag




#***********************VISUALIZE DATE A BIT***********************
#par(mfrow = c(1,2))
plot(data.ts, main = "WTI Returns", col='blue', xlab = "Time (Monthly)", ylab = "Return (Percentage)")
#plot(data.ts2, main = "WTI Price", col='blue', xlab = "Time (Monthly)", ylab = "Price Per Barrel (USD)")
#The Data doesn't look to have any sort of trend...it looks pretty stationary 


#Box Plot of monthly differences
boxplot(data.ts ~ cycle(data.ts), col = "blue", main = "Box-Plots of Return Data (Monthly)", xlab = "Month", ylab = "Returns")
#boxplot(data.ts2 ~ cycle(data.ts2), col = "blue", main = "Box-Plots of Price Data (Monthly)", xlab = "Month", ylab = "Price")

#***********************Decompose Data (WTI Returns) and ADF Test***********************
decomp <- decompose(data.ts)
plot(decomp)
decomp$seasonal
#Still looks stationary

#Plot trend-component only...no trend...stationary 
plot(decomp$trend, main = "Trend Component",col="dodgerblue3", xlab = "Time (Monthly)",lwd=3)

#Seasonality...this is kinda useless...there is no seasonality...I would just ignore this
seasons <- as.data.frame(rep(unique(decomp$seasonal),4))
seasons$index <- 1:nrow(seasons)
names(seasons)[1] <- "seasons"
plot_ly(data = seasons, x = ~index, y = ~seasons, type = "scatter", mode = "lines") %>%
  layout(title = 'WTI Return Seasonality (Quarterly)',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE), showlegend = FALSE)





#Augmented Dickey Fuller Test: If a time series has a unit root, it shows a systematic pattern that is unpredictable and non-stationary.

#ADF Hypothesis Testing:
#Ho: The null hypothesis of Augmented Dickey Fuller Test is that a unit root is present in a time series sample (It is NON-stationary) 
#Ha: The alternative hypothesis is that the data IS stationary or trend-stationary.

#Agumented Dickey Fuller Test: Lag order = 12 (monthly)
frequency(data.ts) #Gives number of periods
adf.test(data.ts, k = 12)
#Very small p-value...Reject Null hypothesis...Our Data is Trend Stationary FOR RETURN!!


#adf.test(data.ts2,k = 12)
#adf.test(data.diff)
#when we ran our test on whether or not price is stationary...we got high p-value! Fail to Reject Null...Data is not Stationary FOR PRICE!


#***********************SIMPLE LINEAR TREND MODEL***********************
lm_1 <- lm(data1$WTI_return ~ data1$date)
summary(lm_1)
#Really low R-Squared...time has very little to do with WTI return....024% of variation in WTI returns is explained by time alone

#lm_2 <- lm(data1$price ~ data1$date)
#summary(lm_2)
#Pretty high adjusted R-Squared...time has more to do with WTI Price....17.6% of variation in WTI Price is explained by time alone
#This must be due to inflation


#***********************Regress against all potential predictor variables**********************8
#Remove non-predictive variables from data set to iterate over entire data set
data2 <- select(data1, -c(price, open, vol))

lm_3 <- lm(WTI_return ~.,
           data = data2)
summary(lm_3)
#R-Squared predicting WTI RETURN against all of our variables actually isn't too bad 30%...

#lm_4 <- lm(price ~.,
#           data = data1)
#summary(lm_4)
#Super high R-Squared...almost 100% variation in PRICE is explained by all variables (Massive collinearity issues though of course)



#***********************VARIABLE SELECTION***********************

# Used to explore how coefficients change as we change lambda
#lambda. min is the value of λ that gives minimum mean cross-validated error. 
#The other λ saved is lambda. 1se , which gives the most regularized model such that error is within one standard error of the minimum.
library(coefplot)



#LASSO 
lasso_mod <- cv.glmnet(WTI_return ~., 
                       data = data2,
                       alpha = 1)
plot(lasso_mod)
coefpath(lasso_mod) #The lambdas are literally negative

round(coef(lasso_mod), digits = 5) 


#ELASTIC NET
alpha_grid <- seq(0,1,len = 11) #creates vector of alphas between 0 and 1...segmented by 1 alpha_grid
alpha_grid
enet_fit <- cva.glmnet(WTI_return ~.,
                       data = data2,
                       alpha = alpha_grid)

#plot

minlossplot(enet_fit)
plot(enet_fit)

#Min CV loss at .8 (CV = Cross-Validated)...if you re-run this you will get different results
#matrix of coefficients at each alpha
enet_coefs <- data.frame(
  varname = rownames(coef(enet_fit,alpha = 0)),
  ridge = as.matrix(coef(enet_fit, alpha = 0)) %>% round(10),
  alpha01 = as.matrix(coef(enet_fit, alpha = 0.1)) %>% round(10),
  alpha02 = as.matrix(coef(enet_fit, alpha = 0.2)) %>% round(10),
  alpha03 = as.matrix(coef(enet_fit, alpha = 0.3)) %>% round(10),
  alpha04 = as.matrix(coef(enet_fit, alpha = 0.4)) %>% round(10),
  alpha05 = as.matrix(coef(enet_fit, alpha = 0.5)) %>% round(10),
  alpha06 = as.matrix(coef(enet_fit, alpha = 0.6)) %>% round(10),
  alpha07 = as.matrix(coef(enet_fit, alpha = 0.7)) %>% round(10),
  alpha08 = as.matrix(coef(enet_fit, alpha = 0.8)) %>% round(10),
  alpha09 = as.matrix(coef(enet_fit, alpha = 0.9)) %>% round(10),
  lasso = as.matrix(coef(enet_fit, alpha = 1)) %>% round(10)
) %>% rename(varname = 1, ridge = 2, alpha01 = 3, alpha02 = 4, alpha03 = 5, alpha04 = 6,
             alpha05 = 7, alpha06 = 8, alpha07 = 9, alpha08 = 10, alpha09 = 11, lasso = 12) %>% 
  remove_rownames()
head(enet_coefs)

select_optimal <- select(enet_coefs,c(varname,alpha08))
select_optimal


#FOWARD STEP-WISE MODEL: WEAK method of subset/variable selection
fwd_fit <-
  regsubsets(WTI_return ~. ,
             data = data2,
             nvmax = 12,
             method = "forward")
summary(fwd_fit)


#BACKWARD STEP-WISE MODEL: WEAK method of subset/variable selection
bkwd_fit <-
  regsubsets(WTI_return ~. ,
             data = data2,
             nvmax = 12,
             method = "backward")
summary(fwd_fit)


#********************CORRELATION MATRIX TO DETECT COLINEARITY********************
cormat <- cor(data2 %>% select_if(is.numeric) %>% drop_na())
cormat

#cormat <- data.frame(round(cormat, 4))
library(corrplot)

devtools::install_github("vsimko/corrplot")
corrplot(cormat, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust")

#corrplot(cormat, type = "full")

#Variance Inflation Factor (VIF)...anything over like 10 is problematic (regular OLS model)
library(olsrr)

#This just excluded VIF Score for "Date"...since it's not numeric
data_for_lm5 <- select(data2,-c(date))
lm_5 <- lm(WTI_return ~.,
           data = data_for_lm5)

summary(lm_5)
VIF_1 <- ols_vif_tol(lm_5)




#******************** MORE EXPLORATORY PLOTS********************

#Create Binary/Classification Variable to split good returns and bad returns...just for ridgeline density plot
binomial_data <- select(data1, -c(open, vol))
binomial_data <- binomial_data %>% mutate(good_return = ifelse(WTI_return > 1.7150,1,0))


p_histo <- ggplot(data = data1, aes(x=WTI_return)) + geom_histogram(color = "green") + scale_x_continuous(limits = c(-10,30)) + 
  scale_y_continuous(limits = c(0,25)) + labs(title = "Histogram: Returns")

plot(p_histo)



#Boxplots of all variables to see distribution...in normal non-binomial data
for (i in 1:ncol(data2)){
  boxplot(data2[,i], main=colnames(data2[i]), col="red")
}


#Scatter of US Unemployment vs WTI Price...as Unemployment Rate Increaes so does Futures Prices...probably due to the price of gasoline
p1 <- ggplot(data1, aes(x = us_unemployment, y = price)) + geom_point(color = "purple") + 
  labs(x = "US Unemployment Rate", y = "WTI Futures Price (USD Per Barrel)", title = "WTI Price vs Unemployment Rate") + stat_smooth()
plot(p1)

#Ridgline Density Plot...weirdly returns seemed to be slightly better during the period of time with the highest unemployment rates...maybe due to short-sales?
library(ggridges)
data_forplot <- binomial_data %>% mutate(good_return2 = ifelse( good_return == 1,"Yes",
                                                                 "No"))

#Although it is a bit difficult to see...there was a greater proportion of returns with a 3 month treasury bill yeild rate over 4%
#This makes sense because this means that the economy is in an expansion if t-bill yeilds are good...which encourages more risky investments such as in equities or in commodities (much more volitile)
p2 <- ggplot(data_forplot, aes(x = tb_rate, y = good_return2, fill = good_return2)) + 
  geom_density_ridges() + 
  scale_x_continuous(limits = c(.01,.617)) + 
  labs(x = "3-Month Treasury Bill Rate", y = "Good Return", title = "Ridgeline Density Plot") 
plot(p2)


#Animated Time-Series Plot...this shows the price of WTI Asset Price over time...spiked in 2008 due to energy crisis and high demand for oil 
#Due to The post-9/11 war on terror, labor strikes, hurricane threats to oil platforms, fires and terrorist threats at refineries, etc.
library(gganimate)
library(png)
library(gifski)

package.list <- c("tidyverse","gapminder","gganimate","png","gifski")
new.packages <- package.list[!(package.list %in% installed.packages()[, "Package"])] 
lapply(package.list, require, character.only = TRUE)



p3 <- ggplot(
  data_forplot, 
  aes(x = date, y= price, colour = good_return2)
) +
  geom_point(alpha = 0.7)  +
  scale_size(range = c(2, 12)) +
  labs(x = "Time", y = "WTI Asset Price") +
  transition_time(date) + labs(title = "Historical WTI Futures Prices (USD Per Barrel) 
       Time (Monthly): {frame_time}")

animate(p3, fps=5)



#Animated Plot of Renewable Energy Production and WTI Asset RETURN

p4 <- ggplot(
  data_forplot, 
  aes(x = renew_energy_prod, y= price, colour = good_return2)
) +
  geom_point(alpha = 0.7)  +
  scale_size(range = c(2, 12)) +
  labs(x = "Renewable Energy Production", y = "WTI Asset Price") + 
  transition_time(date) +
  labs(title = "Renewable Energy Production (Quad BTU) vs WTI Asset Price Time: {frame_time}")

animate(p4, fps=5)








#********************Linear Regression Modeling with filtered/FINAL VARIABLES********************

#Final Variables from LASSO and ELASTIC NET SELECTION
final_data <- select(data1,c(date, 
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
class(final_data$date)

lm_5 <- lm(WTI_return ~.,
           data = final_data)
summary(lm_5)




###********************k-fold Cross validation********************




#------------------------------------------------
WTI_sub <- 
  mutate(final_data,
         folds = createFolds(final_data$WTI_return,
                             k = 10, list = FALSE)
  )

WTI_sub$folds

### K-Fold Cross Validation...and for loop 
nfolds <- 10
preds_10FoldCV_DF <- data.frame(
  folds = WTI_sub$folds,
  preds_10FoldCV = rep(NA,nrow(WTI_sub))
)

#forloop
for(i in 1:nfolds){
  mod <- lm(WTI_return ~ ., 
            data = WTI_sub %>% 
              filter(folds != i))
  preds <- predict(mod, 
                   newdata = WTI_sub %>%
                     filter(folds == i))
  preds_10FoldCV_DF[preds_10FoldCV_DF$folds == i,"preds_10FoldCV"]  <- preds
}


final_data <- data.frame(
  preds_10FoldCV = preds_10FoldCV_DF$preds_10FoldCV,
  final_data  
)
summary(mod)



#Adjusted R-Squared: 0.2522 (Pretty solid in my opinion)...
#considering that if use all 28 predictors we only get a 4% increase in R2

#********************ROBUSTNESS CHECKS********************

#I will calculate R2,RMSE,MAE etc.
#Get predictions
final_data <- data.frame(
  preds_ols = predict(lm_5, data = final_data), 
  final_data)
head(final_data)

final_data <- final_data %>% mutate(
  resids_ols = final_data$WTI_return - final_data$preds_ols, 
  resids_kfold = final_data$WTI_return - final_data$preds_10FoldCV
)



library(caret)
RMSE <- function(t,p) {
  sqrt(sum(((t - p)^2)) * (1/nrow(t)))
}

#R2: Correlation Coefficient
R2_ols <- R2(final_data %>% select(preds_ols), final_data %>% select(WTI_return))
R2_ols
#It's just R2 not Adjusted R-Squared...so it comes out to 0.2903657

#RMSE:Place larger weight on errors that are larger
RMSE_ols <- RMSE(final_data %>% select(preds_ols), final_data %>% select(WTI_return))
RMSE_ols
#RMSE has the same unit as the dependent variable (DV). 
#It means that there is no absolute good or bad threshold, however you can define it based on your DV. 
#For a data which ranges from 0 to 1000, an RMSE of 0.7 is small, but if the range goes from 0 to 1, it is not that small anymore
#Our RMSE is kinda high tbh

#K-Fold Cross Validation
RMSE_kfold <- RMSE(final_data %>% select(preds_10FoldCV),final_data %>% select(WTI_return))
RMSE_kfold

R2_kfold <- R2(final_data %>% select(preds_10FoldCV),final_data %>% select(WTI_return))
R2_kfold

#True-Plot to see if it's heteroskedastic
#kfold
ggplot(final_data, aes(x = preds_ols, y = resids_ols)) + geom_point() + scale_x_continuous(limits = c(-24, 20)) + 
  labs(title = "Heteroscedastic?", x = "Predictions OLS", y = "Residuals") + geom_abline(color = "blue")

#Kfold
ggplot(final_data, aes(x = preds_10FoldCV, y = resids_kfold)) + geom_point() + scale_x_continuous(limits = c(-24, 20)) + 
  labs(title = "Heteroscedastic?: Kfold (Nfolds = 10)", x = "Predictions OLS", y = "Residuals") + geom_abline(color = "blue")


summary(final_data$WTI_return)

#Breusch-Pagan Test For Formal Analysis
#Sig. Threshold: p<.05
#Ho (Null): Our model is Homoskedastic
#Ha (Alternative): Our model is Heteroskedastic
bptest(lm_5)
#p-value = 0.62...fail to reject null hypothesis...model appears homoskedastic...we've eliminated a lot of the noise through our
#variable selection methods...stil heteroskedastic


#for kfold:
bptest(mod)



#********************ARIMA Model********************
### Gauss Markov Assumptions

  
#- Linearity: the parameters we are estimating using the OLS method must be themselves linear.
#- Random: our data must have been randomly sampled from the population.
#- Non-Collinearity: the regressors being calculated aren’t perfectly correlated with each other.
#- Exogeneity: the regressors aren’t correlated with the error term.
# Homoscedasticity: no matter what the values of our regressors might be, the error of the variance is constant


fin.data.ts <- ts(final_data$WTI_return, start = c(2000,1,1), end = c(2019,8,1), frequency = 12)
print(fin.data.ts)





#Agumented Dickey Fuller Test: Lag order = 12 (monthly)
frequency(fin.data.ts) #Gives number of periods
adf.test(fin.data.ts, k = 12)

#P-value of .01...we can reject the null hypothesis (our data is stationary)


# Plot ACF's 
par(mfrow = c(1,2))
#Final Data
acf(as.numeric(fin.data.ts), main="ACF; WTI Return")


#Plot PACF's
#Final Data
pacf(as.numeric(fin.data.ts), main="PACF; WTI Return")




# Auto Arima to select best parameters for model
#trace: If TRUE, the list of ARIMA models considered will be reported.

#Optimal For Best Stationary Data
arima.best <- auto.arima(fin.data.ts, trace = TRUE)

#Best model: ARIMA(0,0,1)(0,1,1)[12]

### Step 8: Generate and Plot Forecast...for the next 12 months
arima.forecast <- forecast(arima.best, h = 24)
autoplot(arima.forecast)

#Plot ACF and PACF one last time
acf(arima.forecast$residuals,main="ACF; Arima Best-WTI Return")
pacf(arima.forecast$residuals,main="PACF; Arima Best-WTI Return")
























