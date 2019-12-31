# This file performs linear regression with throughput_efficiency as response.
source('config.r')
load('data/baseline_data.rda')
load('data/baseline_data_clean.rda')
source('code/utils.r')
df <- baseline_clean

#################################################################################
# Correlation Matrix
cor.matrix <- cor(df[complete.cases(df), 3:17])
write.csv(cor.matrix, 'data/cor_matrix.csv')
# because throughput_efficiency is a function of throughput and touch_time,
# remove these variables from df
keep.vars <- !names(df) %in% c('throughput','touch_time')
df <- df[, keep.vars]
# Multiple Regression Model
mod1 <- lm(throughput_efficiency ~ ., data = df)
summary(mod1)
# p-value of regression: 0.4611
# adjusted R^2: 0.5961
# significant predictors: none
# this is a poor model, consider keeping certain x variables instead or others
names(df)
drop.fields <- c('date','throughput','touch_time','throughput_rate')
df2 <- df[,!names(df) %in% drop.fields]
# re-model with new data
mod2 <- lm(throughput_efficiency ~., data = df2)
summary(mod2)
# poor model
# p-value is not significant 0.1822
# Adjusted R^2 is 0.6541
# none of the x variables are significant. 
# Consider eliminating the x variable with largest p-value from model
which.max(summary(mod2)$coefficients[,4])
# remove total_batches
drop.fields <- c(drop.fields, 'total_batches')
df3 <- df[, !names(df) %in% drop.fields]
# re-model with new data
mod3 <- lm(throughput_efficiency ~., data = df3)
summary(mod3)
# model improved
# p-value: 0.08
# Adjusted R^2: 0.74
# Significant predictors: day_of_weekSunday, total_reports
# p-value is still not significant at alpha level of 0.05
# however it has gone from 0.1822 down to 0.08
# Adjusted R^2 has gone from 0.6541 up to 0.7361
# continue backwards stepwise regression
which.max(summary(mod3)$coefficients[,4])
# day_of_weekThursday has largest p-value of 0.487
# because we cannot just remove one level of a categorical variable
# what if we just remove day_of_week altogether?
drop.fields <- c(drop.fields, 'day_of_week')
df4 <- df[, !names(df) %in% drop.fields]
# re-model with new data
mod4 <- lm(throughput_efficiency ~., data = df4)
summary(mod4)
# model became much worse by removing day_of_week
# p-value increased to 0.2074 from 0.08
# Adjusted R^2 has gone down from 0.7361 to 0.2598
# let's add back day_of_week since it did have one significant category
# day_of_week is Sunday
# let's add it back as is_sunday, a binary variable
df5 <- df4
df5$is_sunday <- df3$day_of_week == 'Sunday'
# re-model with new data
mod5 <- lm(throughput_efficiency ~., data = df5)
summary(mod5)
# model improved from before by adding is_sunday variable
# p-value decreased back to 0.08
# but Adjusted R^2 is still very low at 0.475
# is_sundayTRUE still remains our only semi-significant x variable
# we can still continue backwards stepwise regression from here
# eliminate the variable with highest p-value
which.max(summary(mod5)$coefficients[,4])
# drop total_models from the data
rm_var <- 'total_models'
df6 <- df5[,!names(df5) %in% rm_var]
# re-model with new data
mod6 <- lm(throughput_efficiency ~., data = df6)
summary(mod6)
# the model improved
# p-value of the model is significant 0.0398 < 0.05
# Adjusted R^2 increased from 0.475 to 0.5251
# we now have two significant x variables total_reports and is_sundayTRUE
# let's continue backwards stepwise regression
# eliminate the variable with highest p-value
which.max(summary(mod6)$coefficients[,4])
# drop avg_size_model from the data
rm_var <- 'avg_size_model'
df7 <- df6[,!names(df6) %in% rm_var]
# re-model with new data
mod7 <- lm(throughput_efficiency ~., data = df7)
summary(mod7)
# model is once again significant now at p-value of 0.02
# adjusted R^2 increased to 0.5632
# the biggest change is that there are several more significant x variables
# significant: avg_models_batch, total_time_batching, total_size, 
# total_reports, is_sundayTRUE
# continue backwards stepwise regression
# eliminate the variable with highest p-value
which.max(summary(mod7)$coefficients[,4])
rm_var <- 'avg_size_batch'
df8 <- df7[,!names(df7) %in% rm_var]
# re-model with new data
mod8 <- lm(throughput_efficiency ~., data = df8)
summary(mod8)
# model has improved somewhat particularly with the p-value now at 0.01
# the adjusted R^2 has decreased though down to 0.5362
# significant predictors: avg_models_batch, total_size, total_reports, total_sys_time_processing,
# is_sundayTRUE
# let's compare all the models on the basis of AIC 
# which model thus far has lowest AIC combined with simplicity?
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
# model 7 shows the most promise, but is similar to model 8 which is simpler

# let's continue backwards stepwise regression
summary(mod8)$coefficients[,4]
rm_var <- 'avg_time_batching'
df9 <- df8[,!names(df8) %in% rm_var]
mod9 <- lm(throughput_efficiency ~., data = df9)
summary(mod9)
# the p-value has gone down to 0.0086
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9)
# seems like mod7 might still a better model given that p-value is low
# Adjust R^2 is the highest yet combined with low p-value
plot(mod7)
# residuals display mostly normal distribution
qqnorm(mod7$residuals);qqline(mod7$residuals)
hist(mod7$residuals)
# residuals display mean very close to zero
mean(mod7$residuals)
# residuals appear to be independent of each other

save(mod7,
     file = 'data/model_throughput_efficiency.rda')
