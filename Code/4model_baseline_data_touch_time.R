# This file performs linear regression with throughput_efficiency as response.
source('config.r')
load('data/baseline_data.rda')
load('data/baseline_data_clean.rda')
source('code/utils.r')
df <- baseline_clean

#################################################################################
# Correlation Matrix
# cor.matrix <- cor(df[complete.cases(df), 3:17])
# write.csv(cor.matrix, 'data/cor_matrix.csv')
# because throughput_efficiency is a function of throughput and touch_time,
# remove these variables from df
keep.vars <- !names(df) %in% c('throughput_efficiency','throughput_rate')
df <- df[, keep.vars]
# Multiple Regression Model
mod1 <- lm(touch_time ~ ., data = df)
summary(mod1)
# p-value of regression: 0.009948
# adjusted R^2: 0.9998
# significant predictors: several
# this is a useful and informative model despite its complexity
summary(mod1)$coefficients[,4] %>% sort
summary(mod1)$coefficients[,1] %>% abs %>% sort(., decreasing = TRUE)
# variables of interest: total_reports

mod2 <- lm(touch_time ~., data = df[, !names(df) %in% 'total_reports'])
summary(mod2)

save(
  mod1,
  file = 'data/model_touch_time.rda'
)
