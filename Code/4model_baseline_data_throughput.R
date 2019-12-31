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
keep.vars <- !names(df) %in% c('throughput_efficiency','touch_time','throughput_rate')
df <- df[, keep.vars]
# Multiple Regression Model
mod1 <- lm(throughput ~ ., data = df)
summary(mod1)
# p-value of regression: 3.623e-05
# adjusted R^2: 1
# significant predictors: several
# this is a useful and informative model despite its complexity
summary(mod1)$coefficients[,4] %>% sort
# variables of interest: avg_models_batch

save(
  mod1,
  file = 'data/model_throughput.rda'
)