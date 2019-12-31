# This file explores the baseline data
library(gridExtra)
library(moments)
source('config.r')
load('data/baseline_data.rda')
load('data/baseline_data_clean.rda')
source('code/utils.r')
df <- baseline_clean

#################################################################################
# Average throughput efficiency
mean(df$throughput_efficiency)
# Standard deviation of throughput efficiency
sd(df$throughput_efficiency)
# Target throughput efficiency
mean(df$throughput_efficiency) * 1.1

# Get the margin of error of throughput efficiency
get_margin_error_c(alpha = 0.05, s = sd(df$throughput_efficiency), 
                   n = length(df$throughput_efficiency))
# Get the ideal sample size to decrease margin of error by 10%
get_sample_size_c(alpha = 0.05, sigma = sd(df$throughput_efficiency), 
                  e = .9*get_margin_error_c(alpha = 0.05, s = sd(df$throughput_efficiency), 
                                            n = length(df$throughput_efficiency)))

# Distribution of throughput efficiency
g <- ggplot(df, aes(x = factor(0))) +
  xlab('') +
  theme_bw()
a <- g + geom_boxplot(aes(y = throughput_efficiency)) +
  ggtitle('Throughput Efficiency')
# Distribution of throughput
b <- g + geom_boxplot(aes(y = throughput)) +
  ggtitle('Throughput')
# Distribution of throughput rate
c <- g + geom_boxplot(aes(y = throughput_rate)) +
  ggtitle('Throughput Rate')
# Distribution of touch-time
d <- g + geom_boxplot(aes(y = touch_time)) +
  ggtitle('Touch-Time')

grid.arrange(a,b,d, nrow = 3)

summary(df$throughput_efficiency) %>% as.matrix
summary(df$throughput) %>% as.matrix
summary(df$touch_time) %>% as.matrix

# touch-time over time
ggplot(df, aes(x = date, y = touch_time)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle('Touch-time Over Time') +
  theme(plot.title = element_text(size = 22))
# throughput over time
ggplot(df, aes(x = date, y = throughput)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Throughput Over Time') +
  theme(plot.title = element_text(size = 22))
# throughput rate over time
ggplot(df, aes(x = date, y = throughput_rate)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle('Throughput Rate Over Time')
ggplot(df, aes(x = date, y = throughput_efficiency)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle('Throughput Efficiency Over Time') +
  theme(plot.title = element_text(size = 22))
#################################################################################
# Estimate of total disk space required during period of performance
sum(baseline_clean$total_size) / 1000000000
#################################################################################
# System processing time by Number of Models
ggplot(df, aes(x = total_models, y = total_sys_time_processing)) +
  geom_point(aes(col = avg_size_model, size = avg_size_batch)) +
  geom_smooth(method = 'lm', formula = y~x) +
  theme_bw() +
  scale_color_gradientn(colors = c('blue','orange')) +
  ggtitle('Sys. Process Time by # of Models') +
  theme(plot.title = element_text(size = 22))

# Does the size and volume of the models affect yield and touch-time?
# Scatterplot of Throughput by Total Size (bytes)
ggplot(df, aes(x = total_size, y = throughput_rate)) +
  geom_point(aes(col = avg_size_model, size = total_models)) +
  geom_smooth(method = 'lm', formula = y~x) +
  theme_bw() +
  scale_color_gradientn(colors = c('blue','orange')) +
  ggtitle('Throughput Rate by Total Size (bytes)') + 
  theme(plot.title = element_text(size = 22))

# Scatterplot of Touch-Time by Total Size (bytes)
ggplot(df, aes(x = total_size, y = touch_time)) +
  geom_point(aes(col = avg_size_model, size = total_models)) +
  geom_smooth(method = 'lm', formula = y~x) +
  theme_bw() +
  scale_color_gradientn(colors = c('blue','orange')) +
  ggtitle('Touch-Time by Total Size (bytes)') +
  theme(plot.title = element_text(size = 22))

# Does the size and volume of the models affect batch creation performance?
# Scatterplot of number of models and time elapsed
ggplot(df, aes(x = total_models, y = total_time_batching)) +
  geom_point(aes(size = total_size, col = avg_size_model)) +
  geom_smooth(method = 'lm', formula = y~x) +
  scale_color_gradientn(colors = c('blue','orange')) +
  ggtitle('Batch Creation Times by # of Models') +
  xlab("Total Models") +
  ylab("Time Elapsed for Batch Creation") +
  theme_bw() +
  theme(plot.title = element_text(size = 22))

# plot relationship between day of week and touch time
plot(as.factor(df$day_of_week), df$touch_time)
f_lvls <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
df$day_of_week <- factor(df$day_of_week, levels = f_lvls)
ggplot(df, aes(x = day_of_week)) +
  geom_boxplot(aes(y = touch_time))
# plot line graph of throughput_efficiency by date
df_scaled <- as.data.frame(scale(df[,3:17])) %>% cbind(df[,1:2],.)
ggplot(df_scaled, aes(x = date, y = throughput)) +
  geom_line(aes(col = throughput_efficiency), size = 2) +
  #geom_line(aes(x = date, y = touch_time), color = 'red', size = 1) +
  ggtitle('Throughput Efficiency Across Time')
#################################################################################
# normality plot of throughput
qqnorm(df$throughput); qqline(df$throughput)
# histogram of throughput
hist(df$throughput)
# normality plot of throughput efficiency
qqnorm(df$throughput_efficiency); qqline(df$throughput_efficiency)
# histogram of throughput efficiency
hist(df$throughput_efficiency)
#################################################################################
# compute SQL (sigma quality level)
## d = defect opportunities per unit
## u = units produced per period
## a = total actual defects
d = 2
u = nrow(df)
a = sum(df$throughput < 5000) + sum(df$total_batches > df$total_reports)
dpmo = get_dpmo(d,u,a)
get_sql(d,u,a)
#################################################################################
# plot histograms of time study baseline
colnames(time_study_baseline) <- c('id','size','First Touch Time','Second Touch Time','Third Touch Time','Fourth Touch Time')
df <- melt(time_study_baseline[, 2:6], id = 'size')
df$size <- as.factor(df$size)

plots <- lapply(
  levels(df$variable),
  function(x) {
    y = df[df$variable == x,'value'] %>% .[!is.na(.)]
    breaks = pretty(range(y), n = nclass.FD(y), min.n = 1)
    binwidth = breaks[2] - breaks[1]
    ggplot(df[df$variable == x, ], aes(x = value)) +
    geom_histogram(binwidth = binwidth, color = 'white', fill = 'blue') +
    facet_wrap(~size) +
    theme_bw() +
    ggtitle(paste("Histograms of", x, "by Batch Size"))
    }
)
plots
#################################################################################
# compute mean touch times given batch size
time_study_mean <- sapply(
  unique(time_study_baseline$size), 
  function(x) sapply(time_study_baseline[time_study_baseline$size == x,3:6], mean, na.rm = TRUE)
)
colnames(time_study_mean) <- unique(time_study_baseline$size)
time_study_mean
# compute standard deviation of touch times given batch size
time_study_sd <- sapply(
  unique(time_study_baseline$size), 
  function(x) sapply(time_study_baseline[time_study_baseline$size == x,3:6], sd, na.rm = TRUE)
  )
colnames(time_study_sd) <- unique(time_study_baseline$size)
time_study_sd
# compute margin of error of touch times given batch size
time_study_me <- sapply(
  unique(time_study_baseline$size), 
  function(x) sapply(time_study_baseline[time_study_baseline$size == x,3:6], 
                     function(x) get_margin_error_c(alpha = 0.05, s = sd(x, na.rm = T), n = length(x[!is.na(x)])))
)
colnames(time_study_me) <- unique(time_study_baseline$size)
time_study_me
#################################################################################


#################################################################################
# Simple Linear Regression Model between throughput efficiency and num_models

x_vars <- c('throughput','touch_time','total_batches','total_models','avg_models_batch','total_time_batching',
            'avg_time_batching','total_size','total_reports','total_sys_time_processing','avg_sys_time_processing','throughput_rate',
            'avg_size_model','avg_size_batch')
mod_list <- lapply(x_vars, function(x) lm(baseline_clean$throughput_efficiency ~ baseline_clean[,x]))
names(mod_list) <- x_vars

# r.squared value for each model
(r_squared <- sapply(mod_list, function(x) summary(x)$r.squared))
# correlation coefficient for each model
(cc <- sapply(mod_list, function(x) sqrt(summary(x)$r.squared)))
# p-values for each model
(p_vals <- sapply(mod_list, function(x) summary(x)$coefficients[8]))

## Plot the data!
df <- baseline_clean
plots <- lapply(
  x_vars,
  function(k) {
    ggplot(df, aes_string(x = k, y = 'throughput_efficiency')) +
      geom_point(color = 'blue') + 
      stat_smooth(method = 'lm', col = 'red') +
      theme_bw() + 
      ggtitle(toTitleCase(paste("Relationship between", k, "and throughput_efficiency")))
  }
)
names(plots) <- x_vars

# which variables displayed the highest r-squared, correlation coefficient, p-value
names(r_squared)[which.max(r_squared)]
names(cc)[which.max(cc)]
names(p_vals)[which.min(p_vals)]

# variable: avg_time_batching
## Practicality
# The time it takes to create a batch by the batching software can impact both throughput and touch-time
## Plot
plots$avg_time_batching
# From the plot we can see a clustering of data, which may indicate inadequecies of a linear model
## Statistical Analysis
# percent of variability in throughput_efficiency explained by throughput
r_squared['avg_time_batching'] # Only 20% of the variability in throughput_efficiency is explained by avg_time_batching
# is there a meaningful relationship between throughput_efficiency and avg_time_batching?
cc['avg_time_batching']
cc['avg_time_batching'] <= -0.7 | cc['avg_time_batching'] >= 0.7 # there doesn't appear to be a meaningful relationship
# is there a significant relationship?
p_vals['avg_time_batching']
p_vals['avg_time_batching'] <= 0.05 # there does appear to be a significant relationship
# normality of residuals and randomness of residuals
plot(mod_list$avg_time_batching) # residuals would be normally distributed around a mean of zero if not for outliers; nor do they display randomness

# variable: total_batches
## Practicality
# More batches translates to more touch_time but may not be practical. The effect it has on touch_time can be greatly confounded by the volume of throughput generated by each batch
## Plot
plots$total_batches
# From the plot we can see the opposite of what we might expect from the direct effect it has on touch_time. This seems to support the previous assumption that the amount of throughput generated by additional batches greatly diminishes this variables effect.
## Statistical Analysis
# percent of variability in throughput_efficiency explained by total_batches
r_squared['total_batches'] # Only 11% of the variability in throughput_efficiency is explained by total_batches
# is there a meaningful relationship between throughput_efficiency and avg_time_batching?
cc['total_batches']
cc['total_batches'] <= -0.7 | cc['total_batches'] >= 0.7 # there doesn't appear to be a meaningful relationship
# is there a significant relationship?
p_vals['total_batches']
p_vals['total_batches'] <= 0.05 # there does appear to be a significant relationship
# normality of residuals and randomness of residuals
plot(mod_list$total_batches) # residuals are not normally distributed around a mean of zero; nor do they display randomness


# HOW ABOUT THROUGHPUT INSTEAD OF THROUGHPUT_EFFICIENCY?
# HOW ABOUT TOUCH-TIME?

#################################################################################
# Chi-Squared Test for Independence
# option: day_of_week, defect (either Throughput less than 5000 or There are unprocessed batches left at the end of day or Both)
# There isn't enough data to perform the Chi-Squared test for independence 
# on the baseline data. In order to have enough data, we must have over 70 
# observations and even then it might not be enough.

# option: batch size and system processing time
batch_sizes <- sapply(
  as.character(batch_processing_times$scenario_name),
  function(x) strsplit(x, split = '_') %>%
    unlist %>% .[3], USE.NAMES = FALSE
)
batch_sizes <- ifelse(as.numeric(batch_sizes) <= 400, "0-400",
                      ifelse(as.numeric(batch_sizes) > 400, "400+",
                             "unknown"))
processing_time_min <- difftime(batch_processing_times$end_timestamp,
                                batch_processing_times$start_timestamp,
                                units = 'min')
processing_time_min <- ifelse(processing_time_min <= 60, "0-60",
                              ifelse(processing_time_min > 60, "60+",
                                     "unknown"))
m <- as.data.frame.matrix(
  table(
    data.frame(batch_sizes, processing_time_min)
  )
)
# perform chi-squared test
chisq.test(x = m)
# observed two-way table
chisq.test(x = m)[['observed']]
# expected two-way table
chisq.test(x = m)[['expected']]

#################################################################################

# Confidence Intervals

# Estimating the average amount of throughput per day with 95% confidence
get_ci_c(x = mean(df$throughput), alpha = 0.05, s = sd(df$throughput), n = dim(df)[1])
get_margin_error_c(alpha = 0.05, s = sd(df$throughput), n = dim(df)[1])

# Estimating the average amount of touch-time per day with 95% confidence
get_ci_c(x = mean(df$touch_time), alpha = 0.05, s = sd(df$touch_time), n = dim(df)[1])
get_margin_error_c(alpha = 0.05, s = sd(df$touch_time), n = dim(df)[1])

# Estimating the average amount of system-time process per model with 95% confidence
get_ci_c(x = mean(df$total_sys_time_processing/df$total_models),
         alpha = 0.05,
         s = sd(df$total_sys_time_processing/df$total_models),
         n = sum(df$total_models))
get_margin_error_c(alpha = 0.05,
                   sd(df$total_sys_time_processing/df$total_models),
                   n = sum(df$total_models))

# Estimating the average size of CAD models in bytes with 95% confidence
get_ci_c(x = mean(cad_models_size),
         alpha = 0.05,
         s = sd(cad_models_size),
         n = length(cad_models_size))
get_margin_error_c(alpha = 0.05,
                   sd(cad_models_size),
                   n = length(cad_models_size))
