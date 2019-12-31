rm(list = ls())
library(magrittr)
library(readxl)
source('code/utils.r')
source('../code/utils_mbc638.r')
load('data/baseline_data_clean.rda')
things_to_keep <- c('get_dpmo','get_sql','get_ci_c','get_margin_error_c',
                    'get_p_from_z', 'get_p_from_t', 'get_t_2s_c','get_z_2s_c',
                    'df_hypothesis_test','get_hypothesis_stmt','dpmo_to_sql',
                    'sql_table','time_study_baseline')
rm(list = setdiff(ls(), things_to_keep))
load('data/improve_data_clean.rda')
time_study_improve <- read_excel('data/time_study_feature_extraction_improve.xlsx')

###########################################
# Improve
# Time Study of End-to-End touch-time per batch
###########################################
# average end-to-end touch-time per batch
t1 <- sapply(time_study_baseline[,3:6], function(x) mean(x, na.rm = TRUE)) %>% sum
t2 <- sapply(time_study_improve[,2:3], function(x) mean(x, na.rm = TRUE)) %>% sum
# percent change
(t2 - t1) / t1
###########################################
# Percent change average daily throughput
###########################################
meanThroughput.0 <- mean(baseline_clean$throughput)
meanThroughput.1 <- mean(improve_clean$throughput)
round(
  ((meanThroughput.1 - meanThroughput.0) / meanThroughput.0)*100, 2
)
# 2.83% increase in average daily throughput

###########################################
# Percent change average daily touchtime
###########################################
meanTouch.0 <- mean(baseline_clean$touch_time)
meanTouch.1 <- mean(improve_clean$touch_time)
round(
  ((meanTouch.1 - meanTouch.0) / meanTouch.0)*100, 2
)
# 95% decrease in average daily touchtime

###########################################
# Percent change average daily throughput efficiency
###########################################
meanResponse.0 <- mean(baseline_clean$throughput_efficiency)
meanResponse.1 <- mean(improve_clean$throughput_efficiency)
round(
  ((meanResponse.1 - meanResponse.0) / meanResponse.0)*100, 2
)
# 1984.76% increase in average daily throughput efficiency

###########################################
# compute SQL (sigma quality level) - Baseline
###########################################
## d = defect opportunities per unit
## u = units produced per period
## a = total actual defects
d = 2
u = nrow(baseline_clean)
a = sum(baseline_clean$throughput < 5000) + 
  sum(baseline_clean$total_batches > baseline_clean$total_reports)
dpmo = get_dpmo(d,u,a)
get_sql(d,u,a)

###########################################
# compute SQL (sigma quality level) - Improve
###########################################
## d = defect opportunities per unit
## u = units produced per period
## a = total actual defects
d = 2
u = nrow(improve_clean)
a = sum(improve_clean$throughput < 5000) + 
  sum(improve_clean$batch_created > improve_clean$batch_processed)
dpmo = get_dpmo(d,u,a)
get_sql(d,u,a)

###########################################
# Confidence Intervals - Baseline
###########################################
# Estimating the average amount of throughput per day with 95% confidence
get_ci_c(x = mean(baseline_clean$throughput), 
         alpha = 0.05, s = sd(baseline_clean$throughput), 
         n = dim(baseline_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(baseline_clean$throughput), 
                   n = dim(baseline_clean)[1])

# Estimating the average amount of touch-time per day with 95% confidence
get_ci_c(x = mean(baseline_clean$touch_time), 
         alpha = 0.05, s = sd(baseline_clean$touch_time), 
         n = dim(baseline_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(baseline_clean$touch_time), 
                   n = dim(baseline_clean)[1])

# Estimating the average amount of throughput efficiency per day with 95% confidence
get_ci_c(x = mean(baseline_clean$throughput_efficiency), 
         alpha = 0.05, s = sd(baseline_clean$throughput_efficiency), 
         n = dim(baseline_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(baseline_clean$throughput_efficiency), 
                   n = dim(baseline_clean)[1])

###########################################
# Confidence Intervals - Improve
###########################################
# Estimating the average amount of throughput per day with 95% confidence
get_ci_c(x = mean(improve_clean$throughput), 
         alpha = 0.05, s = sd(improve_clean$throughput), 
         n = dim(improve_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(improve_clean$throughput), 
                   n = dim(improve_clean)[1])

# Estimating the average amount of touch-time per day with 95% confidence
get_ci_c(x = mean(improve_clean$touch_time), 
         alpha = 0.05, s = sd(improve_clean$touch_time), 
         n = dim(improve_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(improve_clean$touch_time), 
                   n = dim(improve_clean)[1])

# Estimating the average amount of throughput efficiency per day with 95% confidence
get_ci_c(x = mean(improve_clean$throughput_efficiency), 
         alpha = 0.05, s = sd(improve_clean$throughput_efficiency), 
         n = dim(improve_clean)[1])
get_margin_error_c(alpha = 0.05, s = sd(improve_clean$throughput_efficiency), 
                   n = dim(improve_clean)[1])

###########################################
# Hypothesis test 
# determine if process improved
# Continuous Two Sample Left-tailed test
###########################################
# sample data (two samples)
x_1 <- baseline_clean$throughput_efficiency
x_2 <- improve_clean$throughput_efficiency
# specify sample mean 1
x1 <- mean(x_1)
# specify sample mean 2
x2 <- mean(x_2)
# specify sample standard deviation 1
s1 = sd(x_1)
# specify sample standard deviation 2
s2 = sd(x_2)
# specify sample sizes
n1 = length(x_1)
n2 = length(x_2)
# Specify Hypothesis Test
data_type = 'continuous'
num_samples = 2
test_type = 'left_tailed'
alpha = 0.05
### Calculated variables
# test statistic and p-value
if (n1 + n2 >= 30) {
  test_stat = get_z_2s_c(x1 = x1, x2 = x2, s1 = s1, s2 = s2, n1 = n1, n2 = n2)
  p_val = get_p_from_z(test_type = test_type, z = test_stat)
} else if (n1 + n2 < 30) {
  test_stat = get_t_2s_c(x1 = x1, x2 = x2, s1 = s1, s2 = s2, n1 = n1, n2 = n2)
  p_val = get_p_from_t(test_type = test_type, t = test_stat, df = (n1 + n2 - 2))
}
# conduct hypothesis test
get_hypothesis_stmt(data_type, num_samples, test_type) %>% gsub(pattern = 'u0|u2|p0|p2', replacement = round(x2,2), .)
# State the rejection rule
print(paste("If p value is <=", as.character(alpha), "Ho must go."))
# check valid test type
if (test_type %in% c('two_tailed','left_tailed','right_tailed')) {
  print(paste("p-value is", as.character(round(p_val,2))))
  # test the rejection rule criteria
  test <- p_val <= alpha
  # state conclusion
  if (test) {
    print(paste("Reject null hypothesis due to sufficient evidence at level of significance alpha =", alpha))
  } else print(paste("Fail to reject null hypothesis due to insufficient evidence at level of significance alpha =", alpha))
}

###########################################
# Process Control Chart XMR/IMR
# write data to csv for charting in Excel
###########################################
write.csv(improve_clean$throughput_efficiency, 'data/improve_proc_control_data.csv')
