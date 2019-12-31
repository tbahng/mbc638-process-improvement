# This file transforms the improve data for the purposes of:
## Measuring sigma quality level (SQL)
## Measuring confidence interval
## Hypothesis test to determine if process improved
## Process Control Chart XMR/IMR
source('config.r')

# load the improve data
load('data/improve_data.rda')
# assign to new data frame
df <- throughput_by_scenario
# convert factors to character
i <- sapply(df, is.factor)
df[i] <- lapply(df[i], as.character)
# add rollup (batch id) to df
df$rollup <- sapply(df$scenario, get_batch_id)
# batch create data
df.batch <- data.frame(date_processed = as.Date(sapply(df$scenario, get_batch_date)),
                       rollup = sapply(df$scenario, get_batch_id),
                       num_models = sapply(df$scenario, get_num_models2),
                       scenario = df$scenario,
                       stringsAsFactors = FALSE) %>%
  unique

# subset data to within range 2019-05-10 to 2019-05-30
keep.dates <- paste0('2019-05-', 10:30) %>% as.Date
df <- df[df$last_saved %in% keep.dates,]
df.batch <- df.batch[df.batch$date_processed %in% keep.dates,]
# measure: throughput per day
daily_throughput <- tapply(df$num_parts, df$last_saved, sum)
# measure: number of scenarios created per day
batches_created <- tapply(df.batch$scenario, df.batch$date_processed, 
                          function(x) length(unique(x)))
# measure: number of scenarios processed per day
batch_processed <- tapply(df$scenario, df$last_saved,
                          function(x) length(unique(x)))
# measure: number of rollups processed per day
rollup_processed <- tapply(df$rollup, df$last_saved,
                           function(x) length(unique(x)))
# time study results: avg touch-time per rollup
time_study_improve$num_models <- sapply(time_study_improve$Comments,
                                        get_num_models2)
avg_touchtime_1 <- tapply(time_study_improve[,2],
                          time_study_improve$num_models, mean, na.rm = TRUE) %>%
  .[names(.) == 2600]
avg_touchtime_2 <- tapply(time_study_improve[,3],
                          time_study_improve$num_models, mean, na.rm = TRUE) %>%
  .[names(.) == 2600]
avg_touchtime_per_rollup <- avg_touchtime_1 + avg_touchtime_2
# measure: average touch time per day
daily_touchtime <- avg_touchtime_per_rollup * rollup_processed
# measure: throughput efficiency per day
throughput_efficiency <- daily_throughput / daily_touchtime

# improve clean data
improve_clean <- data.frame(
  date = keep.dates,
  throughput = daily_throughput,
  touch_time = daily_touchtime,
  throughput_efficiency = throughput_efficiency,
  batch_processed = batch_processed
)
improve_clean$batch_created <- sapply(
  as.character(keep.dates),
  function(x) ifelse(x %in% names(batches_created), 
                     batches_created[names(batches_created) == x], 0)
)
# load baseline data
load('data/baseline_data_clean.rda')

# save data for final analysis
save(
  baseline_clean,
  improve_clean,
  file = 'data/improve_data_clean.rda'
)
