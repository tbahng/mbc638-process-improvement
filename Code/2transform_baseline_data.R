# This file transforms baseline_data into R data suitable for further exploration and modeling
# This will be a dataframe with the following dimensions:
# date | day_of_week | throughput | touch_time | total_batches | total_models | avg_models_batch 
# total_time_batching | avg_time_batching | avg_size_batch | avg_size_model | total_size | total_reports
# total_sys_time_processing | avg_sys_time_processing | throughput_rate | throughput_efficiency
source('config.r')
# time start
ptm <- proc.time()
load('data/baseline_data.rda')


## variable: date
df_file_info_reports$date <- as.Date(df_file_info_reports$ctime)
# subset to period of performance / measurement
df_file_info_reports <- df_file_info_reports %>% .[.$date >= "2019-03-21" & .$date <= "2019-04-12", ]
## variable: day_of_week
df_file_info_reports$day_of_week <- weekdays(df_file_info_reports$date)

## Variable: throughput
# Count of models with feature extraction on observed day
throughput_by_file <- sapply(report_files_list[1:length(report_files_list)], get_throughtput)
# correction: throughput value
throughput_by_file[str_extract(names(throughput_by_file), pattern_batch_folder) == "batch_6et_400_2019_03_22"] <- 367
df_file_info_reports$throughput <- sapply(
  rownames(df_file_info_reports),
  function(x) throughput_by_file[names(throughput_by_file) == x]
) %>% unlist
# add batch_id column
df_file_info_reports$batch_id <- str_extract(rownames(df_file_info_reports), pattern_batch_folder)

## variable: touch_time
# Total time where the user was actively involved in the process on observed day
# Average touch-time per size level
time_study_baseline <- as.data.frame(time_study_baseline)
touch_time_list <- lapply(
  unique(time_study_baseline$size),
  function(x) data.frame(
    v1 = mean(time_study_baseline[time_study_baseline$size == x, 3], na.rm = TRUE),
    v2 = mean(time_study_baseline[time_study_baseline$size == x, 4], na.rm = TRUE),
    v3 = mean(time_study_baseline[time_study_baseline$size == x, 5], na.rm = TRUE),
    v4 = mean(time_study_baseline[time_study_baseline$size == x, 6], na.rm = TRUE)
  )
)
names(touch_time_list) <- unique(time_study_baseline$size)
# add rowSum to each size
touch_time_list <- lapply(touch_time_list, function(x) rowSums(x))
# add touch time for size of 5 models
touch_time_list['5'] <- 120
# add variable num_models to df_file_info_reports
df_file_info_reports$num_models <- sapply(
  df_file_info_reports$batch_id,
  function(x) strsplit(x, split = '_') %>% unlist %>% .[3] %>% as.numeric
)
df_file_info_reports$touch_time <- sapply(
  df_file_info_reports$num_models,
  function(x) touch_time_list[[as.character(x)]]
) %>% unlist

## variable: total_time_batching
# Total Time Elapsed for Batch Creation
# For batch folders missing time_elapsed.csv average time elapsed given 
names(time_elapsed) <- str_extract(names(time_elapsed), pattern_batch_folder)
# average time elapsed by number of models in batch
num_models <- sapply(names(time_elapsed), get_num_models) %>% unname %>% unique %>% sort
time_elapsed_list <- lapply(num_models, 
                            function(x) time_elapsed[sapply(names(time_elapsed), 
                                                            function(x) get_num_models(x)) == x])
names(time_elapsed_list) <- num_models
time_elapsed_list <- sapply(time_elapsed_list, mean)
batching_times <- sapply(
  df_file_info_reports$batch_id,
  function(x) if (x %in% names(time_elapsed)) {
    time_elapsed[names(time_elapsed) == x]
  } else time_elapsed_list[[as.character(df_file_info_reports$num_models[df_file_info_reports$batch_id == x])]]
)
df_file_info_reports$total_time_batching <- batching_times

## variable: size per batch

# rename cad_models_size
names(cad_models_size) <- CleanPN(get_string.pn(names(cad_models_size)))
# remove duplicated cad_model_size
cad_models_size <- cad_models_size[!duplicated(names(cad_models_size))]
# size by batch
size_by_batch <- lapply(
  models_by_batch, 
  function(x) cad_models_size[names(cad_models_size) %in% x]
)
# combine with size_by_batch2: missing batches with sizes
size_by_batch <- c(size_by_batch2, size_by_batch)
# variable: total_size
df_file_info_reports$total_size <- sapply(
  df_file_info_reports$batch_id,
  function(x) sum(size_by_batch[[x]], na.rm = TRUE)
)

# variable: total_sys_time_processing
# amount of time taken by software tool to process a batch
batch_processing_times$time_elapsed <- batch_processing_times$end_timestamp - batch_processing_times$start_timestamp
# average system processing time by number of models
batch_processing_times$num_models <- sapply(as.character(batch_processing_times$scenario_name), get_num_models)
batch_processing_times$time_elapsed <- as.numeric(batch_processing_times$time_elapsed)
processing_times_by_size <- sapply(
  sort(unique(batch_processing_times$num_models)),
  function(x) mean(batch_processing_times$time_elapsed[batch_processing_times$num_models == x], na.rm = TRUE)
)
names(processing_times_by_size) <- sort(unique(batch_processing_times$num_models))
df_file_info_reports$total_sys_time_processing <- sapply(
  df_file_info_reports$batch_id,
  function(x) batch_processing_times$time_elapsed[batch_processing_times$scenario_name == x] %>% as.numeric
) %>% as.numeric
missing_sys_time_processing <- is.na(df_file_info_reports$total_sys_time_processing) %>% which
df_file_info_reports$total_sys_time_processing[missing_sys_time_processing] <- sapply(
  missing_sys_time_processing,
  function(x) processing_times_by_size[names(processing_times_by_size) == as.character(df_file_info_reports$num_models[x])]
)

# create clean baseline data
# date | day_of_week | throughput | touch_time | total_batches | total_models | avg_models_batch 
baseline_clean <- data.frame(
  date = sort(unique(df_file_info_reports$date))
)
baseline_clean$day_of_week <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$day_of_week[df_file_info_reports$date == x] %>% unique
)
baseline_clean$throughput <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$throughput[df_file_info_reports$date == x] %>% sum
)
baseline_clean$touch_time <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$touch_time[df_file_info_reports$date == x] %>% sum
)
baseline_clean$total_batches <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_batch_folders[as.Date(df_file_info_batch_folders$ctime) == x,] %>% nrow(.)
)
baseline_clean$total_models <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$num_models[df_file_info_reports$date == x] %>% sum
)
baseline_clean$avg_models_batch <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$num_models[df_file_info_reports$date == x] %>% mean
)
# total_time_batching | avg_time_batching | total_size | total_reports

baseline_clean$total_time_batching <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$total_time_batching[df_file_info_reports$date == x] %>% sum
)
baseline_clean$avg_time_batching <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$total_time_batching[df_file_info_reports$date == x] %>% mean
)
baseline_clean$total_size <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$total_size[df_file_info_reports$date == x] %>% sum
)
baseline_clean$total_reports <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports[df_file_info_reports$date == x,] %>% nrow(.)
)
# total_sys_time_processing | avg_sys_time_processing | throughput_rate | throughput_efficiency
baseline_clean$total_sys_time_processing <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$total_sys_time_processing[df_file_info_reports$date == x] %>% sum
)
baseline_clean$avg_sys_time_processing <- sapply(
  baseline_clean$date, 
  function(x) df_file_info_reports$total_sys_time_processing[df_file_info_reports$date == x] %>% mean
)
baseline_clean$throughput_rate <- baseline_clean$throughput / baseline_clean$total_models
baseline_clean$throughput_efficiency <- baseline_clean$throughput / baseline_clean$touch_time
# avg_size_model | avg_size_batch
baseline_clean$avg_size_model <- baseline_clean$total_size / baseline_clean$total_models
baseline_clean$avg_size_batch <- ifelse(baseline_clean$total_batches == 0, NA,
                                        baseline_clean$total_size / baseline_clean$total_batches)
# save all data
save(
  baseline_clean,
  df_file_info_reports,
  time_elapsed,
  time_study_baseline,
  file = 'data/baseline_data_clean.rda'
)

# time elapsed
time_elapsed <- proc.time() - ptm %>% data.matrix %>% t
write.csv(time_elapsed, 'data/log/log_2transform_baseline_data.csv')