# this file extracts all data from multiple file and database sources and generates baseline data for experiment
source('config.r')
# time start
ptm <- proc.time()

# vector of all batch folders in process
batch_folders <- list.files(paste0(batch_dir, '/data'), 
                            full.names = TRUE, ignore.case = TRUE, pattern = 'batch_[A-z0-9]+_[0-9]+')

# vector of all model files in process
system.time(
  cad_models <- lapply(batch_folders, read_cad_models) %>% unlist %>% unique
)

# vector of all model file sizes
system.time(
  cad_models_size <- file.size(cad_models)
)

names(cad_models_size) <- cad_models

# data frame of file information of all batch folders
system.time(
  df_file_info_batch_folders <- file.info(batch_folders)
)

# vector of all stock machining reports
system.time(
  report_files <- sapply(batch_folders, function(x) list.files(x, full.names = TRUE, ignore.case = TRUE,
                                                               pattern = 'CR_CLOC_StockMach2018R001')) %>%
    unlist
)

# list of data frames of all stock machining reports
system.time(
  report_files_list <- lapply(report_files, read_report_file)
)
names(report_files_list) <- report_files

# data frame of file information of all stock machining reports
system.time(
  df_file_info_reports <- file.info(report_files)
)

# vector of all time elapsed file paths in batch folder creation
system.time(
  time_elapsed_files <- sapply(batch_folders, function(x) list.files(x, full.names = TRUE, ignore.case = TRUE,
                                                                     pattern = 'time_elapsed')) %>%
    unlist
)

# data frame of file information of all time_elapsed.csv files
system.time(
  df_file_info_time_elapsed <- file.info(time_elapsed_files)
)

###############################################################

# vector of all time elapsed values in batch folder creation
time_elapsed <- sapply(time_elapsed_files, read_time_elapsed)

# baseline time study report for measuring user touch time
time_study_baseline <- read_excel(pth_time_study_baseline, sheet = 2)
# list of batch processing times from transactional database
scenarios <- get_string(rownames(df_file_info_batch_folders)[df_file_info_batch_folders$isdir == TRUE])
batch_processing_times <- getProcessingTimes(scenarios)

# vector of file paths to cad_models.csv
cad_models_files <- sapply(batch_folders, function(x) list.files(x, pattern = 'cad_models.csv', full.names = TRUE))
# some batches are missing cad_models.csv; remove those
cad_models_files <- cad_models_files[sapply(cad_models_files, length) > 0]
names(cad_models_files) <- str_extract(names(cad_models_files), pattern_batch_folder)
models_by_batch <- lapply(
  cad_models_files,
  function(x) read.csv(x) %>% .[,1] %>% as.character
)
# correction: report_files_list
report_files_list[[24]] <- read_excel(names(report_files_list)[24])

# some batches are missing cad_models.csv
# for these batches read the user input spreadsheet to get part numbers
# get the file size of these part numbers
these_batches <- c('batch_2p8_25_2019','batch_AnZ_25_2019_03_20','batch_CUy_25_2019_03_21','batch_GZr_400_2019_03_21',
                   'batch_mU8_25_2019_03_21','batch_QAX_100_2019_03_21','batch_trT_25_2019_03_21','batch_zGb_25_2019_03_20')
pth_all_cat_files <- '//nw/data/affcloc/bca_only/development/repo_data/AllCatFiles.rda'
load(pth_all_cat_files)
MyDat.Cat$part_number <- CleanPN(get_string.pn(MyDat.Cat$filepath))
MyDat.Cat <- MyDat.Cat[!duplicated(MyDat.Cat$part_number),]
# part numbers in each of these_batches
pth_user_input <- sapply(
  these_batches, 
  function(x) rownames(df_file_info_batch_folders)[grepl(pattern = x, rownames(df_file_info_batch_folders))]
) %>% list.files(., pattern = 'userinputs.xlsx', full.names = TRUE)
models_by_user_inputs <- lapply(
  pth_user_input,
  function(x) read_excel(x) %>% .[,1] %>% as.character %>% 
    gsub(pattern = '.catpart', replacement = '', ignore.case = TRUE, .) %>% CleanPN
)
size_by_user_inputs <- lapply(
  models_by_user_inputs,
  function(x) file.size(MyDat.Cat$filepath[MyDat.Cat$part_number %in% x])
)
size_by_batch2 <- size_by_user_inputs
names(size_by_batch2) <- these_batches

# save all data
save(
  batch_folders,
  cad_models,
  cad_models_size,
  df_file_info_batch_folders,
  report_files,
  report_files_list,
  df_file_info_reports,
  time_elapsed_files,
  df_file_info_time_elapsed,
  time_elapsed,
  time_study_baseline,
  scenarios,
  batch_processing_times,
  cad_models_files,
  models_by_batch,
  size_by_batch2,
  file = 'data/baseline_data.rda'
)

# time elapsed
time_elapsed <- proc.time() - ptm %>% data.matrix %>% t
write.csv(time_elapsed, 'data/log/log_1generate_baseline_data.csv')