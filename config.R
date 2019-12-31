# This file specifies the necessary configurations for this project including: 
# - paths to original data 
# - sourcing data tools
# - loading libraries
rm(list = ls())
library(magrittr)
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(tools)
source('code/utils.r')

# file_paths
batch_dir <- '//nw/data/affCLOC/BCA_Only/Development/_Requests/20190315_RunBacklogPartsApriori'
pth_time_study_baseline <- 'data/time_study_feature_extraction_baseline.xlsx'
batch_dir_improve <- '//nw/data/affCLOC/BCA_Only/Development/_Requests/20190507_RunBacklogPartsApriori'
pth_time_study_improve <- 'data/time_study_feature_extraction_improve.xlsx'
# regex patterns
pattern_stock_mach <- 'CR_CLOC_StockMach2018R001'
pattern_batch_folder <- 'batch_[A-z0-9]+_[0-9]+'
pattern_time_elapsed <- 'time_elapsed'
pattern_rollup <- 'batch_[sheet|stock_mach]+_[A-z0-9]+_[0-9]+'
