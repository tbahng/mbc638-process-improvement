# This file extracts the improve data from sources
# Data collected includes:
# Daily Throughput
# Daily Touch-time
# Date
# Number of Models processed per day
# Number of Batches generated per day
# Number of Batches processed per day

source('config.r')

# Copy time study to project directory
pth <- paste0(batch_dir_improve,'/data/time_study_feature_extraction_20190508.xlsx')
if (file.exists(pth)) {
  file.copy(from = pth, to = pth_time_study_improve, overwrite = TRUE)
}
time_study_improve <- read_excel(pth_time_study_improve) %>% as.data.frame
# all scenarios in new process
rollups <- list.files(paste0(batch_dir_improve,'/data'), 
                             pattern = pattern_rollup)
sql_scenarios <- paste(readLines('code/scenarios.sql'), collapse = '\n')
system.time(
  scenarios <- lapply(
    rollups,
    function(x) GetDat.CostInsight(
      gsub(pattern = 'paste_scenario_name', replacement = x, sql_scenarios)
    )[,1] %>% as.character
  )
)
scenarios <- do.call('c',scenarios)

# Query throughput by scenario
sql_throughput <- paste(readLines('code/throughput_by_scenario.sql'),
                        collapse = '\n')
system.time(
  throughput_by_scenario <- GetDat.CostInsight(
    gsub(pattern = 'paste_scenario_names', replacement = toWhereIn(scenarios), sql_throughput)
  )
)

save(
  time_study_improve,
  scenarios,
  throughput_by_scenario,
  file = 'data/improve_data.rda'
)