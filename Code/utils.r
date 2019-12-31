# This file contains relevant functions for data collection on the apriori feature extraction process

# function to read 'cad_models.csv' in a batch folder and return a vector of all underlying filepaths
# dir_pth = batch folder directory path
read_cad_models <- function(dir_pth) {
  file_pth <- list.files(dir_pth, full.names = TRUE, ignore.case = TRUE, pattern = 'cad_models.csv')
  if (length(file_pth) > 0) {
    vec_out <- fread(file_pth) %>% .[['file_path']] %>% as.character
    return(vec_out)
  }
}

# function to read time_elapsed.csv file
# pth = file path of time_elapsed.csv file
read_time_elapsed <- function(pth) {
  vec_out <- read.csv(pth) %>% .[,'elapsed']
  return(vec_out)
}

# Function to read excel report files
read_report_file <- function(filename) {
  options(java.parameters = "-Xmx16000m")
  require('xlsx')
    tryCatch({
      dat <- read.xlsx(file = filename, sheetIndex = 1)
      return(dat)
    }, error = function(e){print(paste(filename,"has an issue"))})
}

# return last element of a file path including file ext
get_string <- function(vec) {
  positions <- lapply(gregexpr(pattern ='/',vec), function(x) x[length(x)] + 1)
  substring(vec, positions)
}

# Function to convert vector to comma separated string for SQL WHERE IN() clause
toWhereIn <- function(vec) {
  return(
    paste(shQuote(vec, type = "sh"), collapse = ',')
  )
}

# Function to query data from Elabs BCA CLOC; takes SQL query as string argument; 
GetDat.elab <- function(x) {
  require(RODBC)
  source('C:/Users/ke392d/Desktop/credentials.r')
  channel.elabs_bca_cloc <- odbcConnect(dsn = credentials.elabs_bca_cloc[1],
                                        uid = credentials.elabs_bca_cloc[2],
                                        pwd = credentials.elabs_bca_cloc[3]
  )
  dat <- sqlQuery(channel.elabs_bca_cloc,
                  x,
                  rows_at_time = 1024,
                  buffsize = 10000)
  return(dat)
  odbcClose(channel.elabs_bca_cloc)
}

# Function to query data from Cost Insight; takes SQL query as string argument; 
GetDat.CostInsight <- function(x) {
  require(RODBC)
  source('C:/Users/ke392d/Desktop/credentials.r')
  MyChannel <- odbcConnect(dsn = credentials.cost_insight[1],
                           uid = credentials.cost_insight[2],
                           pwd = credentials.cost_insight[3]
  )
  dat <- sqlQuery(MyChannel,
                  x,
                  rows_at_time = 1024,
                  buffsize = 10000)
  return(dat)
  odbcClose(MyChannel)
}

# Function to query batch processing times from COST Insight
# vec = vector of part numbers
getProcessingTimes <- function(vec) {
  base_query <- paste(readLines('code/bulk_costing_time.sql'), collapse = '\n') 
  my_query <- gsub(pattern = 'PASTE_SCENARIO_NAMES_HERE', replacement = toWhereIn(vec), base_query)
  return(
    GetDat.CostInsight(my_query)
  )
}

# Function to return the number of models with throughput from a bulk-costing output report
get_throughtput <- function(df) {
  if (is.data.frame(df)) {
    length(df$finishedVolumeInches3[!is.na(df$finishedVolumeInches3)])
  }
}

# Function to return the number of models given a batch folder ID
get_num_models <- function(batch_id) {
  strsplit(batch_id, split = '_') %>% unlist %>% .[3] %>% as.numeric
}

# return last element of a file path without extension (CATPart)
get_string.pn <- function(vec) {
  library(magrittr)
  positions <- lapply(gregexpr(pattern ='/',vec), function(x) x[length(x)] + 1)
  x <- substring(vec, positions)
  gsub(pattern = '.CATPART', replacement = '', toupper(x))
}

# Clean part number string to standard convention for CLOC
CleanPN <- function(x) {
  require(magrittr)
  # Use this pattern when string contains special prefix keywords (i.e. 'SPL')
  pattern.1 <- '^\\W\\D\\D|^INF_|_V4CONV|_REF|_--\\D| --\\D|--\\D| --\\w*$|--\\w*$|\\_.*$|\\W\\d\\W$|\\D-.*$|\\..*$|[[:space:]].*|\\(.*'
  # Use this pattern when string doesn't contain special prefix keywords (i.e. 'SPL')
  pattern.2 <- '^\\D*|^\\W\\D\\D|^INF_|_V4CONV|_REF|_--\\D| --\\D|--\\D| --\\w*$|--\\w*$|\\_.*$|\\W\\d\\W$|\\D-.*$|[[:alpha:]]$|\\D\\D$|\\..*$|[[:space:]].*|\\(.*'
  # These are special prefixes on valid part numbers; add as necessary
  special.prefix <- paste('^SPL', sep = '|')
  test <- grepl(pattern = special.prefix, x)
  ifelse(test,
         gsub(pattern = pattern.1, replacement = '', x = trimws(x), ignore.case = T) %>%
           gsub(pattern = pattern.1, replacement = '', ., ignore.case = T) %>%
           toupper(),
         gsub(pattern = pattern.2, replacement = '', x = trimws(x), ignore.case = T) %>%
           gsub(pattern = pattern.2, replacement = '', ., ignore.case = T) %>%
           toupper()
  )
}

# Sigma Quality Level (SQL): Measure of performance quality in a process
# read sigma quality level table
sql_table <- read.csv('data/sql_table.csv')
# compute dpmo (defects per million opportunities) given defect opportunities per unit, units produced, and actual defects
## d = defect opportunities per unit
## u = units produced per period
## d * u = total possible defects per period
## a = total actual defects
## a / (d * u) = defect per opportunity rate
get_dpmo <- function(d, u, a) {
  # calculate total possible defects per period
  du <- d * u
  # calculate defect per opportunity rate
  dpo <- a / du
  # calculate dpmo
  dpmo_out <- dpo * 1000000
  return(dpmo_out)
}
## Example
# get_dpmo(d = 1, u = 30, a = 6)

# compute sql (sigma quality level) given a dpmo (defects per million opportunities) value
dpmo_to_sql <- function(dpmo, lower = TRUE) {
  if (is.numeric(dpmo)) {
    lower_bound <- max(sql_table$sql[sql_table$dpmo >= dpmo])
    upper_bound <- min(sql_table$sql[sql_table$dpmo <= dpmo])
    sql_vec <- c(lower_bound, upper_bound)
    if (lower) {
      return(min(sql_vec))
    } else return(max(sql_vec))
  } else print("Input is not valid!")
}
## Example
# dpmo_to_sql(dpmo = 903200.0)

# compute sql (sigma quality level) given:
## d = defect opportunities per unit
## u = units produced per period
## a = total actual defects
get_sql <- function(d = NULL, u = NULL, a = NULL, dpmo = NULL) {
  not_null_d <- !is.null(d); not_null_u <- !is.null(u)
  not_null_a <- !is.null(a); not_null_dpmo <- !is.null(dpmo)
  exists_var <- not_null_d & not_null_u & not_null_a | not_null_dpmo
  if (exists_var) {
    if (is.numeric(dpmo)) {
      return(dpmo_to_sql(dpmo))
    } else if (all(is.numeric(c(d, u, a)))) {
      dpmo <- get_dpmo(d = d, u = u, a = a)
      return(dpmo_to_sql(dpmo))
    } else print("Input is not valid!")
  } else print("Input is not valid!")
}
## Example
# get_sql(dpmo = 200000)
# get_sql(d = 1, u = 30, a = 6)

# compute dpmo (defects per million opportunities) given sql (sigma quality level) value
sql_to_dpmo <- function(sql, lower = FALSE) {
  if (is.numeric(sql)) {
    lower_bound <- max(sql_table$dpmo[sql_table$sql >= sql])
    upper_bound <- min(sql_table$dpmo[sql_table$sql <= sql])
    dpmo_vec <- c(lower_bound, upper_bound)
    if (lower) {
      return(min(dpmo_vec))
    } else return(max(dpmo_vec))
  } else print("Input is not valid!")
}
## Example
# sql_to_dpmo(sql = 2.3)

# Kappa (k) is a measure used to evaluate a DISCRETE measurement system on: 
# k = (probability_observed - probability_chance) / (1 - probability_chance)
# reproducibility (consistency across operators)
# repeatability (consistency by an operator)
# p_o = probability_observed
# p_c = probability_chance
# k = (p_o - p_c) / (1 - p_c)
# If k > 0.7 then measurement system is "good" else it is "bad" in terms of reproducibility
# If k > 0.85 then measurement system is "good" else it is "bad" in terms of repeatability

# compute kappa score of a binary discrete measurement system (T/F)
# Good = TRUE, Bad = FALSE
# input: data frame of 2 or more logical vectors
get_kappa_df <- function(df) {
  if (is.data.frame(df) & all(sapply(df, is.logical))) {
    if (all(sapply(df, function(x) all(x == 1)))) {
      return(1)
    } else {
      # calculate total observations
      obs <- dim(df)[1]
      # calculate probability observed
      # this is the sum of the number of observations where all measures are equal divided by total observations
      p_o <- sum(apply(df, 1, function(x) mean(x) %in% c(1,0))) / obs
      # calculate probability chance
      #(i.e. (product of percent "good" across vectors) + (product of percent "bad" across vectors))
      perc_true_list <- lapply(df, function(x) sum(x) / obs)
      perc_false_list <- lapply(perc_true_list, function(x) 1-x)
      p_c <- sum(
        prod(unlist(perc_true_list)),
        prod(unlist(perc_false_list))
      )
      # calculate kappa score
      k <- (p_o - p_c) / (1 - p_c)
      return(k)
    }
  } else print("Input is not valid!")
}
# Example
# get_kappa_df(
#   data.frame(
#     day1 <- c(T,T,T,F,F,F,F,F,F,F,T,T),
#     dat2 <- c(T,F,T,F,F,T,F,F,F,F,T,F)
#   )
# )

# Compute kappa score of a bivariate discrete measurement system (e.g. 'G'/'B')
# (e.g. 'G'/'B'; Good = 'G', Bad = 'B')
# input: 2 bivariate discrete vectors x, y of the same length
get_kappa <- function(x, y) {
  is_valid_length <- length(x) == length(y) & length(x) > 0
  is_bi_var <- length(levels(factor(c(x,y)))) %in% c(1,2)
  if (is_valid_length & is_bi_var) {
    if (all(x == y)) {
      return(1)
    } else {
      # possible values
      values <- levels(factor(c(x,y)))
      # calculate total observations
      obs <- length(x)
      # calculate probability observed 
      # (i.e. sum(# of observations where all measures are equal) /  total observations)
      p_o <- sum(x == y) / obs
      # calculate probability chance; 
      #(i.e. (product of percent "good" across vectors) + (product of percent "bad" across vectors))
      vec_list <- list(x,y)
      perc_list1 <- lapply(vec_list, function(x) sum(x == values[1]) / obs)
      perc_list2 <- lapply(vec_list, function(x) sum(x == values[2]) / obs)
      p_c <- sum(
        prod(unlist(perc_list1)),
        prod(unlist(perc_list2))
      )
      # calculate kappa score
      k <- (p_o - p_c) / (1 - p_c)
      return(k)
    }
  } else print("Input is not valid!")
}
# Example
# set.seed(23); x = sample(c('G','B'), 12, replace = T)
# set.seed(2); y = sample(c('G','B'), 12, replace = T)
# get_kappa(x = x, y = y)

# Function to get confidence intervals for continuous data
# x = sample mean
# alpha = 1 minus the level of confidence you want
# s = standard deviation of sample
# n = sample size
get_ci_c <- function(x, alpha, s, n) {
  # compute z or t value based on the sample size
  test_stat <- if (n >= 30) {abs(qnorm(p = alpha / 2))} else if (n < 30) {abs(qt(p = alpha / 2, df = n - 1))}
  if (is.numeric(test_stat)) {
    # upper limit
    u <- x + test_stat * s / sqrt(n)
    # lower limit
    l <- x - test_stat * s / sqrt(n)
    c_interval <- c(u, l)
    names(c_interval) <- c('upper_limit', 'lower_limit')
    return(c_interval)
  }
}
# Example
# get_ci_c(x = 17.23, alpha = 0.05, s = 4.52, n = 30)

# Function to get sample size for continuous data given a desired margin of error
# alpha = 1 minus the level of confidence (e.g. 95% confidence)
# s = standard deviation of population
# e = desired margin of error
get_sample_size_c <- function(alpha, sigma, e) {
  z <- abs(qnorm(p = alpha / 2))
  n <- (z * sigma / e) ^ 2
  return(n)
}
# Example 
# get_sample_size_c(alpha = 0.05, sigma = 3, e = 2)

# Function to get confidence intervals for discrete data
# p = sample proportion
# alpha = 1 minus the level of confidence (e.g. 99% confidence)
# n = sample size
get_ci_d <- function(p, alpha, n) {
  # compute z or t value based on the sample size
  test_stat <- abs(qnorm(p = alpha / 2))
  if (is.numeric(test_stat)) {
    # upper limit
    u <- p + test_stat * sqrt(p * (1 - p) / n)
    # lower limit
    l <- p - test_stat * sqrt(p * (1 - p) / n)
    c_interval <- c(u, l)
    names(c_interval) <- c('upper_limit', 'lower_limit')
    return(c_interval)
  }
}
# Example
# get_ci_d(p = 164/300, alpha = 0.01, n = 300)

# Function to get sample size for discrete data given a desired margin of error
# alpha = 1 minus the level of confidence (e.g. 95% confidence)
# p = proportion of successes in population
# e = desired margin of error
get_sample_size_d <- function(alpha, p, e) {
  # z value associated with confidence of interest
  z <- abs(qnorm(p = alpha / 2))
  n <- z^2 * p * (1 - p) / e^2
  return(n)
}
# Example 
# get_sample_size_d(alpha = 0.05, p = .5, e = 2)

# Function to get margin of error for continuous data confidence interval
# alpha = 1 minus the level of confidence (e.g. 95% confidence)
# s = standard deviation
# n = sample size
get_margin_error_c <- function(alpha, s, n) {
  z <- abs(qnorm(p = alpha / 2))
  z * (s / sqrt(n))
}
# Example
# get_margin_error_c(alpha = 0.05, s = 1500, n = 385)

# Function to get margin of error for discrete data confidence interval
# alpha = 1 minus the level of confidence (e.g. 95% confidence)
# p = sample proportion
# n = sample size
get_margin_error_d <- function(alpha, p, n) {
  z <- abs(qnorm(p = alpha / 2))
  z * sqrt(p * (1 - p) / n)
}
# Example
# get_margin_error_d(alpha = 0.05, p = 15/59, n = 59)

# Function to get binwidth for histogram given variable x
get_binwidth <- function(x) {
  breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
  binwidth <- breaks[2] - breaks[1]
  return(binwidth)
}

# Function to get confidence intervals for continuous data
# x = sample mean
# alpha = 1 minus the level of confidence you want
# s = standard deviation of sample
# n = sample size
get_ci_c <- function(x, alpha, s, n) {
  # compute z or t value based on the sample size
  test_stat <- if (n >= 30) {abs(qnorm(p = alpha / 2))} else if (n < 30) {abs(qt(p = alpha / 2, df = n - 1))}
  if (is.numeric(test_stat)) {
    # upper limit
    u <- x + test_stat * s / sqrt(n)
    # lower limit
    l <- x - test_stat * s / sqrt(n)
    c_interval <- c(u, l)
    names(c_interval) <- c('upper_limit', 'lower_limit')
    return(c_interval)
  }
}
# function to get number of models from a scenario name
get_num_models2 <- function(batch_id) {
  require(magrittr)
  x <- strsplit(batch_id, split = '_') %>% unlist
  if (x[2] == 'sheet') {
    return(as.numeric(x[4]))
  } else return(
    as.numeric(x[5])
  )
}

# function to get date of batch creation from a scenario name
get_batch_date <- function(batch_id) {
  require(magrittr)
  x <- strsplit(batch_id, split = '_') %>% unlist
  if (x[2] == 'sheet') {
    return(
      x[5:7] %>% paste(., collapse = '-')
    )
  } else return(
    x[6:8] %>% paste(., collapse = '-')
  )
}

# function to get batch id
get_batch_id <- function(batch_id) {
  require(magrittr)
  x <- strsplit(batch_id, split = '_') %>% unlist 
  return(paste(x[-length(x)], collapse = '_'))
}
