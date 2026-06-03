# Merge 2 summary files

# Set-up and get data -----------------------------------------------------

library(tidyverse)
library(purrr)
library(dplyr)

#name of the first OM
model_SSMSE_dir <- file.path("base_models")
run_SSMSE_dir <- file.path("runs_output")

#name of the results files
results_name <- "_selectivity_random_34_extra"

#pull the summary and dat files, the dat file isn't actually that important.  
summary <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name, ".rda")))

results_name_orig <- "_no_rt_additions"
summary_2 <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name_orig, ".rda")))

summary_2_filtered <- map(summary_2, function(df) {
  if ("scenario" %in% names(df)) {
    df <- df %>% filter(!grepl("rt_34$", scenario))
  }
  return(df)
})

# Update the scenarios
updated_list <- lapply(summary, function(sub_list) {
  if ("scenario" %in% names(sub_list)) {
    sub_list$scenario <- gsub("_all_yrs", "_34_all_yrs", sub_list$scenario)
    sub_list$scenario <- gsub("_rt_2", "_rt_34", sub_list$scenario)
  }
  return(sub_list)
})

# This pairs them up by their element names and binds the rows
merged_summary <- map2(summary_2_filtered, updated_list[names(summary_2_filtered)], bind_rows)

saveRDS(merged_summary, file = file.path(run_SSMSE_dir, paste0("results_summary_merged_no_rt.rda")))

