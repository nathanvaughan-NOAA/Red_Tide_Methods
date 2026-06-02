# Merge 2 summary files


# Set-up and get data -----------------------------------------------------

library(tidyverse)
library(patchwork)
library(knitr)
library(kableExtra)
# OM locations

#name of the first OM
OM_name <- "default_sigmaR"
model_SSMSE_dir <- file.path("base_models")
default <- file.path(model_SSMSE_dir, OM_name)
run_SSMSE_dir <- file.path("runs_output")

#name of the results files
results_name <- "_selectivity_random_34_extra"
n_iterations <- 100
min_year <- 2018
max_year <- 2116
max_year_short_term <- min_year+4
save <- TRUE

#create a list of scenarios for plot generation, usually the default order is fine.  
#scen_list <- unique(summary$ts$scenario)
#hard coded in a specific order.  
scen_list <- c(
  "no_rt",
  "no_rt_x_rt_2",
  "rt_2_x_no_rt",
  "rt_2_x_rt_2", 
  "no_rt_x_all_yrs",
  "rep_3_x_all_yrs",
  "rt_2_x_all_yrs",
  "flat_x_flat_rt_2",
  "young_x_young_rt_2",
  "old_x_old_rt_2",
  "mid_x_mid_rt_2",
  "flat_x_young_rt_2",
  "flat_x_old_rt_2",
  "flat_x_mid_rt_2",
  "young_x_flat_rt_2",
  "young_x_old_rt_2",
  "young_x_mid_rt_2",
  "old_x_flat_rt_2",
  "old_x_young_rt_2",
  "old_x_mid_rt_2",
  "mid_x_flat_rt_2",
  "mid_x_young_rt_2",
  "mid_x_old_rt_2",
  "flat_x_flat_all_yrs",
  "young_x_young_all_yrs",
  "old_x_old_all_yrs",
  "mid_x_mid_all_yrs",
  "flat_x_young_all_yrs",
  "flat_x_old_all_yrs",
  "flat_x_mid_all_yrs",
  "young_x_flat_all_yrs",
  "young_x_old_all_yrs",
  "young_x_mid_all_yrs",
  "old_x_flat_all_yrs",
  "old_x_young_all_yrs",
  "old_x_mid_all_yrs",
  "mid_x_flat_all_yrs",
  "mid_x_young_all_yrs",
  "mid_x_old_all_yrs"
)

#pull the summary and dat files, the dat file isn't actually that important.  
summary <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name, ".rda")))
dat <- r4ss::SS_readdat(file.path(default, "red_grouper_1986_2017_RedTideFleet.dat"))

results_name_orig <- "_selectivity_100_redo_2"
summary_2 <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name_orig, ".rda")))

# Update the scenarios
updated_list <- lapply(summary, function(sub_list) {
  if ("scenario" %in% names(sub_list)) {
    sub_list$scenario <- gsub("_all_yrs", "_34_all_yrs", sub_list$scenario)
    sub_list$scenario <- gsub("_rt_2", "_rt_34", sub_list$scenario)
  }
  return(sub_list)
})

library(purrr)
library(dplyr)

# This pairs them up by their element names and binds the rows
merged_summary <- map2(summary_2, updated_list[names(summary_2)], bind_rows)

saveRDS(merged_summary, file = file.path(run_SSMSE_dir, paste0("results_summary_merged.rda")))

