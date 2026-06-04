# This script was used to test generating plots for the SEDAR105 TWG
# This version uses the _merged_no_rt data with all new scenarios
# Many boxplots were eliminated in favor of time series.  


# Set-up and get data -----------------------------------------------------

library(tidyverse)
library(patchwork)
library(knitr)
library(kableExtra)

#location of the inputs
model_SSMSE_dir <- file.path("base_models")
run_SSMSE_dir <- file.path("runs_output")
plot_folder <- "merged_no_rt_plots"

#name of the results files and input settings
results_name <- "_merged_no_rt"
n_iterations <- 100
min_year <- 2018
max_year <- 2065
model_run_selection <- 2065
max_year_short_term <- min_year+4
save <- TRUE

#create a list of scenarios for plot generation, usually the default order is fine.  
#scen_list <- unique(summary$ts$scenario)
#hard coded in a specific order.  
scen_list <- c(
  "no_rt_x_no_rt",
  "no_rt_x_flat_all_yrs",
  "no_rt_x_young_all_yrs",
  "no_rt_x_mid_all_yrs",
  "no_rt_x_old_all_yrs",
  "no_rt_x_flat_rt_17",
  "no_rt_x_young_rt_17",
  "no_rt_x_mid_rt_17",
  "no_rt_x_old_rt_17",
  "flat_x_no_rt",
  "young_x_no_rt",
  "mid_x_no_rt",
  "old_x_no_rt",
  "flat_x_flat_rt_34",
  "young_x_young_rt_34",
  "old_x_old_rt_34",
  "mid_x_mid_rt_34",
  "flat_x_young_rt_34",
  "flat_x_old_rt_34",
  "flat_x_mid_rt_34",
  "young_x_flat_rt_34",
  "young_x_old_rt_34",
  "young_x_mid_rt_34",
  "old_x_flat_rt_34",
  "old_x_young_rt_34",
  "old_x_mid_rt_34",
  "mid_x_flat_rt_34",
  "mid_x_young_rt_34",
  "mid_x_old_rt_34",
  "flat_x_flat_34_all_yrs",
  "young_x_young_34_all_yrs",
  "old_x_old_34_all_yrs",
  "mid_x_mid_34_all_yrs",
  "flat_x_young_34_all_yrs",
  "flat_x_old_34_all_yrs",
  "flat_x_mid_34_all_yrs",
  "young_x_flat_34_all_yrs",
  "young_x_old_34_all_yrs",
  "young_x_mid_34_all_yrs",
  "old_x_flat_34_all_yrs",
  "old_x_young_34_all_yrs",
  "old_x_mid_34_all_yrs",
  "mid_x_flat_34_all_yrs",
  "mid_x_young_34_all_yrs",
  "mid_x_old_34_all_yrs"
)


#pull the summary files, the dat file isn't actually that important.  
summary <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name, ".rda")))

# Filter the summary data
#   Remove "Base" model runs, remove the last 3 years of data of each model_run, 
#   remove any NA scenarios that aren't in the list above.  
#   Break up the scenario names in the following format:  
#         om_name (no_rt, flat, young, old, mid), 
#         em_name (no_rt, flat, young, old, mid), 
#         exp_type (all_yrs, rt_34, no_rt)

summary$ts <- summary$ts %>%
  filter(model_run != "", !str_detect(model_run, "Base")) %>% #remove "Base" model 
  mutate(end_year = as.numeric(str_extract(model_run, "\\d{4}$")) + 3, 
         years_until_terminal = end_year - year) %>%
  filter(case_when(
    str_detect(model_run, "_EM") ~ years_until_terminal > 2,
    TRUE ~ TRUE # Keep all other rows if no _EM
  )) %>%
  mutate(
    scenario = factor(scenario, scen_list)
  ) %>%
  filter(!is.na(scenario)) %>%
  separate_wider_regex(
    cols = scenario,
    patterns = c(
      om_name = "^[^_]+(?:_[^_]+)*", 
      "_x_",                          
      em_name = "[a-z]+(?:_[a-z]+)*", 
      exp_type = ".*"                 
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(
    # Clean up leading underscores in exp_type
    exp_type = str_remove(exp_type, "^_"),
    
    # Add "rt_" back to exp_type ONLY if it is purely a number
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type),
    
    # Strip "_rt" only if it follows flat, young, old, or mid
    across(c(om_name, em_name), ~ str_replace_all(.x, "(flat|young|old|mid)_rt", "\\1")) 
  ) %>%
  mutate(Commercial = deadB_1 + deadB_2, Recreational = deadB_4)

summary$dq <- summary$dq %>%
  filter(model_run != "", !str_detect(model_run, "Base")) %>%
  mutate(end_year = as.numeric(str_extract(model_run, "\\d{4}$")) + 3,
         years_until_terminal = end_year - year) %>%
  filter(case_when(
    str_detect(model_run, "_EM") ~ years_until_terminal > 2,
    TRUE ~ TRUE # Keep all other rows if no _EM
  )) %>%
  mutate(
    scenario = factor(scenario, scen_list)
  ) %>%
  filter(!is.na(scenario)) %>%
  separate_wider_regex(
    cols = scenario,
    patterns = c(
      om_name = "^[^_]+(?:_[^_]+)*", 
      "_x_",                          
      em_name = "[a-z]+(?:_[a-z]+)*", 
      exp_type = ".*"                 
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(
    # Clean up leading underscores in exp_type
    exp_type = str_remove(exp_type, "^_"),
    
    # Add "rt_" back to exp_type ONLY if it is purely a number
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type),
    
    # Strip "_rt" only if it follows flat, young, old, or mid
    across(c(om_name, em_name), ~ str_replace_all(.x, "(flat|young|old|mid)_rt", "\\1"))
  ) 
  

summary$scalar <- summary$scalar %>%
  filter(model_run != "", !str_detect(model_run, "Base")) %>%
  filter(!is.na(scenario)) %>%
  separate_wider_regex(
    cols = scenario,
    patterns = c(
      om_name = "^[^_]+(?:_[^_]+)*", 
      "_x_",                          
      em_name = "[a-z]+(?:_[a-z]+)*", 
      exp_type = ".*"                 
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(
    # Clean up leading underscores in exp_type
    exp_type = str_remove(exp_type, "^_"),
    
    # Add "rt_" back to exp_type ONLY if it is purely a number
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type),
    
    # Strip "_rt" only if it follows flat, young, old, or mid
    across(c(om_name, em_name), ~ str_replace_all(.x, "(flat|young|old|mid)_rt", "\\1"))
  )

# Sets of scenarios for filtering

core_4 <- c("no_rt_x_no_rt",
            "no_rt_x_flat_rt_17",
            "flat_x_no_rt",
            "flat_x_flat_rt_34")

all_years <- c("no_rt_x_flat_all_yrs", "flat_x_flat_34_all_yrs")

selectivity_rt_34 <- c(
  "flat_x_flat_rt_34",
  "young_x_young_rt_34",
  "old_x_old_rt_34",
  "mid_x_mid_rt_34",
  "flat_x_young_rt_34",
  "flat_x_old_rt_34",
  "flat_x_mid_rt_34",
  "young_x_flat_rt_34",
  "young_x_old_rt_34",
  "young_x_mid_rt_34",
  "old_x_flat_rt_34",
  "old_x_young_rt_34",
  "old_x_mid_rt_34",
  "mid_x_flat_rt_34",
  "mid_x_young_rt_34",
  "mid_x_old_rt_34"
)

selectivity_all_yrs <- c(
  "flat_x_flat_34_all_yrs",
  "young_x_young_34_all_yrs",
  "old_x_old_34_all_yrs",
  "mid_x_mid_34_all_yrs",
  "flat_x_young_34_all_yrs",
  "flat_x_old_34_all_yrs",
  "flat_x_mid_34_all_yrs",
  "young_x_flat_34_all_yrs",
  "young_x_old_34_all_yrs",
  "young_x_mid_34_all_yrs",
  "old_x_flat_34_all_yrs",
  "old_x_young_34_all_yrs",
  "old_x_mid_34_all_yrs",
  "mid_x_flat_34_all_yrs",
  "mid_x_young_34_all_yrs",
  "mid_x_old_34_all_yrs"
)

OM_runs <- summary$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary$ts %>%
  filter(str_detect(model_run, "EM"))

OM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "OM"))

EM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "EM"))

# Average Timeseries plots --------------------------------------------------------

## mean F_5 over time

# prepare data for stat of variable over time plots
# create a data frame of OM means, medians, and sds by year and scenario
OM_lines <- OM_runs %>%
  filter(
    str_detect(model_run, as.character(model_run_selection)) | 
      str_detect(model_run, "_OM")
  ) %>%  group_by(year, scenario) %>%
  summarise(
    across(
      .cols = where(is.numeric), # Selects all numeric columns
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE), # Mean function
        median = ~ median(.x, na.rm = TRUE), # Median function
        sd = ~ sd(.x, na.rm = TRUE) # Standard Deviation function
      ),
      # Names the new columns (e.g., value1_mean, value1_median, value1_sd)
      .names = "{.col}_{.fn}" 
    ),
    .groups = "drop" # Drops the grouping structure
  ) %>% mutate(model_type = "OM")

# create a data frame of EM means, medians, and sds by year and scenario
# this currently uses all model_runs, should it just be the last model_run?  
EM_lines <- EM_runs %>%
  filter(
    str_detect(model_run, as.character(model_run_selection)) | 
      str_detect(model_run, "_OM")
  ) %>%  group_by(year, scenario) %>%
  summarise(
    across(
      .cols = where(is.numeric), # Selects all numeric columns
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE), # Mean function
        median = ~ median(.x, na.rm = TRUE), # Median function
        sd = ~ sd(.x, na.rm = TRUE) # Standard Deviation function
      ),
      # Names the new columns (e.g., value1_mean, value1_median, value1_sd)
      .names = "{.col}_{.fn}" 
    ),
    .groups = "drop" # Drops the grouping structure
  ) %>% mutate(model_type = "EM")

combined_lines <- rbind(OM_lines, EM_lines)

# Set the factor level order so EM drawn last (on top of plot)
combined_lines$model_type <- factor(
  combined_lines$model_type, 
  levels = c("OM", "EM") 
)

plot_variable_ts <- function(data = combined_lines, variable = "deadB_5", stat_type = "median", years = c(2004,2025)){
  y_var_sym = sym(paste0(variable, "_", stat_type))
  
  ggplot(data, aes(x = year, y = !!y_var_sym, color = model_type)) +
    geom_line(aes(linetype = model_type)) +
    facet_wrap(~scenario) +
    ggtitle(paste(stat_type, variable, "over time")) +
    scale_color_manual(
      name = "Model",
      values = c("OM" = "#D65F00", "EM" = "black"), 
      labels = c("OM" = "OM", "EM" = "EM"),
      breaks = c("OM", "EM") 
    ) +
    scale_linetype_manual(
      name = "Model", 
      values = c("OM" = "solid", "EM" = "dashed"),
      labels = c("OM" = "OM", "EM" = "EM"),
      breaks = c("OM", "EM") 
    ) + 
    coord_cartesian(xlim = years)
}

# #### Core 4

combined_lines %>%
  filter(scenario %in% core_4) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Average red tide mortality over time - Core") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_core.png"),
         width = 8, height = 6, units = "in", device = "png")
}

# #### All Years

combined_lines %>%
  filter(scenario %in% all_years) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Average red tide mortality over time - All Years") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_all_yrs.png"),
         width = 8, height = 3, units = "in", device = "png")
}

#### Selectivity

just_matching_selectivity_rt_2 <- c("flat_x_flat_rt_34",
                                    "young_x_young_rt_34",
                                    "old_x_old_rt_34",
                                    "mid_x_mid_rt_34")

combined_lines %>% 
  filter(scenario %in% just_matching_selectivity_rt_2) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - 17 Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_sel_rt_2.png"),
         width = 8, height = 6, units = "in", device = "png")
}

just_matching_selectivity_rt_34 <- c("flat_x_flat_34_all_yrs",
                                    "young_x_young_34_all_yrs",
                                    "old_x_old_34_all_yrs",
                                    "mid_x_mid_34_all_yrs")

combined_lines %>% 
  filter(scenario %in% just_matching_selectivity_rt_34) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - All Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_sel_all_yrs.png"),
         width = 8, height = 6, units = "in", device = "png")
}


# Error Tables ------------------------------------------------------------

## error table - for all 4 experiments separately

create_residual_kable <- function(min_year, max_year, scenario_list, em_run_year) {
  residual_runs_prop <- EM_runs %>%
    filter(
      str_detect(model_run, as.character(em_run_year))) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_em-Recruit_0_om,
      res_F_5 = F_5_em-F_5_om,
      res_SpawnBio = SpawnBio_em-SpawnBio_om,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = commercial-com_om,
      res_rec = recreational-deadB_4_om,
      res_dead_5 = deadB_5_em-deadB_5_om,
      res_abundance = Bio_smry_em-Bio_smry_om
    )
  
  residual_runs_prop %>% 
    filter(year %in% seq(min_year, max_year, 1), scenario %in% scenario_list) %>%
    group_by(scenario) %>%
    reframe(
      prop_com = (sum(res_com) / sum(com_om))*100,
      prop_rec = (sum(res_rec) / sum(deadB_4_om))*100,
      prop_red = (sum(res_dead_5) /  sum(deadB_5_om))*100,
      raw_total = (sum(res_com)/n_iterations+sum(res_rec)/n_iterations+sum(res_dead_5)/n_iterations),
      raw_prop = (sum(res_com)+sum(res_rec)+sum(res_dead_5)) /  (sum(com_om)+sum(deadB_4_om) + sum(deadB_5_om)) *100
    ) %>% 
    kable(
      # Rename columns directly within kable
      col.names = c("Scenario", "Commercial Catch Residual Sum (%)", "Recreational Catch Residual Sum (%)", "Red Tide Discards Residual Sum (%)", "Total Removals Residual Sum (MT)", "Proportion of Residuals to Total (%)"),
      align = c("l", "c", "c", "c", "c", "c"), # Align columns (left, center, center, center)
      digits = 2
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
      full_width = FALSE # Don't stretch table to full page width
    ) 
}

#### Core 4

kable_core <- create_residual_kable(min_year, max_year_short_term, core_4, max_year)
kable_core

if(save == TRUE){
  save_kable(kable_core, file = file.path(run_SSMSE_dir, plot_folder,"core_kable.html"))
}

#### All Years

kable_all_yrs <- create_residual_kable(min_year, max_year_short_term, all_years, max_year)
kable_all_yrs

if(save == TRUE){
  save_kable(kable_all_yrs, file = file.path(run_SSMSE_dir,plot_folder,"all_years_kable.html"))
}

#### Selectivity rt_34

kable_sel_rt_34 <- create_residual_kable(min_year, max_year_short_term, selectivity_rt_34, max_year)
kable_sel_rt_34

if(save == TRUE){
  save_kable(kable_sel_rt_34, file = file.path(run_SSMSE_dir,plot_folder,"sel_rt_34_kable.html"))
}

#### Selectivity all_yrs

kable_sel_all_yrs <- create_residual_kable(min_year, max_year_short_term, selectivity_all_yrs, max_year)
kable_sel_all_yrs

if(save == TRUE){
  save_kable(kable_sel_all_yrs, file = file.path(run_SSMSE_dir,plot_folder,"sel_all_yrs_kable.html"))
}

# Long Term Error Tables --------------------------------------------------

# #### Core 4

kable_core <- create_residual_kable(min_year, max_year, core_4, max_year)
kable_core

if(save == TRUE){
  save_kable(kable_core, file = file.path(run_SSMSE_dir,plot_folder,"core_longterm_kable.html"))
}

# #### All Years

kable_all_yrs <- create_residual_kable(min_year, max_year, all_years, max_year)
kable_all_yrs

if(save == TRUE){
  save_kable(kable_all_yrs, file = file.path(run_SSMSE_dir,plot_folder,"all_years_longterm_kable.html"))
}

#### Selectivity rt_2

kable_sel_rt_2 <- create_residual_kable(min_year, max_year, selectivity_rt_34, max_year)
kable_sel_rt_2

if(save == TRUE){
  save_kable(kable_sel_rt_2, file = file.path(run_SSMSE_dir,plot_folder,"sel_rt_34_longterm_kable.html"))
}

#### Selectivity all_yrs

kable_sel_all_yrs <- create_residual_kable(min_year, max_year, selectivity_all_yrs, max_year)
kable_sel_all_yrs

if(save == TRUE){
  save_kable(kable_sel_all_yrs, file = file.path(run_SSMSE_dir,plot_folder,"sel_all_yrs_longterm_kable.html"))
}

#  Median time series plots -------------------------------

plot_median_ts_om <- function (summary_data = summary$ts, scenario_list, min_yr = min_year, max_yr = max_year, col_name = "Recreational", experiment_type) {
  plot_data <- summary_data %>%
    filter(
      scenario %in% c(scenario_list),
      str_detect(model_run, "OM"),
      year >= min_yr,
      year <= max_yr
    ) %>%
    group_by(om_name, em_name, year) %>%
    reframe(
      # Use .data[[col_name]] to look up the column using a string variable
      med_val = median(.data[[col_name]], na.rm = TRUE),
      low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
      high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
      .groups = "drop" # Keeps your console clean of grouping messages
    )
  
  new_labels <- c("young" = "True: Young Selectivity", 
                  "mid" = "True: Middle Selectivity",
                  "old" = "True: Old Selectivity", 
                  "flat" = "True: Flat Selectivity", 
                  "no_rt" = "True: No Red Tide")
  
  # Plotting
  ggplot(plot_data, aes(x = year, y = med_val)) +
    geom_ribbon(aes(ymin = low, ymax = high, fill = em_name), alpha = 0.2) +
    geom_line(aes(color = em_name)) + 
    ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels))+ 
    labs(color = "Assumed Selectivity", fill = "Assumed Selectivity") + xlab("Year")
}

##### Recreational #####
# generic rt_34 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_rt_34, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = c(selectivity_rt_34, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = c(selectivity_rt_34, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")


##### Commercial #####
# generic rt_34 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = selectivity_rt_34, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = c(selectivity_rt_34, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = c(selectivity_rt_34, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")


##### Recruitment #####
# generic rt_34 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = selectivity_rt_34, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = c(selectivity_rt_34, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = c(selectivity_rt_34, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

##### SSB #####
# generic rt_34 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_34, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = c(selectivity_rt_34, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = c(selectivity_rt_34, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")


### Add more Lines 
plot_median_ts_om_lines <- function (summary_data = summary$ts, scenario_list, min_yr = min_year, max_yr = max_year, col_name = "Recreational", experiment_type) {
  
  # 1. First, get the filtered, raw iteration-level data
  raw_filtered_data <- summary_data %>%
    filter(
      scenario %in% c(scenario_list),
      str_detect(model_run, "OM"),
      year >= min_yr,
      year <= max_yr
    )
  
  # 2. Then, calculate your summary statistics from that filtered data
  plot_summary_data <- raw_filtered_data %>%
    group_by(om_name, em_name, year) %>%
    reframe(
      med_val = median(.data[[col_name]], na.rm = TRUE),
      low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
      high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
      .groups = "drop" 
    )
  
  new_labels <- c("young" = "True: Young Selectivity", 
                  "mid" = "True: Middle Selectivity",
                  "old" = "True: Old Selectivity", 
                  "flat" = "True: Flat Selectivity", 
                  "no_rt" = "True: No Red Tide")
  
  # 3. Plotting
  ggplot() +
    # --- NEW: Individual iteration lines ---
    # We use the raw data here. 
    geom_line(data = filter(raw_filtered_data, iteration %in% c(1:5)), 
              aes(x = year, y = .data[[col_name]], color = em_name, group = interaction(iteration, om_name, em_name)), 
              alpha = 0.2) + # Low alpha to keep it in the background
    
    # --- Your original summary layers (using the summary dataset) ---
    geom_ribbon(data = plot_summary_data, 
                aes(x = year, ymin = low, ymax = high, fill = em_name), alpha = 0.2) +
    geom_line(data = plot_summary_data, 
              aes(x = year, y = med_val, color = em_name), linewidth = 1) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed Selectivity", fill = "Assumed Selectivity") + 
    xlab("Year")
}

plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")

plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_34, experiment_type = "Correct Years")
