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
plot_folder <- "red_tide_no_rt_fix_final"

#name of the results files and input settings
results_name <- "_red_tide_no_rt_fix"
n_iterations <- 100
min_year <- 2018
max_year <- 2068
model_run_selection <- 2068
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


#pull the summary files, the dat file isn't actually that important.  
summary <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name, ".rda")))

# Filter the summary data
#   Remove "Base" model runs, remove the last 3 years of data of each model_run, 
#   remove any NA scenarios that aren't in the list above.  
#   Break up the scenario names in the following format:  
#         om_name (no_rt, flat, young, old, mid), 
#         em_name (no_rt, flat, young, old, mid), 
#         exp_type (all_yrs, rt_34, no_rt)
# Remove bad gradients

bad_runs <- summary$scalar %>% 
  filter(max_grad > 1) %>%
  select(scenario, iteration) %>%
  distinct() 



# Sets of scenarios for filtering

core_4 <- c("no_rt_x_no_rt",
            "no_rt_x_flat_rt_17",
            "flat_x_no_rt",
            "flat_x_flat_rt_2")

all_years <- c("no_rt_x_flat_all_yrs", "flat_x_flat_all_yrs")

selectivity_rt_2 <- c(
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
  "mid_x_old_rt_2"
)

selectivity_all_yrs <- c(
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


combined_lines %>%
  filter(scenario %in% c("flat_x_no_rt", "mid_x_no_rt", "young_x_no_rt", "old_x_no_rt")) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Average red tide mortality over time - Core") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality")

combined_lines %>%
  filter(scenario %in% c(  "no_rt_x_no_rt",
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
                           "old_x_no_rt")) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Average red tide mortality over time - Core") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality")


unique(summary$ts$scenario)

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

new_labels <- c("flat_x_flat_rt_2" = "flat x flat - rt_17", 
                "flat_x_no_rt" = "flat x no_rt",
                "no_rt_x_flat_rt_17" = "no_rt x flat - rt_17", 
                "no_rt_x_no_rt" = "no_rt x no_rt")

combined_lines %>%
  filter(scenario %in% c("flat_x_flat_rt_2", "no_rt_x_no_rt")) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Matching") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality") +
  facet_grid(~scenario, labeller = labeller(scenario = new_labels)) +
  theme(
    text = element_text(size = 16),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_core_wide_matching.png"),
         width = 6, height = 4, units = "in", device = "png")
}

combined_lines %>%
  filter(scenario %in% c("flat_x_no_rt", "no_rt_x_flat_rt_17")) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  ggtitle("Not Matching") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality") +
  facet_grid(~scenario, labeller = labeller(scenario = new_labels)) +
  theme(
    text = element_text(size = 16),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_core_wide_not_matching.png"),
         width = 6, height = 4, units = "in", device = "png")
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

just_matching_selectivity_rt_2 <- c("flat_x_flat_rt_2",
                                    "young_x_young_rt_2",
                                    "mid_x_mid_rt_2", 
                                    "old_x_old_rt_2")

new_labels <- c("flat_x_flat_rt_2" = "flat x flat - rt_17", 
                "young_x_young_rt_2" = "young x young - rt_17",
                "old_x_old_rt_2" = "old x old - rt_17", 
                "mid_x_mid_rt_2" = "mid x mid - rt_17")

combined_lines %>% 
  filter(scenario %in% just_matching_selectivity_rt_2) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - 17 Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality") +
  facet_wrap(~scenario, labeller = labeller(scenario = new_labels)) +
  theme(
    text = element_text(size = 16),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "ts_mean_F_5_sel_rt_2.png"),
         width = 8, height = 6, units = "in", device = "png")
}

just_matching_selectivity_rt_2 <- c("flat_x_flat_all_yrs",
                                    "young_x_young_all_yrs",
                                    "old_x_old_all_yrs",
                                    "mid_x_mid_all_yrs")

combined_lines %>% 
  filter(scenario %in% just_matching_selectivity_rt_2) %>%
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

kable_all <- create_residual_kable(min_year, max_year_short_term, scen_list, max_year)
kable_all

if(save == TRUE){
  save_kable(kable_all, file = file.path(run_SSMSE_dir, plot_folder,"all_kable.html"))
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

#### Selectivity rt_2

kable_sel_rt_2 <- create_residual_kable(min_year, max_year_short_term, selectivity_rt_2, max_year)
kable_sel_rt_2

if(save == TRUE){
  save_kable(kable_sel_rt_2, file = file.path(run_SSMSE_dir,plot_folder,"sel_rt_2_kable.html"))
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

kable_sel_rt_2 <- create_residual_kable(min_year, max_year, selectivity_rt_2, max_year)
kable_sel_rt_2

if(save == TRUE){
  save_kable(kable_sel_rt_2, file = file.path(run_SSMSE_dir,plot_folder,"sel_rt_2_longterm_kable.html"))
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
# generic rt_2 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SPRratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt", "no_rt_x_no_rt"), experiment_type = "All Years")


##### Commercial #####
# generic rt_2 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Commercial", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")


##### Recruitment #####
# generic rt_2 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "Recruit_0", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

##### SSB #####
# generic rt_2 and all years
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# add no years line
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years")
plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")

library(viridis)

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
                  "no_rt" = "True: No Red Tide (OM)")
  
  # 3. Plotting
  ggplot() +
    # --- NEW: Individual iteration lines ---
    # We use the raw data here. 
    geom_line(data = filter(raw_filtered_data, iteration %in% c(1:5)), 
              aes(x = year, y = .data[[col_name]], color = em_name, group = interaction(iteration, om_name, em_name)), 
              alpha = 0.2) + # Low alpha to keep it in the background
    
    # --- Your original summary layers (using the summary dataset) ---
    geom_ribbon(data = plot_summary_data, 
                aes(x = year, ymin = low, ymax = high, fill = em_name), alpha = 0.1) +
    geom_line(data = plot_summary_data, 
              aes(x = year, y = med_val, color = em_name), linewidth = 1) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed\nSelectivity (EM)", fill = "Assumed\nSelectivity (EM)") + 
    xlab("Year")
}

# generic rt_2 and all years
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# generic rt_2 and all years
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = all_years, experiment_type = "Correct Years")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# Spawn Bio
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")

# Bratio
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt", "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years")
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt", "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

### Add more Lines 
plot_median_ts_lines <- function (summary_data = summary$ts, scenario_list, target_em, min_yr = min_year, max_yr = max_year, col_name = "Recreational", experiment_type) {
  
  # 1. First, get the filtered, raw iteration-level data
  raw_filtered_data <- summary_data %>%
    filter(
      scenario %in% c(scenario_list),
      str_detect(model_run, target_em),
      year >= min_yr,
      year <= max_yr
    )
  
  # 2. Then, calculate your summary statistics from that filtered data
  plot_summary_data <- raw_filtered_data %>%
    group_by(om_name, em_name, year) %>%
    reframe(
      med_val = mean(.data[[col_name]], na.rm = TRUE),
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
    ggtitle(paste0("Estimated ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed Selectivity", fill = "Assumed Selectivity") + 
    xlab("Year")
}

plot_median_ts_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), target_em = "_2065", experiment_type = "Correct Years")
plot_median_ts_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = core_4, target_em = "_2065", experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = core_4, experiment_type = "Presense or Absense of Red Tide")

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om_lines( min_yr = 1986, max_yr = 2060, col_name = "SPRratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SPRratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")


plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SPRratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17"), experiment_type = "Correct Years")


#most looked at:

#OM Data
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")

#EM Data
plot_median_ts_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")
plot_median_ts_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")


# True: No Red Tide
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c("no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - Known Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 
  

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "no_rt_true_bratio.png"),
         width = 6.5, height = 5, units = "in", device = "png")
}

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - Known Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "rt_17_bratio.png"),
         width = 6.5, height = 5, units = "in", device = "png")
}

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - All Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "all_yrs_bratio.png"),
         width = 6.5, height = 5, units = "in", device = "png")
}

summary_data <- summary$dq 
min_yr = 2017 
max_yr = 2068 
col_name = "Value.Bratio" 
scenario_list = c("flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt")
experiment_type = "Correct Years" 

# Assumed: No Red Tide
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
    med_val = mean(.data[[col_name]], na.rm = TRUE),
    low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
    high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
    .groups = "drop" 
  )

new_labels <- c("young" = "True: Young Selectivity", 
                "mid" = "True: Middle Selectivity",
                "old" = "True: Old Selectivity", 
                "flat" = "True: Flat Selectivity", 
                "no_rt" = "No Red Tide (EM)")

# 3. Plotting
ggplot() +
  # --- NEW: Individual iteration lines ---
  # We use the raw data here. 
  geom_line(data = filter(raw_filtered_data, iteration %in% c(1:5)), 
            aes(x = year, y = .data[[col_name]], color = om_name, group = interaction(iteration, om_name, em_name)), 
            alpha = 0.2) + # Low alpha to keep it in the background
  
  # --- Your original summary layers (using the summary dataset) ---
  geom_ribbon(data = plot_summary_data, 
              aes(x = year, ymin = low, ymax = high, fill = om_name), alpha = 0.1) +
  geom_line(data = plot_summary_data, 
            aes(x = year, y = med_val, color = om_name), linewidth = 1) + # Slightly thicker to pop out
  # --- Formatting layers ---
  ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
  ylab(paste0(col_name, " (MT)")) + 
  labs(color = "True Selectivity (OM)", fill = "True Selectivity (OM)") + 
  xlab("Year") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - Known Years") + 
  facet_grid(~em_name, labeller = labeller(em_name = new_labels)) + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "no_rt_em_true_bratio.png"),
         width = 6, height = 5, units = "in", device = "png")
}

# For Lisa

summary_data <- summary$dq 
min_yr = 2017 
max_yr = 2068 
col_name = "Value.Bratio" 
scenario_list = c("flat_x_no_rt", "no_rt_x_flat_rt_17", "no_rt_x_no_rt", "no_rt_x_flat_all_yrs", "flat_x_flat_rt_2", "flat_x_flat_all_yrs")
experiment_type = "Correct Years" 

# Assumed: No Red Tide
# 1. First, get the filtered, raw iteration-level data
raw_filtered_data <- summary_data %>%
  filter(
    scenario %in% c(scenario_list),
    str_detect(model_run, "OM"),
    year >= min_yr,
    year <= max_yr
  ) %>%
  mutate(
    frequency = case_when(
      str_detect(scenario, "no_rt$")   ~ "No Red Tide (EM)",
      str_detect(scenario, "rt_2$")   ~ "17 Red Tides (EM)",
      str_detect(scenario, "rt_17$")   ~ "17 Red Tides (EM)",
      str_detect(scenario, "all_yrs$") ~ "All Years (EM)"
    )
  )

# 2. Then, calculate your summary statistics from that filtered data
plot_summary_data <- raw_filtered_data %>%
  group_by(frequency, om_name, em_name, year) %>%
  reframe(
    med_val = mean(.data[[col_name]], na.rm = TRUE),
    low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
    high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
    .groups = "drop" 
  )

new_labels <- c("young" = "True: Young Selectivity", 
                "mid" = "True: Middle Selectivity",
                "old" = "True: Old Selectivity", 
                "flat" = "True: 17 Red Tides (OM)", 
                "no_rt" = "True: No Red Tide (OM)")

# 3. Plotting
ggplot() +
  # --- NEW: Individual iteration lines ---
  # We use the raw data here. 
  geom_line(data = filter(raw_filtered_data, iteration %in% c(1:5)), 
            aes(x = year, y = .data[[col_name]], color = frequency, group = interaction(iteration, frequency, om_name, em_name)), 
            alpha = 0.2) + # Low alpha to keep it in the background
  
  # --- Your original summary layers (using the summary dataset) ---
  geom_ribbon(data = plot_summary_data, 
              aes(x = year, ymin = low, ymax = high, fill = frequency), alpha = 0.1) +
  geom_line(data = plot_summary_data, 
            aes(x = year, y = med_val, color = frequency), linewidth = 1) + # Slightly thicker to pop out
  # --- Formatting layers ---
  ggtitle(paste0("Achieved ", col_name, " over time")) + 
  facet_wrap(~om_name, labeller = labeller(om_name = new_labels) ) +
  ylab(paste0(col_name, " (MT)")) + 
  labs(color = "Frequency (EM)", fill = "Frequency (EM)") + 
  xlab("Year") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "just_flat_bratios.png"),
         width = 7, height = 4, units = "in", device = "png")
}

# 1. Separate summary data by facet groupings
sum_em <- filter(plot_summary_data, frequency %in% c("All Years (EM)", "17 Red Tides (EM)"))
sum_om <- filter(plot_summary_data, frequency == "No Red Tide (EM)")

# Separate raw iterations data by facet groupings
raw_em <- filter(raw_filtered_data, frequency %in% c("All Years (EM)", "17 Red Tides (EM)"), iteration %in% 1:5)
raw_om <- filter(raw_filtered_data, frequency == "No Red Tide (EM)", iteration %in% 1:5)

# 2. Build plot with split layers
p <- ggplot() +
  
  # --- FACETS 1 & 2: Color mapped to em_name ---
  geom_line(
    data = raw_em,
    aes(x = year, y = .data[[col_name]], color = em_name, group = interaction(iteration, frequency, om_name, em_name)),
    alpha = 0.2
  ) +
  geom_ribbon(
    data = sum_em,
    aes(x = year, ymin = low, ymax = high, fill = em_name),
    alpha = 0.1
  ) +
  geom_line(
    data = sum_em,
    aes(x = year, y = med_val, color = em_name),
    linewidth = 1
  ) +
  
  # --- FACET 3: Color mapped to om_name ---
  geom_line(
    data = raw_om,
    aes(x = year, y = .data[[col_name]], color = om_name, group = interaction(iteration, frequency, om_name, em_name)),
    alpha = 0.2
  ) +
  geom_ribbon(
    data = sum_om,
    aes(x = year, ymin = low, ymax = high, fill = om_name),
    alpha = 0.1
  ) +
  geom_line(
    data = sum_om,
    aes(x = year, y = med_val, color = om_name),
    linewidth = 1
  ) +
  
  # --- Formatting & Faceting ---
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  facet_grid(~frequency) +
  labs(
    title = paste0("Achieved SSB Ratio over time - No red tides"),
    x = "Year",
    y = "SSB Ratio",
    color = "Selectivity \nAssumption",
    fill  = "Selectivity \nAssumption"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

print(p)

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "no_rt_em_all_bratios.png"),
         width = 9, height = 4, units = "in", device = "png")
}



# New Plots --------------------------------------------------

###### Gradients ###### 

# plot of raw gradients 

summary$scalar %>% 
  mutate(model_run_year = str_extract(model_run, "\\d+")) %>% #extract year from model_run
  ggplot(aes(model_run_year, max_grad))+
  geom_point() + 
  facet_wrap(~scenario) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# List of iterations that have max_grad > 1 in any model_run.  

bad_runs <- summary$scalar %>% 
  filter(max_grad > 1) %>%
  select(scenario, iteration) %>%
  distinct() 

bad_runs %>%
  count(scenario) %>% 
  arrange(desc(n)) %>%  
  kable(
    # Rename columns directly within kable
    col.names = c("Scenario", "Removed iterations"),
    align = c("l", "c"), # Align columns (left, center, center, center)
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  ) 

summary$scalar %>% 
  filter(max_grad > 1) %>%
  select(scenario, iteration) %>%
  distinct() %>%
  count(scenario)

summary_2 <- summary

summary_2$dq <- summary$dq %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))

#OM Data
plot_median_ts_om_lines(summary_2$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")
plot_median_ts_om_lines(summary_2$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")

#EM Data
plot_median_ts_lines(summary_2$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")
plot_median_ts_lines(summary_2$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")

summary_2$ts <- summary$ts %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))

OM_runs <- summary_2$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary_2$ts %>%
  filter(str_detect(model_run, "EM"))

kable_all <- create_residual_kable(min_year, max_year_short_term, scen_list, max_year)
kable_all

##### R0 ##### 

summary$scalar %>% 
  ggplot(aes(scenario, SR_LN_R0)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

summary_2$scalar <- summary$scalar %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))

summary_2$scalar %>% 
  ggplot(aes(scenario, SR_LN_R0)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

##### Selectivity ##### 

base_selectivities <- summary$scalar %>%
  select(model_run, starts_with("AgeSel"),-ends_with("1986")) %>%
  pivot_longer(
    cols = starts_with("AgeSel"),       # Selects Par1, Par2, etc.
    names_to = "age",                # New column name
    names_pattern = "AgeSel_P(\\d+)_RedTide_5",
    values_to = "selectivity",       # Where the cell values go
    names_transform = list(age = as.numeric) # Optional: converts "1", "2" to numbers
  ) %>%
  filter(
    str_detect(model_run, "_OM"),
    !is.na(selectivity)  # This drops any row where selectivity is NA
  ) %>%
  mutate(model_run = str_remove(model_run, "_OM")) %>%
  distinct()

base_selectivities %>%
  filter(model_run != "none") %>%
  ggplot(aes((age-1), selectivity)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ggtitle("Red tide selectivity at age") +
  facet_wrap(~model_run) + xlab("Age") + ylab("Selectivity")


##### one iteration example #### 
max_sample_year = 2068

key_models <- unique(summary$ts$model_run)
key_models <- key_models[grepl("OM", key_models) | grepl(as.character(max_sample_year), key_models)]

plot_data <- summary$ts %>%
  mutate(model_group = case_when(
    str_detect(model_run, "_OM") ~ "OM",
    str_detect(model_run, "_EM") ~ "EM",
    TRUE ~ "Other"  # Catch-all for anything else
  )) %>%
  mutate(model_group = factor(model_group, levels = c("OM", "EM")))

plot_data %>% 
  filter(model_run %in% key_models, iteration %in% 1, scenario %in% c("flat_x_flat_rt_2", "flat_x_flat_all_yrs"), year %in% 2000:2068) %>% #filters to just OM and max year runs
  ggplot(aes(x = year, y = F_5)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_vline(xintercept = 2005, color = "gray", linetype = "dashed") +
  geom_vline(xintercept = 2014, color = "gray", linetype = "dashed") +
  geom_line( aes(linetype = model_group, color = model_group))+
  scale_color_manual(values = c(
    "OM" = "darkorange", 
    "EM" = "black"
  )) +
  scale_linetype_manual(values = c(
    "OM" = "solid", 
    "EM" = "dashed"
  )) +
  facet_grid(iteration~scenario) +
  theme_bw()

plot_data %>% 
  filter(str_detect(model_run, "OM"), iteration %in% 1, scenario %in% c("flat_x_flat_rt_2"), year %in% 2000:2068) %>% #filters to just OM and max year runs
  ggplot(aes(x = year, y = F_5)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_vline(xintercept = 2005, color = "gray", linetype = "dashed") +
  geom_vline(xintercept = 2014, color = "gray", linetype = "dashed") +
  geom_line( aes(linetype = model_group, color = model_group), linewidth = 1)+
  scale_color_manual(values = c(
    "OM" = "darkorange", 
    "EM" = "black"
  )) +
  scale_linetype_manual(values = c(
    "OM" = "solid", 
    "EM" = "dashed"
  )) +
  theme_bw()+
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "rt_17_example.png"),
         width = 6, height = 3, units = "in", device = "png")
}

plot_data <- plot_data %>% 
  filter(model_run %in% key_models, iteration %in% 1, scenario %in% c("flat_x_flat_rt_2", "flat_x_flat_all_yrs"), year %in% 2000:2068) #filters to just OM and max year runs
  
OM_dat <- filter(plot_data, model_group == "OM")
EM_dat <- filter(plot_data, model_group == "EM")
p <-ggplot()+
  geom_segment(aes(x = OM_dat$F_5, y = OM_dat$year,
                   yend = EM_dat$year, xend = EM_dat$F_5), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(data = plot_data, aes(x = F_5, y = year, color = model_group), size = 4, show.legend = TRUE) +
  facet_grid(iteration~scenario) 
p


# 1. Prepare data for segments (one row per year/scenario with both OM and EM values)
dumbbell_data <- plot_data %>%
  filter(
    model_run %in% key_models, 
    iteration == 1, 
    scenario %in% c("flat_x_flat_rt_2"), 
    year %in% 2000:2068
  ) %>%
  select(year, scenario, iteration, model_group, F_5) %>%
  pivot_wider(names_from = model_group, values_from = F_5)

# 2. Filter original long data for the points
points_data <- plot_data %>%
  filter(
    model_run %in% key_models, 
    iteration == 1, 
    scenario %in% c("flat_x_flat_rt_2"), 
    year %in% 2000:2068
  )

# 3. Plot (Year on X-axis, F_5 on Y-axis)
ggplot() +
  # Draw vertical segments connecting OM to EM for each year
  geom_segment(
    data = dumbbell_data,
    aes(x = year, xend = year, y = OM, yend = EM),
    color = "#aeb6bf",
    linewidth = 1.5,
    alpha = 0.5
  ) +
  # Draw points on top
  geom_point(
    data = points_data, 
    aes(x = year, y = F_5, color = model_group), 
    size = 3
  ) +
  facet_wrap( ~ scenario, ncol = 1) +
  theme_bw()


# Comparison Plots --------------------------------------------------

# Load other set of results for comparison plot

summary_rec_devs <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary_new_rec_dev_fix_backup.rda")))

summary_rec_devs$ts <- summary_rec_devs$ts %>%
  filter(model_run != "", !str_detect(model_run, "Base")) %>% #remove "Base" model 
  mutate(end_year = as.numeric(str_extract(model_run, "\\d{4}$")) + 3, 
         years_until_terminal = end_year - year) %>%
  filter(case_when(
    str_detect(model_run, "_EM") ~ years_until_terminal > 2,
    TRUE ~ TRUE # Keep all other rows if no _EM
  )) %>%
  filter(!is.na(scenario)) %>%
  separate_wider_regex(
    cols = scenario,
    patterns = c(
      om_name  = "^(?:old|mid|young|flat|no_rt)", # Added ?: here
      "_x_", 
      em_name  = "(?:old|mid|young|flat|no_rt)",  # Added ?: here
      exp_type = ".*"
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  # --- CLEANUP EXP_TYPE ---
  mutate(
    exp_type = str_remove(exp_type, "^_"),
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type)
  ) %>%
  mutate(Commercial = deadB_1 + deadB_2, Recreational = deadB_4)

summary_rec_devs$dq <- summary_rec_devs$dq %>%
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
      om_name  = "^(?:old|mid|young|flat|no_rt)", # Added ?: here
      "_x_", 
      em_name  = "(?:old|mid|young|flat|no_rt)",  # Added ?: here
      exp_type = ".*"
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  # --- CLEANUP EXP_TYPE ---
  mutate(
    exp_type = str_remove(exp_type, "^_"),
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type)
  )


summary_rec_devs$scalar <- summary_rec_devs$scalar %>%
  filter(model_run != "", !str_detect(model_run, "Base")) %>%
  filter(!is.na(scenario)) %>%
  separate_wider_regex(
    cols = scenario,
    patterns = c(
      om_name  = "^(?:old|mid|young|flat|no_rt)", # Added ?: here
      "_x_", 
      em_name  = "(?:old|mid|young|flat|no_rt)",  # Added ?: here
      exp_type = ".*"
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  # --- CLEANUP EXP_TYPE ---
  mutate(
    exp_type = str_remove(exp_type, "^_"),
    exp_type = if_else(str_detect(exp_type, "^\\d+$"), str_c("rt_", exp_type), exp_type)
  ) 

#OM Data
plot_median_ts_om_lines(summary_rec_devs$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")+ geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - Known Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 
plot_median_ts_om_lines(summary_rec_devs$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")+ geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - All Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme(
    text = element_text(size = 14),    
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) 

