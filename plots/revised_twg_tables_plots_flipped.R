# This script was used to test generating plots for the SEDAR105 TWG
# This version uses the selectivity_100_redo, but flips the OM and EM axes


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
results_name <- "_selectivity_100_redo_2"
n_iterations <- 100
min_year <- 2018
max_year <- 2047
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
  )

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
  )
summary$scalar <- summary$scalar %>%
  filter(model_run != "", !str_detect(model_run, "Base")) 

#rename the OM if it's different from default
OM_name <- "default_sigmaR"

core_4 <- c(  "no_rt",
              "no_rt_x_rt_2",
              "rt_2_x_no_rt",
              "rt_2_x_rt_2")
all_years <- c("no_rt_x_all_yrs",
               "rep_3_x_all_yrs",
               "rt_2_x_all_yrs")
selectivity_rt_2 <- c("flat_x_flat_rt_2",
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
                      "mid_x_old_rt_2")
selectivity_all_yrs <-c("flat_x_flat_all_yrs",
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
                        "mid_x_old_all_yrs")

OM_runs <- summary$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary$ts %>%
  filter(str_detect(model_run, "EM"))

OM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "OM"))

EM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "EM"))

# Timeseries plots --------------------------------------------------------

## mean F_5 over time - pulled from qmd

# prepare data for stat of variable over time plots
# create a data frame of OM means, medians, and sds by year and scenario
OM_lines <- OM_runs %>%
  group_by(year, scenario) %>%
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
  group_by(year, scenario) %>%
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

#### Core 4

combined_lines %>% 
  filter(scenario %in% core_4) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - Core 4") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,"plots", "flipped", "ts_mean_F_5_core.png"),
         width = 8, height = 6, units = "in", device = "png")
}

#### All Years

combined_lines %>% 
  filter(scenario %in% all_years) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - All Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,"plots", "flipped", "ts_mean_F_5_all_yrs.png"),
         width = 8, height = 3, units = "in", device = "png")
}

#### Selectivity

just_matching_selectivity_rt_2 <- c("flat_x_flat_rt_2",
                                    "young_x_young_rt_2",
                                    "old_x_old_rt_2",
                                    "mid_x_mid_rt_2")

combined_lines %>% 
  filter(scenario %in% just_matching_selectivity_rt_2) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - 2 Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,"plots", "flipped", "ts_mean_F_5_sel_rt_2.png"),
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
  ggsave(file.path(run_SSMSE_dir,"plots", "flipped", "ts_mean_F_5_sel_all_yrs.png"),
         width = 8, height = 6, units = "in", device = "png")
}


# Error Tables ------------------------------------------------------------

## error table - for all 4 experiments separately

residual_runs_prop <- EM_runs %>%
  filter(
    str_detect(model_run, "_EM_2047")) %>%
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

create_residual_kable <- function(min_year, max_year, scenario_list) {
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

kable_core <- create_residual_kable(min_year, max_year_short_term, core_4)
kable_core

if(save == TRUE){
  save_kable(kable_core, file = file.path(run_SSMSE_dir,"plots", "flipped","core_kable.html"))
}

#### All Years

kable_all_yrs <- create_residual_kable(min_year, max_year_short_term, all_years)
kable_all_yrs

if(save == TRUE){
  save_kable(kable_all_yrs, file = file.path(run_SSMSE_dir,"plots", "flipped","all_years_kable.html"))
}

#### Selectivity rt_2

kable_sel_rt_2 <- create_residual_kable(min_year, max_year_short_term, selectivity_rt_2)
kable_sel_rt_2

if(save == TRUE){
  save_kable(kable_sel_rt_2, file = file.path(run_SSMSE_dir,"plots", "flipped","sel_rt_2_kable.html"))
}

#### Selectivity all_yrs

kable_sel_all_yrs <- create_residual_kable(min_year, max_year_short_term, selectivity_all_yrs)
kable_sel_all_yrs

if(save == TRUE){
  save_kable(kable_sel_all_yrs, file = file.path(run_SSMSE_dir,"plots", "flipped","sel_all_yrs_kable.html"))
}

# Long Term Error Tables --------------------------------------------------

#### Core 4

kable_core <- create_residual_kable(min_year, max_year, core_4)
kable_core

if(save == TRUE){
  save_kable(kable_core, file = file.path(run_SSMSE_dir,"plots", "flipped","core_longterm_kable.html"))
}

#### All Years

kable_all_yrs <- create_residual_kable(min_year, max_year, all_years)
kable_all_yrs

if(save == TRUE){
  save_kable(kable_all_yrs, file = file.path(run_SSMSE_dir,"plots", "flipped","all_years_longterm_kable.html"))
}

#### Selectivity rt_2

kable_sel_rt_2 <- create_residual_kable(min_year, max_year, selectivity_rt_2)
kable_sel_rt_2

if(save == TRUE){
  save_kable(kable_sel_rt_2, file = file.path(run_SSMSE_dir,"plots", "flipped","sel_rt_2_longterm_kable.html"))
}

#### Selectivity all_yrs

kable_sel_all_yrs <- create_residual_kable(min_year, max_year, selectivity_all_yrs)
kable_sel_all_yrs

if(save == TRUE){
  save_kable(kable_sel_all_yrs, file = file.path(run_SSMSE_dir,"plots", "flipped","sel_all_yrs_longterm_kable.html"))
}

# Rec and Com removal plots -------------------------------

## Rec and Com Catch

# model_run_type = the end year of the model run (i.e. "2047") or "_OM"
create_reccom_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- summary$ts %>% filter(scenario == "rt_2_x_rt_2", str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
    mutate(deadB_6 = deadB_1+deadB_2) %>%
    pivot_longer(
      cols = starts_with("deadB_"), 
      names_to = "fleet", 
      names_pattern = "deadB_(\\d+)",
      values_to = "deadB"
    ) %>%
    filter(fleet %in% c(4, 6)) %>%
    mutate(fleet_name = fct_recode(as.character(fleet),
                                   "recreational"      = "4",
                                   "commercial" = "6"
    )) %>%
    group_by(fleet_name, iteration) %>%
    reframe(mean_deadB = mean(deadB)) %>%
    group_by(fleet_name) %>%
    reframe(median_deadB = median(mean_deadB))
  
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type)
  
  summary$ts %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
    mutate(deadB_6 = deadB_1+deadB_2) %>%
    pivot_longer(
      cols = starts_with("deadB_"), 
      names_to = "fleet", 
      names_pattern = "deadB_(\\d+)",
      values_to = "deadB"
    ) %>%
    filter(fleet %in% c(4, 5, 6)) %>%
    mutate(fleet_name = fct_recode(as.character(fleet),
                                   "recreational"      = "4",
                                   "commercial" = "6",
                                   "Red Tide"      = "5"
    )) %>%
    group_by(scenario, fleet_name, iteration) %>%
    reframe(mean_deadB = mean(deadB)) %>%
    filter(fleet_name != "Red Tide") %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_deadB)) + 
    geom_boxplot()+
    facet_grid(fleet_name~om_name, scales = "free")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("DeadB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("Mean Removals (mt)") +
    geom_hline(data = median_rt_2, aes(yintercept = median_deadB), linetype = "dashed")
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir,"plots", "flipped", paste0(title, ".png")),
           width = 8, height = 6, units = "in", device = "png")
  }
}

create_reccom_plot_self_ref <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # Process the main dataset first so we can derive reference points from it
  plot_data <- summary$ts %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
    mutate(deadB_6 = deadB_1 + deadB_2) %>%
    pivot_longer(
      cols = starts_with("deadB_"), 
      names_to = "fleet", 
      names_pattern = "deadB_(\\d+)",
      values_to = "deadB"
    ) %>%
    filter(fleet %in% c(4, 5, 6)) %>%
    mutate(fleet_name = fct_recode(as.character(fleet),
                                   "recreational" = "4",
                                   "commercial"   = "6",
                                   "Red Tide"     = "5")) %>%
    group_by(scenario, fleet_name, iteration) %>%
    reframe(mean_deadB = mean(deadB)) %>%
    filter(fleet_name != "Red Tide") %>%
    mutate(
      om_name = if_else(scenario == "no_rt", "no_rt", 
                        if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), 
                                scenario, str_extract(scenario, "\\w+(?=_x_)"))),
      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
      em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference"))
    ) %>%
    # Apply factor levels
    mutate(
      om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
      em_name = fct_relevel(em_name, "reference", "all years", "young", "mid", "old", "flat")
    )
  
  # This finds the median of the case where OM == EM for every fleet/facet combo
  dynamic_medians <- plot_data %>%
    filter(as.character(om_name) == as.character(em_name)) %>%
    group_by(fleet_name, om_name) %>%
    summarise(median_deadB = median(mean_deadB), .groups = "drop")
  
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type)
  
  p <- ggplot(plot_data, aes(em_name, mean_deadB)) + 
    geom_boxplot() +
    # This line now maps to the dynamic_medians data frame
    geom_hline(data = dynamic_medians, aes(yintercept = median_deadB), linetype = "dashed", color = "blue") +
    facet_grid(fleet_name ~ om_name, scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(title = paste0("DeadB from ", min_year, "-", max_year, " for each fleet - ", scenario_name),
         x = "EM name", 
         y = "Mean Removals (mt)")
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title,"_self_ref", ".png")),
           plot = p, width = 8, height = 6, units = "in", device = "png")
  }
  
  return(p)
}
#### Core 4

create_reccom_plot(min_year, max_year_short_term, core_4, "Core 4")
create_reccom_plot(min_year, max_year_short_term, core_4, "Core 4", "_OM")

#### All Years

create_reccom_plot(min_year, max_year_short_term, all_years, "All years")
create_reccom_plot(min_year, max_year_short_term, all_years, "All years", "_OM")

#### Selectivity rt_2

create_reccom_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_reccom_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")

create_reccom_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_reccom_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")

#### Selectivity all_yrs

create_reccom_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_reccom_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")

create_reccom_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_reccom_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")

# Rec and Com OM/EM -------------------------------------------------------

create_reccom_om_em_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("com", "rec")) %>%
    mutate(type_name = fct_recode(
      type,
      "recreational" = "rec",
      "commercial" = "com"
    )) %>%
    group_by(type_name, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by(type_name) %>%
    reframe(median_om_em = median(mean_om_em))
  
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_OMEM")
  
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("com", "rec")) %>%
    mutate(type_name = fct_recode(
      type,
      "recreational" = "rec",
      "commercial" = "com"
    )) %>%
    group_by(scenario, type_name, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(type_name~om_name, scales = "free_x")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("DeadB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("OM:EM Ratio") +
    geom_hline(yintercept = 1, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
    #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")

  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 6, units = "in", device = "png")
  }
}

create_reccom_om_em_plot_self_ref <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # 1. Process the main OM:EM ratio dataset
  plot_data <- EM_runs %>%
    filter(
      scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt"),
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise() %>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om / commercial,
      res_rec = deadB_4_om / recreational
    ) %>%
    pivot_longer(
      cols = c(res_com, res_rec),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    mutate(type_name = fct_recode(type, "recreational" = "rec", "commercial" = "com")) %>%
    group_by(scenario, type_name, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(
      om_name = if_else(scenario == "no_rt", "no_rt", 
                        if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), 
                                scenario, str_extract(scenario, "\\w+(?=_x_)"))),
      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
      em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference"))
    ) %>%
    mutate(
      om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
      em_name = fct_relevel(em_name, "reference", "all years", "young", "mid", "old", "flat")
    )
  
  # 2. Create the dynamic reference data (where OM matches EM)
  dynamic_medians <- plot_data %>%
    filter(as.character(om_name) == as.character(em_name)) %>%
    group_by(type_name, om_name) %>%
    summarise(median_val = median(mean_om_em), .groups = "drop")
  
  # 3. Plotting
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_OMEM")
  
  p <- ggplot(plot_data, aes(em_name, mean_om_em)) + 
    geom_boxplot() +
    # Add the dynamic reference line
    geom_hline(data = dynamic_medians, aes(yintercept = median_val), 
               linetype = "dashed", color = "blue", alpha = 0.7) +
    #optional: Keep the y=1 line (perfect match) for context
    #geom_hline(yintercept = 1, color = "black", alpha = 0.5) +
    facet_grid(type_name ~ om_name, scales = "free_x") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(
      title = paste0("OM:EM Ratio from ", min_year, "-", max_year, " - ", scenario_name),
      x = "EM name", 
      y = "Mean OM:EM Ratio"
    )
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, "_self_ref.png")),
           plot = p, width = 8, height = 6, units = "in", device = "png")
  }
  
  return(p)
}

#### Core 4

create_reccom_om_em_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_reccom_om_em_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_reccom_om_em_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_reccom_om_em_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_reccom_om_em_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_reccom_om_em_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# DeadB Percent Bias -------------------------------------------------------

#EM-OM/OM

create_deadb_bias_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("com", "rec")) %>%
    mutate(type_name = fct_recode(
      type,
      "recreational" = "rec",
      "commercial" = "com"
    )) %>%
    group_by(type_name, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by(type_name) %>%
    reframe(median_om_em = median(mean_om_em))
  
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_Bias")
  
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("com", "rec")) %>%
    mutate(type_name = fct_recode(
      type,
      "recreational" = "rec",
      "commercial" = "com"
    )) %>%
    group_by(scenario, type_name, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(type_name~om_name, scales = "free_x")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("DeadB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("% Bias") +
    geom_hline(yintercept = 0, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
  #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 6, units = "in", device = "png")
  }
}

create_deadb_bias_plot_self_ref <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # 1. Process the main dataset
  plot_data <- EM_runs %>%
    filter(
      scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt"),
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise() %>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      com_om = sum(deadB_1_om, deadB_2_om),
      # Calculate % Bias: (EM - OM) / OM * 100
      res_com = (commercial - com_om) / com_om * 100,
      res_rec = (recreational - deadB_4_om) / deadB_4_om * 100
    ) %>%
    pivot_longer(
      cols = c(res_com, res_rec),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "bias_val"
    ) %>%
    mutate(type_name = fct_recode(type, "recreational" = "rec", "commercial" = "com")) %>%
    group_by(scenario, type_name, iteration) %>%
    reframe(mean_bias = mean(bias_val)) %>%
    mutate(
      om_name = if_else(scenario == "no_rt", "no_rt", 
                        if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), 
                                scenario, str_extract(scenario, "\\w+(?=_x_)"))),
      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
      em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference"))
    ) %>%
    mutate(
      om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
      em_name = fct_relevel(em_name, "reference", "all years", "young", "mid", "old", "flat")
    )
  
  # 2. Create the dynamic reference data (where OM == EM)
  dynamic_medians <- plot_data %>%
    filter(as.character(om_name) == as.character(em_name)) %>%
    group_by(type_name, om_name) %>%
    summarise(median_bias = median(mean_bias), .groups = "drop")
  
  # 3. Plotting
  title <- paste0("DeadB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_Bias")
  
  p <- ggplot(plot_data, aes(em_name, mean_bias)) + 
    geom_boxplot() +
    # Dynamic reference line (the median of the "correct" model configuration)
    geom_hline(data = dynamic_medians, aes(yintercept = median_bias), 
               linetype = "dashed", color = "blue") +
    # Zero line (Theoretical perfect accuracy)
    geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.3) +
    facet_grid(type_name ~ om_name, scales = "free_x") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(
      title = paste0("DeadB % Bias from ", min_year, "-", max_year, " - ", scenario_name),
      x = "EM name", 
      y = "% Bias"
    )
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, "self_ref.png")),
           plot = p, width = 8, height = 6, units = "in", device = "png")
  }
  
  return(p)
}

#### Core 4

create_deadb_bias_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_deadb_bias_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_deadb_bias_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_deadb_bias_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_deadb_bias_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_deadb_bias_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# Recruitment -------------------------------------------------------------

## Recruitment

create_recruitment_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  median_rt_2_rec <- summary$ts %>%
    filter(scenario == "rt_2_x_rt_2",
           str_detect(model_run, model_run_type),
           year >= min_year,
           year <= max_year) %>%
    group_by(iteration) %>%
    mutate(mean_recruits = mean(Recruit_0)) %>%
    ungroup() %>%
    reframe(median_recruits = median(mean_recruits))

  title <- paste0("Recruits_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type)
  
summary$ts %>%
  filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(Recruit_0)) %>%
  mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
         om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
         em_name = fct_relevel(em_name, "reference", "all years"),
         em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
  ggplot(aes(em_name, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~om_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle(paste0("Recruits from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("Mean Recruits (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")
if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
         width = 8, height = 3, units = "in", device = "png")
}

}

#### Core 4

create_recruitment_plot(min_year, max_year_short_term, core_4, "Core 4")
create_recruitment_plot(min_year, max_year_short_term, core_4, "Core 4 OM", "_OM")

#### All Years

create_recruitment_plot(min_year, max_year_short_term, all_years, "All years")
create_recruitment_plot(min_year, max_year_short_term, all_years, "All years", "_OM")

#### Selectivity rt_2

create_recruitment_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_recruitment_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")

#### Selectivity all_yrs

create_recruitment_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_recruitment_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")

# Recruitment OM/EM -------------------------------------------------------

create_recruit_om_em_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("Recruit_0")) %>%
    group_by(iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by() %>%
    reframe(median_om_em = median(mean_om_em))
  title <- paste0("Recruits_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_OMEM")
  
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("Recruit_0")) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(~om_name, scales = "free")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("Recruitment from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("OM:EM Ratio") +
    geom_hline(yintercept = 1, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
  #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 3, units = "in", device = "png")
  }
}

#### Core 4

create_recruit_om_em_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_recruit_om_em_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_recruit_om_em_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_recruit_om_em_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# Recruitment Percent Bias -------------------------------------------------------

#EM-OM/OM

create_recruitment_bias_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("Recruit_0")) %>%
    group_by(iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by() %>%
    reframe(median_om_em = median(mean_om_em))
  title <- paste0("recruitment_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_Bias")
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("Recruit_0")) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(~om_name, scales = "free")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("Recruitment from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("% Bias") +
    geom_hline(yintercept = 0, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
  #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 3, units = "in", device = "png")
  }
}

#### Core 4

create_recruitment_bias_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_recruitment_bias_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_recruitment_bias_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_recruitment_bias_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# SSB ---------------------------------------------------------------------

## SSB

#### Core 4

create_ssb_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {

median_rt_2_rec <- summary$ts %>% filter(scenario == "rt_2_x_rt_2", str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
  group_by(iteration) %>%
  mutate(mean_ssb = mean(SpawnBio)) %>%
  ungroup()%>%
  reframe(median_ssb = median(mean_ssb))

title <- paste0("SSB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type)
summary$ts %>%
  filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, model_run_type), year >= min_year, year <= max_year) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_ssb = mean(SpawnBio)) %>%
  mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
         om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
         em_name = fct_relevel(em_name, "reference", "all years"),
         em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
  ggplot(aes(em_name, mean_ssb)) + 
  geom_boxplot()+
  facet_grid(~om_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle(paste0("SSB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("Mean SSB (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_ssb), linetype = "dashed")

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
         width = 8, height = 3, units = "in", device = "png")
}
}

create_ssb_plot_self_ref <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # 1. Process the main SSB data
  plot_data <- summary$ts %>%
    filter(
      scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt"),
      str_detect(model_run, model_run_type), 
      year >= min_year, 
      year <= max_year
    ) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_ssb = mean(SpawnBio)) %>%
    mutate(
      om_name = if_else(scenario == "no_rt", "no_rt", 
                        if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), 
                                scenario, str_extract(scenario, "\\w+(?=_x_)"))),
      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
      em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference"))
    ) %>%
    mutate(
      om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
      em_name = fct_relevel(em_name, "reference", "all years", "young", "mid", "old", "flat")
    )
  
  # 2. Create the dynamic reference data (where OM == EM)
  # This provides a unique horizontal line for each facet column
  dynamic_medians <- plot_data %>%
    filter(as.character(om_name) == as.character(em_name)) %>%
    group_by(om_name) %>%
    summarise(median_ssb = median(mean_ssb), .groups = "drop")
  
  # 3. Plotting
  title <- paste0("SSB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type)
  
  p <- ggplot(plot_data, aes(em_name, mean_ssb)) + 
    geom_boxplot() +
    # Maps the dynamic line to each em_name facet
    geom_hline(data = dynamic_medians, aes(yintercept = median_ssb), 
               linetype = "dashed", color = "blue") +
    facet_grid(~om_name, scales = "free") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    labs(
      title = paste0("SSB from ", min_year, "-", max_year, " - ", scenario_name),
      x = "EM name", 
      y = "Mean SSB (mt)"
    )
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, "self_ref.png")),
           plot = p, width = 8, height = 3, units = "in", device = "png")
  }
  
  return(p)
}

#### Core 4

create_ssb_plot(min_year, max_year_short_term, core_4, "Core 4")
create_ssb_plot(min_year, max_year_short_term, core_4, "Core 4 OM", "_OM")

#### All Years

create_ssb_plot(min_year, max_year_short_term, all_years, "All years")
create_ssb_plot(min_year, max_year_short_term, all_years, "All years", "_OM")

#### Selectivity rt_2

create_ssb_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_ssb_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")

create_ssb_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_ssb_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")

#### Selectivity all_yrs

create_ssb_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_ssb_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")

create_ssb_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_ssb_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")

# SSB OM/EM -------------------------------------------------------

create_ssb_om_em_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("SpawnBio")) %>%
    group_by(iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by() %>%
    reframe(median_om_em = median(mean_om_em))
  title <- paste0("SSB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_OMEM")
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = Recruit_0_om/Recruit_0_em,
      res_SpawnBio = SpawnBio_om/SpawnBio_em,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = com_om/commercial,
      res_rec = deadB_4_om/recreational,
      res_dead_5 = deadB_5_om/deadB_5_em,
      res_abundance = Bio_smry_om/Bio_smry_em
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("SpawnBio")) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(~om_name, scales = "free")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("SSB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("OM:EM Ratio") +
    geom_hline(yintercept = 1, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
  #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 3, units = "in", device = "png")
  }
}

create_ssb_om_em_plot_self_ref <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # 1. Process the main dataset for SSB OM:EM ratios
  plot_data <- EM_runs %>%
    filter(
      scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt"),
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(res_SpawnBio = SpawnBio_om / SpawnBio_em) %>%
    ungroup() %>%
    filter(!is.na(res_SpawnBio)) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_om_em = mean(res_SpawnBio)) %>%
    mutate(
      om_name = if_else(scenario == "no_rt", "no_rt", 
                        if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), 
                                scenario, str_extract(scenario, "\\w+(?=_x_)"))),
      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
      em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference"))
    ) %>%
    mutate(
      om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
      em_name = fct_relevel(em_name, "reference", "all years", "young", "mid", "old", "flat")
    )
  
  # 2. Create the dynamic reference data (where OM == EM)
  dynamic_medians <- plot_data %>%
    filter(as.character(om_name) == as.character(em_name)) %>%
    group_by(om_name) %>%
    summarise(median_val = median(mean_om_em), .groups = "drop")
  
  # 3. Plotting
  title <- paste0("SSB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_OMEM")
  
  p <- ggplot(plot_data, aes(em_name, mean_om_em)) + 
    geom_boxplot() +
    # Dynamic reference line: the median ratio for the matched OM/EM scenario
    geom_hline(data = dynamic_medians, aes(yintercept = median_val), 
               linetype = "dashed", color = "blue") +
    # Horizontal line at 1 for reference (where OM matches EM perfectly)
    geom_hline(yintercept = 1, color = "black", alpha = 0.5) +
    facet_grid(~om_name, scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(
      title = paste0("SSB OM:EM Ratio from ", min_year, "-", max_year, " - ", scenario_name),
      x = "EM name", 
      y = "OM:EM Ratio"
    )
  
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, "self_ref.png")),
           plot = p, width = 8, height = 3, units = "in", device = "png")
  }
  
  return(p)
}

#### Core 4

create_ssb_om_em_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_ssb_om_em_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_ssb_om_em_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_ssb_om_em_plot_self_ref(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_ssb_om_em_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_ssb_om_em_plot_self_ref(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# SSB Percent Bias -------------------------------------------------------

#EM-OM/OM

create_ssb_bias_plot <- function (min_year, max_year, scenario_list, scenario_name, model_run_type = "2047") {
  
  # create the reference point from the "best" case scenario
  median_rt_2 <- EM_runs %>%
    filter(
      scenario == "rt_2_x_rt_2",
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("SpawnBio")) %>%
    group_by(iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    group_by() %>%
    reframe(median_om_em = median(mean_om_em))
  title <- paste0("SSB_", min_year, "_", max_year, "_", scenario_name, "_", model_run_type, "_Bias")
  EM_runs %>%
    filter(scenario %in% c(scenario_list, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
    filter(
      str_detect(model_run, model_run_type),
      year >= min_year,
      year <= max_year
    ) %>%
    rowwise()%>%
    mutate(commercial = sum(deadB_1, deadB_2), recreational = deadB_4) %>%
    left_join(OM_runs, by = c("year", "scenario", "iteration"), suffix = c("_em", "_om")) %>%
    group_by(scenario, iteration, year) %>%
    mutate(
      res_Recruit_0 = (Recruit_0_em-Recruit_0_om)/Recruit_0_om*100,
      res_SpawnBio = (SpawnBio_em -SpawnBio_om)/SpawnBio_om*100,
      com_om = sum(deadB_1_om, deadB_2_om),
      res_com = (commercial-com_om)/com_om*100,
      res_rec = (recreational-deadB_4_om)/deadB_4_om*100,
      res_dead_5 = (deadB_5_em-deadB_5_om)/deadB_5_om*100,
      res_abundance = (Bio_smry_em-Bio_smry_om)/Bio_smry_om*100
    ) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "type",
      names_pattern = "res_(.*)",
      values_to = "om_em"
    ) %>%
    filter(type %in% c("SpawnBio")) %>%
    group_by(scenario, iteration) %>%
    reframe(mean_om_em = mean(om_em)) %>%
    mutate(om_name = if_else(scenario == "no_rt", "no_rt", if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt", "rep_3"), scenario, str_extract(scenario, "\\w+(?=_x_)"))), 
           om_name = fct_relevel(om_name, "reference", "young", "mid", "old", "flat"),
           em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = if_else(scenario %in% all_years, "all years", replace_na(em_name, "reference")), 
           em_name = fct_relevel(em_name, "reference", "all years"),
           em_name = fct_relevel(em_name, "reference", "young", "mid", "old", "flat")) %>% 
    ggplot(aes(em_name, mean_om_em)) + 
    geom_boxplot()+
    facet_grid(~om_name, scales = "free")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) + 
    ggtitle(paste0("SSB from ", min_year, "-", max_year, " for each fleet - ", scenario_name)) + xlab("EM name") + ylab("% Bias") +
    geom_hline(yintercept = 0, linetype = "dashed") # switch if you want the median line of the rt_2_x_rt_2
  #geom_hline(data = median_rt_2, aes(yintercept = median_om_em), linetype = "dashed")
  if(save == TRUE){
    ggsave(file.path(run_SSMSE_dir, "plots", "flipped", paste0(title, ".png")),
           width = 8, height = 3, units = "in", device = "png")
  }
}

#### Core 4

create_ssb_bias_plot(min_year, max_year_short_term, core_4, "Core 4")

#### All Years

create_ssb_bias_plot(min_year, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_ssb_bias_plot(min_year, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_ssb_bias_plot(min_year, max_year_short_term, selectivity_all_yrs, "Selectivity all years")


# Terminal Year SSB Short-term -------------------------------------------------------

#### Core 4

create_ssb_plot(max_year_short_term, max_year_short_term, core_4, "Core 4")
create_ssb_plot(max_year_short_term, max_year_short_term, core_4, "Core 4 OM", "_OM")
create_ssb_bias_plot(max_year_short_term, max_year_short_term, core_4, "Core 4")

#### All Years

create_ssb_plot(max_year_short_term, max_year_short_term, all_years, "All years")
create_ssb_plot(max_year_short_term, max_year_short_term, all_years, "All years", "_OM")
create_ssb_bias_plot(max_year_short_term, max_year_short_term, all_years, "All years")

#### Selectivity rt_2

create_ssb_plot(max_year_short_term, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")
create_ssb_plot(max_year_short_term, max_year_short_term, selectivity_rt_2, "Selectivity 2 years", "_OM")
create_ssb_bias_plot(max_year_short_term, max_year_short_term, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_ssb_plot(max_year_short_term, max_year_short_term, selectivity_all_yrs, "Selectivity all years")
create_ssb_plot(max_year_short_term, max_year_short_term, selectivity_all_yrs, "Selectivity all years", "_OM")
create_ssb_bias_plot(max_year_short_term, max_year_short_term, selectivity_all_yrs, "Selectivity all years")

# Terminal Year SSB Short-term -------------------------------------------------------

#### Core 4

create_ssb_plot(max_year, max_year, core_4, "Core 4")
create_ssb_plot(max_year, max_year, core_4, "Core 4 OM", "_OM")
create_ssb_bias_plot(max_year, max_year, core_4, "Core 4")

#### All Years

create_ssb_plot(max_year, max_year, all_years, "All years")
create_ssb_plot(max_year, max_year, all_years, "All years", "_OM")
create_ssb_bias_plot(max_year, max_year, all_years, "All years")

#### Selectivity rt_2

create_ssb_plot(max_year, max_year, selectivity_rt_2, "Selectivity 2 years")
create_ssb_plot(max_year, max_year, selectivity_rt_2, "Selectivity 2 years", "_OM")
create_ssb_bias_plot(max_year, max_year, selectivity_rt_2, "Selectivity 2 years")

#### Selectivity all_yrs

create_ssb_plot(max_year, max_year, selectivity_all_yrs, "Selectivity all years")
create_ssb_plot(max_year, max_year, selectivity_all_yrs, "Selectivity all years", "_OM")
create_ssb_bias_plot(max_year, max_year, selectivity_all_yrs, "Selectivity all years")

