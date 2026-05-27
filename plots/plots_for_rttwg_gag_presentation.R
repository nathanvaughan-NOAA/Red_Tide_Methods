
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

#### All Years

combined_lines %>% 
  filter(scenario %in% all_years) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") + 
  ggtitle("Average red tide mortality over time - All Years") +
  theme_bw() + 
  xlab("Year") + ylab("Average Red Tide Mortality")

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

#### Core 4

residual_runs_prop %>% 
  filter(year %in% seq(2017, 2022, 1), scenario %in% core_4) %>%
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
    col.names = c("Scenario", "Commercial Catch Residual Sum (MT)", "Recreational Catch Residual Sum (MT)", "Red Tide Discards Residual Sum (MT)", "Total Removals Residual Sum (MT)", "Proportion of Residuals to Total (%)"),
    align = c("l", "c", "c", "c", "c", "c"), # Align columns (left, center, center, center)
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )

#### All Years

residual_runs_prop %>% 
  filter(year %in% seq(2017, 2022, 1), scenario %in% all_years) %>%
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
    col.names = c("Scenario", "Commercial Catch Residual Sum (MT)", "Recreational Catch Residual Sum (MT)", "Red Tide Discards Residual Sum (MT)", "Total Removals Residual Sum (MT)", "Proportion of Residuals to Total (%)"),
    align = c("l", "c", "c", "c", "c", "c"), # Align columns (left, center, center, center)
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )

#### Selectivity rt_2

residual_runs_prop %>% 
  filter(year %in% seq(2017, 2022, 1), scenario %in% selectivity_rt_2) %>%
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
    col.names = c("Scenario", "Commercial Catch Residual Sum (MT)", "Recreational Catch Residual Sum (MT)", "Red Tide Discards Residual Sum (MT)", "Total Removals Residual Sum (MT)", "Proportion of Residuals to Total (%)"),
    align = c("l", "c", "c", "c", "c", "c"), # Align columns (left, center, center, center)
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )

#### Selectivity all_yrs

residual_runs_prop %>% 
  filter(year %in% seq(2017, 2022, 1), scenario %in% selectivity_all_yrs) %>%
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
    col.names = c("Scenario", "Commercial Catch Residual Sum (MT)", "Recreational Catch Residual Sum (MT)", "Red Tide Discards Residual Sum (MT)", "Total Removals Residual Sum (MT)", "Proportion of Residuals to Total (%)"),
    align = c("l", "c", "c", "c", "c", "c"), # Align columns (left, center, center, center)
    digits = 2
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )


# Recreational and commercial removal plots -------------------------------

## Rec and Com Catch

#### Core 4
median_rt_2 <- summary$ts %>% filter(scenario == "rt_2_x_rt_2", str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
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

summary$ts %>%
  filter(scenario %in% c(core_4, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
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
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt"), scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(scenario, mean_deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet - Core 4") + xlab("OM name") + ylab("Mean Removals (mt)") +
  geom_hline(data = median_rt_2, aes(yintercept = median_deadB), linetype = "dashed")


#### All Years

summary$ts %>%
  filter(scenario %in% c(all_years, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
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
  mutate(em_name = if_else(scenario %in% all_years, "all years", "reference"), 
         em_name = fct_relevel(em_name, "reference", "all years")) %>% 
  ggplot(aes(scenario, mean_deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet - All years") + xlab("OM name") + ylab("Mean Removals (mt)") +
  geom_hline(data = median_rt_2, aes(yintercept = median_deadB), linetype = "dashed")


#### Selectivity rt_2

summary$ts %>%
  filter(scenario %in% c(selectivity_rt_2, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
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
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") == "rt_2", scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet - Selectivity 2 years") + xlab("OM name") + ylab("Mean Removals (mt)") +
  geom_hline(data = median_rt_2, aes(yintercept = median_deadB), linetype = "dashed")


#### Selectivity all_yrs

summary$ts %>%
  filter(scenario %in% c(selectivity_all_yrs, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
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
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") == "rt_2", scenario, str_extract(scenario, "\\w+(?=_x_)")), 
          em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
           em_name = replace_na(em_name, "reference"),
           em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet - Selectivity all years") + xlab("OM name") + ylab("Mean Removals (mt)") +
  geom_hline(data = median_rt_2, aes(yintercept = median_deadB), linetype = "dashed")


# Recruitment -------------------------------------------------------------

## Recruitment

#### Core 4

median_rt_2_rec <- summary$ts %>% filter(scenario == "rt_2_x_rt_2", str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(iteration) %>%
  mutate(mean_recruits = mean(Recruit_0)) %>%
  ungroup()%>%
  reframe(median_recruits = median(mean_recruits))

summary$ts %>%
  filter(scenario %in% c(core_4, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(Recruit_0)) %>%
  ggplot(aes(scenario, mean_recruits)) + 
  geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Recruits from 2017-2022 for each fleet - Core 4") + xlab("OM name") + ylab("Mean Recruits (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### All Years

summary$ts %>%
  filter(scenario %in% c(all_years, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(Recruit_0)) %>%
  mutate(em_name = if_else(scenario %in% all_years, "all years", "reference"), 
         em_name = fct_relevel(em_name, "reference", "all years")) %>%
  ggplot(aes(scenario, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Recruits from 2017-2022 for each fleet - All years") + xlab("OM name") + ylab("Mean Recruits (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### Selectivity rt_2
summary$ts %>%
  filter(scenario %in% c(selectivity_rt_2, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(Recruit_0)) %>%
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt"), scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Recruits from 2017-2022 for each fleet - Selectivity 2 years") + xlab("OM name") + ylab("Mean Recruits (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### Selectivity all_yrs

summary$ts %>%
  filter(scenario %in% c(selectivity_all_yrs, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(Recruit_0)) %>%
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt"), scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Recruits from 2017-2022 for each fleet - Selectivity all years") + xlab("OM name") + ylab("Mean Recruits (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")


# SSB ---------------------------------------------------------------------

## SSB

#### Core 4

median_rt_2_rec <- summary$ts %>% filter(scenario == "rt_2_x_rt_2", str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(iteration) %>%
  mutate(mean_recruits = mean(SpawnBio)) %>%
  ungroup()%>%
  reframe(median_recruits = median(mean_recruits))

summary$ts %>%
  filter(scenario %in% c(core_4, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(SpawnBio)) %>%
  ggplot(aes(scenario, mean_recruits)) + 
  geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("SSB from 2017-2022 for each fleet - Core 4") + xlab("OM name") + ylab("Mean SSB (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### All Years

summary$ts %>%
  filter(scenario %in% c(all_years, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(SpawnBio)) %>%
  mutate(em_name = if_else(scenario %in% all_years, "all years", "reference"), 
         em_name = fct_relevel(em_name, "reference", "all years")) %>%
  ggplot(aes(scenario, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("SSB from 2017-2022 for each fleet - All years") + xlab("OM name") + ylab("Mean SSB (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### Selectivity rt_2
summary$ts %>%
  filter(scenario %in% c(selectivity_rt_2, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(SpawnBio)) %>%
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt"), scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("SSB from 2017-2022 for each fleet - Selectivity 2 years") + xlab("OM name") + ylab("Mean SSB (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

#### Selectivity all_yrs

summary$ts %>%
  filter(scenario %in% c(selectivity_all_yrs, "rt_2_x_rt_2", "rt_2_x_no_rt")) %>%
  filter(str_detect(model_run, "2047"), year > 2017, year <= 2022) %>%
  group_by(scenario, iteration) %>%
  reframe(mean_recruits = mean(SpawnBio)) %>%
  mutate(om_name = if_else(str_extract(scenario, "\\w+(?=_x_)") %in% c("rt_2", "no_rt"), scenario, str_extract(scenario, "\\w+(?=_x_)")), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)"),
         em_name = replace_na(em_name, "reference"),
         em_name = fct_relevel(em_name, "reference", "flat", "young", "mid", "old")) %>% 
  ggplot(aes(om_name, mean_recruits)) + 
  geom_boxplot()+
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("SSB from 2017-2022 for each fleet - Selectivity all years") + xlab("OM name") + ylab("Mean SSB (mt)") +
  geom_hline(data = median_rt_2_rec, aes(yintercept = median_recruits), linetype = "dashed")

