# Script to confirm the random_34 runs are working
# table that compares the red tide years between the OM and EM
# plots of 10 of the iterations OM and EM over time

# Set-up ------------------------------------------------------------------

library(tidyverse)
library("nmfspalette")
library(patchwork)
library(knitr)
library(kableExtra)
# OM locations
#name of the first OM
OM_name <- "default_sigmaR"
model_SSMSE_dir <- file.path("base_models")
default <- file.path(model_SSMSE_dir, OM_name)
run_SSMSE_dir <- file.path("runs_output")

#rename the OM if it's different from default
OM_name <- "default_sigmaR"

#the model_run you want to use, typically the final model_run
end_yr <- 2068
niter <- 100

#name of the results files
results_name <- "_new_rec_dev_fix_backup"

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
  "old_x_no_rt"
)
scen_list <- c(
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


OM_runs <- summary$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary$ts %>%
  filter(str_detect(model_run, "EM"))

OM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "OM"))

EM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "EM"))


# F_5 Time Series by iteration --------------------------------------------
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

scen_in_order_oms <- c(
  "flat_x_no_rt",
  "young_x_no_rt",
  "mid_x_no_rt",
  "old_x_no_rt"
  )
scen_in_order_all_yrs<- c(
  "no_rt_x_no_rt",
  "no_rt_x_flat_all_yrs",
  "no_rt_x_young_all_yrs",
  "no_rt_x_mid_all_yrs",
  "no_rt_x_old_all_yrs"
)

scen_in_order_rt_17<- c(
  "no_rt_x_no_rt",
  "no_rt_x_flat_rt_17",
  "no_rt_x_young_rt_17",
  "no_rt_x_mid_rt_17",
  "no_rt_x_old_rt_17"
)

plot_data %>% 
  filter(model_run %in% key_models, iteration %in% 1:10, scenario %in% scen_list[1:5], year %in% 2000:2047) %>% #filters to just OM and max year runs
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
  filter(model_run %in% key_models, iteration %in% 1:10, scenario %in% scen_in_order_all_yrs, year %in% 2000:2047) %>% #filters to just OM and max year runs
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
  filter(model_run %in% key_models, iteration %in% 1:10, scenario %in% scen_in_order_rt_17, year %in% 2000:2047) %>% #filters to just OM and max year runs
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
  filter(model_run %in% key_models, iteration %in% 1:5, scenario %in% scen_in_order_flat, year %in% 2000:2117) %>% #filters to just OM and max year runs
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

# Check matching years OM/EM ----------------------------------------------


years_list <- summary$ts %>% 
  filter(model_run %in% key_models, year > 2017, F_5 > 0) %>%
  group_by(scenario, model_run, iteration) %>%
  arrange(year) %>% 
  reframe(
    years_used = paste(unique(year), collapse = ", "),
    .groups = "drop"
  )

summary$ts %>%
  filter(model_run %in% key_models, year > 2017, F_5 > 0, scenario %in% scen_in_order_rt_17) %>% # just EM and OM, simulation years, and have red tide mortality
  group_by(scenario, model_run, iteration) %>%
  reframe(
    years_list = paste(sort(unique(year)), collapse = ",") # unique years with commas
  ) %>%
  pivot_wider( # make table with scenarios, models, and listed years
    names_from = model_run, 
    values_from = years_list)
  mutate( # check if EM and OM year lists match up
    is_equal = `flat_EM_2068` == `flat_OM` 
  )%>%
  kable(
    # Rename columns directly within kable
    col.names = c("Scenario", "Iteration", "EM", "OM", "Is Equal?"),
    align = c("l","c", "l", "l", "c") # Align columns (left, center, center, center)
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )


# Median/Mean F_5 ---------------------------------------------------------

# median/mean F_5 for each combo
summary$ts %>%
  filter(model_run %in% key_models, year > 2017, F_5 > 0) %>% # just EM and OM, simulation years, and have red tide mortality
  group_by(scenario, model_run) %>%
  reframe(
    mean_F_5 = mean(F_5),
    sd_F_5 = sd(F_5),
    median_F_5 = median(F_5)
  ) %>%
  arrange(scenario) %>%
  kable(
    # Rename columns directly within kable
    col.names = c("Scenario", "Model Run",  "Mean", "Standard Dev", "Median"),
    align = c("l","l", "c", "c", "c") # Align columns (left, center, center, center)
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )

# median/mean F_5 for each combo, short term
summary$ts %>%
  filter(model_run %in% key_models, year > 2017, year <= 2022, F_5 > 0) %>% # just EM and OM, simulation years, and have red tide mortality
  group_by(scenario, model_run, iteration) %>%
  reframe(
    mean_F_5 = mean(F_5),
    sd_F_5 = sd(F_5),
    median_F_5 = median(F_5)
  ) %>%
  arrange(scenario, iteration) %>%
  kable(
    # Rename columns directly within kable
    col.names = c("Scenario", "Model Run", "Iteration", "Mean", "Standard Dev", "Median"),
    align = c("l","l", "c", "c", "c", "c") # Align columns (left, center, center, center)
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    # Add bootstrap styling
    full_width = FALSE # Don't stretch table to full page width
  )


# Plots of Mean Comparisons -----------------------------------------------

mean_comparison <- summary$ts %>%
  filter(model_run %in% key_models, year > 2017, F_5 > 0) %>% # just EM and OM, simulation years, and have red tide mortality
  group_by(scenario, model_run, iteration) %>%
  reframe(
    mean_F_5 = mean(F_5),
    sd_F_5 = sd(F_5),
    median_F_5 = median(F_5)
  ) %>%
  pivot_wider(
    names_from = model_run,
    values_from = c(mean_F_5, sd_F_5, median_F_5),
    names_glue = "{model_run}_{.value}"
  ) %>%
  # Optional: Clean up the long names for easier plotting
  rename_with(~ str_replace(., "default_sigmaR_EM_2047", "EM"), contains("EM")) %>%
  rename_with(~ str_replace(., "default_sigmaR_OM", "OM"), contains("OM"))
  
# These should be linear if the OM and EMs are matching well?  
mean_comparison %>%
  ggplot(aes(OM_mean_F_5, EM_mean_F_5)) +
  geom_point()+
  facet_wrap(~scenario)

mean_comparison %>%
  ggplot(aes(OM_median_F_5, EM_median_F_5)) +
  geom_point()+
  facet_wrap(~scenario)

comparison_f_5 <- summary$ts %>%
  filter(model_run %in% key_models, year > 2017, F_5 > 0) %>% # just EM and OM, simulation years, and have red tide mortality
  group_by(scenario, model_run, iteration, year) %>%
  pivot_wider(
    names_from = model_run,
    values_from = c(F_5),
    names_glue = "{model_run}_{.value}"
  ) %>%
  # Optional: Clean up the long names for easier plotting
  rename_with(~ str_replace(., "default_sigmaR_EM_2047", "EM"), contains("EM")) %>%
  rename_with(~ str_replace(., "default_sigmaR_OM", "OM"), contains("OM")) %>%
  ggplot(aes(OM_F_5, EM_F_5)) +
  geom_point()+
  facet_wrap(~scenario)
comparison_f_5
