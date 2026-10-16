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

#name of the results files
results_name <- "_selectivity"

#create a list of scenarios for plot generation, usually the default order is fine.  
#scen_list <- unique(summary$ts$scenario)
#hard coded in a specific order.  
scen_list <- c(
  "rt_2_x_rt_2",
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

#rename the OM if it's different from default
OM_name <- "default_sigmaR"

OM_runs <- summary$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary$ts %>%
  filter(str_detect(model_run, "EM"))

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

residual_runs_prop %>% 
  filter(year %in% seq(2017, 2022, 1)) %>%
  mutate(om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario 
  group_by(em_name) %>%
  reframe(
    prop_com = (sum(res_com) / sum(com_om))*100,
    prop_rec = (sum(res_rec) / sum(deadB_4_om))*100,
    prop_red = (sum(res_dead_5) /  sum(deadB_5_om))*100,
    raw_total = (sum(res_com)/10+sum(res_rec)/10+sum(res_dead_5)/10),
    raw_prop = (sum(res_com)+sum(res_rec)+sum(res_dead_5)) /  (sum(com_om)+sum(deadB_4_om) + sum(deadB_5_om)) *100
  ) %>%
  arrange(em_name) %>%
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
