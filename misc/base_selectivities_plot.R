# Script to make selectivity plots using a results folder
# Currently requires the results_summary_selectivity.rda 

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
scen_list <- unique(summary$ts$scenario)

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
  mutate(model_run = str_remove(model_run, "_OM"), age = age-1) %>%
  distinct()

base_selectivities %>%
  mutate(selectivity_corrected = 1/(1+exp(-selectivity))) %>%
  ggplot(aes(age, selectivity_corrected)) +
  geom_line() +
  geom_point() +
  theme_bw() + 
  ggtitle("Red tide selectivity at Age") +
  ylab("selectivity")+
  facet_wrap(~model_run)

summary$scalar %>% 
  mutate(model_run_year = str_extract(model_run, "\\d+")) %>% #extract year from model_run
  ggplot(aes(model_run_year, max_grad))+
  geom_point() + 
  facet_wrap(~scenario) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

bad_grads <- summary$scalar %>%
  filter(max_grad>1) %>% 
  select(iteration, model_run, scenario)
bad_grads

names(summary$dq)
dq_f_2017_2047 <- summary$dq %>%
  mutate(model_run_year = str_extract(model_run, "\\d+")) %>% #extract year from model_run
  filter(model_run_year == 2047, year > 2017) %>%
  ggplot(aes(scenario, Value.F)) + 
  geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
dq_f_2017_2047

dq_f_2017_2022 <- summary$dq %>%
  mutate(model_run_year = str_extract(model_run, "\\d+")) %>% #extract year from model_run
  filter(model_run_year == 2047, year > 2017, year <= 2022) %>%
  ggplot(aes(scenario, Value.F)) + 
  geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
dq_f_2017_2022

dq_f_2017_2022 <- summary$dq %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  ggplot(aes(em_name, Value.F, fill = em_name)) + 
  geom_boxplot()+
  facet_wrap(~om_name, scales = "free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Total F from 2017-2022 for each scenario, EM_2047")
dq_f_2017_2022


ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
    mutate(fleet_name = fct_recode(as.character(fleet),
                                   "Rec"      = "4",
                                   "Com" = "6",
                                   "Red Tide"      = "5"
                                   
    )) %>%
  ggplot(aes(fleet_name, deadB, fill = em_name)) + 
  geom_boxplot()+
  facet_wrap(~om_name, scales = "free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022


ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
                                 
  )) %>%
  ggplot(aes(om_name, deadB, fill = fleet_name)) + 
  geom_boxplot()+
  facet_wrap(~em_name, scales = "free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(em_name, deadB, fill = fleet_name)) + 
  geom_boxplot()+
  facet_wrap(~om_name, scales = "free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("Total F from 2017-2022 for each scenario, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = fleet_name)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = fleet_name, color = fleet_name)) +  
  facet_grid(em_name~om_name)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022


summary$ts %>% mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
                 om_name = paste0(str_extract(scenario, "\\w+(?=_x_)"), "_om"), 
                 em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(is.na(model_run_year), year > 2017, scenario != "rt_2_x_rt_2", iteration == 1) %>%
  ggplot(aes(year, SpawnBio)) + geom_line() +
  facet_grid(em_name~om_name)
  
summary$ts %>% mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
                      om_name = paste0(str_extract(scenario, "\\w+(?=_x_)"), "_om"), 
                      em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(is.na(model_run_year)|model_run_year == 2047, year >= 2017, year <=2022, scenario != "rt_2_x_rt_2", iteration == 1) %>%
  ggplot(aes(year, SpawnBio, color = model_run_year)) + geom_line() +
  facet_grid(em_name~om_name)

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = em_name)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = scenario, color = em_name)) +  
  facet_wrap(~fleet_name)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022


ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = em_name)) + 
  geom_boxplot() +
  facet_grid(om_name~fleet_name)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = em_name)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = scenario, color = em_name)) +  
  facet_grid(fleet_name~om_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = em_name)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = scenario, color = em_name)) +  
  facet_grid(fleet_name~om_name)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Red Tide"      = "5",
                                 "Com" = "6"
  )) %>%
  ggplot(aes(factor(year), deadB, color = em_name)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = em_name, color = em_name)) +  
  facet_grid(~fleet_name)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ref_points <- summary$scalar %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047) 
ref_points_plot <- ref_points %>%
  mutate(ssb_spr = SSB_Unfished/SSB_SPR) %>%
  ggplot(aes(scenario, ssb_spr)) +
  geom_boxplot() +
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + ggtitle("SSB/SSB_SPR for each scenario by EM name, 2047")
ref_points_plot

ref_points <- summary$scalar %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2026) 
ref_points_plot <- ref_points %>%
  mutate(ssb_spr = SSB_Unfished/SSB_SPR) %>%
  ggplot(aes(scenario, ssb_spr)) +
  geom_boxplot() +
  facet_grid(~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + ggtitle("SSB/SSB_SPR for each scenario by EM name, 2026")
ref_points_plot


ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  ggplot(aes(scenario, deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  filter(deadB_5>0) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  filter(!(fleet_name == "Red Tide" & deadB == 0)) %>%
  ggplot(aes(scenario, deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

ssb_spr <- summary$scalar %>%
  filter(str_detect(model_run, "EM_2047")) %>%
  select(scenario, iteration, SSB_SPR)

ssb_spr %>%
  mutate( #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  ggplot(aes(scenario, SSB_SPR)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")



summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2040, str_detect(model_run, "_OM")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  
  
summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2026, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  

SSB_SPR_EM_2047 <- summary$ts %>%
  left_join(summary$scalar, join_by(model_run, scenario, iteration)) %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(str_detect(model_run, "EM_2047")) %>% 
  mutate(SSB_SPR_yr = SpawnBio/SSB_SPR)

SSB_SPR_EM_2047 %>%
  filter(year == 2023) %>%
  ggplot(aes(scenario, SSB_SPR_yr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSByear/SSB_SPR, 2023, EM_2047")

SSB_SPR_EM_2047 %>%
  filter(year == 2026) %>%
  ggplot(aes(scenario, SSB_SPR_yr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSByear/SSB_SPR, 2026, EM_2047")

SSB_SPR_EM_2047 %>%
  filter(year == 2047) %>%
  ggplot(aes(scenario, SSB_SPR_yr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSByear/SSB_SPR, 2047, EM_2047")

SSB_SPR_EM_2047 %>%
  ggplot(aes(scenario, SSB_SPR_yr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSByear/SSB_SPR, All years, EM_2047")

SSB_SPR_EM_2047 %>%
  filter(year == 2047) %>%
  ggplot(aes(scenario, SSB_SPR_yr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSByear/SSB_SPR, 2047, EM_2047")

### SSB

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSB, All years, EM_2047") 

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2023, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("SSB, 2023, EM_2047")

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2026, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("SSB, 2026, EM_2047")

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2047, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, SpawnBio)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("SSB, 2047, EM_2047")


summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year > 2017, year <= 2022, str_detect(model_run, "EM_2047")) %>% 
  group_by(scenario, iteration, em_name, om_name) %>%
  reframe(sum_ssb = sum(SpawnBio)) %>%
  ggplot(aes(om_name, sum_ssb)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free") + 
  ggtitle("SSB, 2017-2022, EM_2047") 

### Rec and Com Catch

# ts_f_2017_2022 <- summary$ts %>%
#   mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
#          om_name = str_extract(scenario, "\\w+(?=_x_)"), 
#          em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
#   filter(model_run_year == 2047, year > 2017, year <= 2022, scenario != "rt_2_x_rt_2") %>%
#   mutate(deadB_6 = deadB_1+deadB_2) %>%
#   filter(deadB_5>0) %>%
#   pivot_longer(
#     cols = starts_with("deadB_"), 
#     names_to = "fleet", 
#     names_pattern = "deadB_(\\d+)",
#     values_to = "deadB"
#   ) %>%
#   filter(fleet %in% c(4, 5, 6)) %>%
#   mutate(fleet_name = fct_recode(as.character(fleet),
#                                  "Rec"      = "4",
#                                  "Com" = "6",
#                                  "Red Tide"      = "5"
#   )) %>%
#   filter(!(fleet_name == "Red Tide" & deadB == 0), fleet_name != "Red Tide") %>%
#   ggplot(aes(scenario, deadB)) + 
#   geom_boxplot()+
#   facet_grid(fleet_name~em_name, scales = "free")+
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
#   ) + 
#   ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
# ts_f_2017_2022


ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year == 2023, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  filter(!(fleet_name == "Red Tide" & deadB == 0), fleet_name != "Red Tide") %>%
  ggplot(aes(scenario, deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2023 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year == 2026, scenario != "rt_2_x_rt_2") %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  filter(!(fleet_name == "Red Tide" & deadB == 0), fleet_name != "Red Tide") %>%
  ggplot(aes(scenario, deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2026 for each fleet, EM_2047")
ts_f_2017_2022

ts_f_2017_2022 <- summary$ts %>%
  mutate(model_run_year = str_extract(model_run, "\\d+"), #extract year from model_run
         om_name = str_extract(scenario, "\\w+(?=_x_)"), 
         em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(model_run_year == 2047, year == 2047) %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  filter(!(fleet_name == "Red Tide" & deadB == 0), fleet_name != "Red Tide") %>%
  ggplot(aes(scenario, deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name~em_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2047 for each fleet, EM_2047")
ts_f_2017_2022

# sum deadB

ts_f_2017_2022 <- summary$ts %>%
 filter(str_detect(model_run, 2047), year > 2017, year <= 2022) %>%
  mutate(deadB_6 = deadB_1+deadB_2) %>%
  pivot_longer(
    cols = starts_with("deadB_"), 
    names_to = "fleet", 
    names_pattern = "deadB_(\\d+)",
    values_to = "deadB"
  ) %>%
  filter(fleet %in% c(4, 5, 6)) %>%
  mutate(fleet_name = fct_recode(as.character(fleet),
                                 "Rec"      = "4",
                                 "Com" = "6",
                                 "Red Tide"      = "5"
  )) %>%
  group_by(scenario, fleet_name, iteration) %>%
  reframe(sum_deadB = sum(deadB)) %>%
  filter(!(fleet_name == "Red Tide" & sum_deadB == 0), fleet_name != "Red Tide") %>%
  ggplot(aes(scenario, sum_deadB)) + 
  geom_boxplot()+
  facet_grid(fleet_name, scales = "free")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  ggtitle("DeadB from 2017-2022 for each fleet, EM_2047")
ts_f_2017_2022

### recruitment

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2023, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, Recruit_0)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("Recruitment, 2023, EM_2047")

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2026, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, Recruit_0)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("Recruitment, 2026, EM_2047")

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year == 2047, str_detect(model_run, "EM_2047")) %>% 
  ggplot(aes(scenario, Recruit_0)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("Recruitment, 2047, EM_2047")

summary$ts %>%
  mutate( #extract year from model_run
    om_name = str_extract(scenario, "\\w+(?=_x_)"), 
    em_name = str_extract(scenario, "(?<=_x_).*?(?=_all_yrs|_rt_2)")) %>% #extract om from scenario
  filter(year > 2017, year <= 2022, str_detect(model_run, "EM_2047")) %>% 
  group_by(scenario, iteration, em_name, om_name) %>%
  reframe(sum_rec = sum(Recruit_0)) %>%
  ggplot(aes(scenario, sum_rec)) + geom_boxplot()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  facet_grid(~em_name, scales = "free")  + 
  ggtitle("Recruitment, 2017-2022, EM_2047")

