# This script was used to test generating plots for the SEDAR105 TWG
# This version uses the _merged_no_rt data with all new scenarios
# Many boxplots were eliminated in favor of time series.  


# Set-up and get data -----------------------------------------------------

# Load packages 
library(tidyverse)
library(patchwork)
library(knitr)
library(kableExtra)
library(viridis)
library(grid)

# Location of the runs_output folder that will contain the .rda and folder for 
# plots to be saved to.  
run_SSMSE_dir <- file.path("runs_output")
plot_folder <- "red_tide_no_rt_fix_manuscript_options" # save the plots

# Name of the results files and input settings
results_name <- "_red_tide_no_rt_fix"  # .rda file name without "results_summary"
n_iterations <- 100 # number of iterations
min_year <- 2018 #min year analyzed (inclusive)
max_year <- 2068 #max year analyzed (inclusive)
model_run_selection <- 2068 # the model_run year you want to use for plots, typically the last year
max_year_short_term <- min_year + 4 # our shorter time period for error calculations
save <- TRUE # if you want to save the plots
plot_type <- ".png"  # the format of the saved plots: .pdf, etc.  
font_size <- 10
#pull the summary files, takes a few seconds.   
summary <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary", results_name, ".rda")))

#get a list of the scenarios and reorder if desired.  
scen_list <- unique(summary$ts$scenario)

# Establish overall theme
# Original from: https://fishr-core-team.github.io/fishR/blog/posts/2022-12-22_AFS_Style_Figures/index.html
theme_AFS <- function(base_size=10) {
  theme_classic(base_size=base_size) +
    theme(
      # modify plot title,the B in this case
      plot.title=element_text(family="Arial",face="bold"),
      # margin for the plot
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
      # set axis label (i.e., title) colors and margins
      axis.title.y=element_text(colour="black",margin=margin(t=0,r=10,b=0,l=0)),
      axis.title.x=element_text(colour="black",margin=margin(t=10,r=0,b=0,l=0)),
      # set tick label color, margin, and position and orientation
      axis.text.y=element_text(colour="black",margin=margin(t=0,r=5,b=0,l=0),
                               vjust=0.5,hjust=1),
      axis.text.x=element_text(colour="black",margin=margin(t=5,r=0,b=0,l=0),
                               vjust=0,hjust=0.5,),
      # set size of the tick marks for y- and x-axis
      axis.ticks=element_line(linewidth=0.5),
      # adjust length of the tick marks
      axis.ticks.length=unit(0.2,"cm"),
      # set the axis size,color,and end shape
      axis.line=element_line(colour="black",linewidth=0.5,lineend="square"),
      # adjust size of text for legend
      legend.text=element_text(size=10)
    )
}

# Filter the summary data -----------------------------------------------------
#   Remove "Base" model runs, remove the last 3 years of data of each model_run, 
#   remove any NA scenarios that aren't in the list above.  
#   Break up the scenario names in the following format:  
#         om_name (no_rt, flat, young, old, mid), 
#         em_name (no_rt, flat, young, old, mid), 
#         exp_type (all_yrs, rt_34, no_rt)
#   Add deadB_1 to deadB_2 to get Commercial DeadB, and relabel Recreational.   
summary$ts <- summary$ts %>%
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
      om_name  = "^(?:old|mid|young|flat|no_rt)", # pull these strings before _x_
      "_x_", 
      em_name  = "(?:old|mid|young|flat|no_rt)",  # pull these strings after _x_
      exp_type = ".*" # pull everything else (rt_, all_yrs, etc.)
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(  # Clean exp_type
    exp_type = str_remove(exp_type, "^_"), # get rid of the leading _
    exp_type = if_else(str_detect(exp_type, "rt"), "Known", exp_type),
    exp_type = if_else(exp_type == "", "No Years", exp_type),
    exp_type = if_else(exp_type %in% "all_yrs", "All Years", exp_type)
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
      om_name  = "^(?:old|mid|young|flat|no_rt)", # Added ?: here
      "_x_", 
      em_name  = "(?:old|mid|young|flat|no_rt)",  # Added ?: here
      exp_type = ".*"
    ),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(  # Clean exp_type
    exp_type = str_remove(exp_type, "^_"), # get rid of the leading _
    exp_type = if_else(str_detect(exp_type, "rt"), "Known", exp_type),
    exp_type = if_else(exp_type == "", "No Years", exp_type),
    exp_type = if_else(exp_type %in% "all_yrs", "All Years", exp_type)
  )


summary$scalar <- summary$scalar %>%
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
  mutate(  # Clean exp_type
    exp_type = str_remove(exp_type, "^_"), # get rid of the leading _
    exp_type = if_else(str_detect(exp_type, "rt"), "Known", exp_type),
    exp_type = if_else(exp_type == "", "No Years", exp_type),
    exp_type = if_else(exp_type %in% "all_yrs", "All Years", exp_type)
  )

# Remove bad gradients

bad_runs <- summary$scalar %>% 
  filter(max_grad > 1) %>%
  select(scenario, iteration) %>%
  distinct() 

summary$ts <- summary$ts %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))

summary$dq <- summary$dq %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))

summary$scalar <- summary$scalar %>%
  anti_join(bad_runs, by = c("scenario", "iteration"))


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

# filtered summary with just the OM or EM runs from the ts or dq.  
OM_runs <- summary$ts %>%
  filter(str_detect(model_run, "OM"))

EM_runs <- summary$ts %>%
  filter(str_detect(model_run, "EM"))

OM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "OM"))

EM_runs_dq <- summary$dq %>%
  filter(str_detect(model_run, "EM"))


# Figure X. Selectivity-at-age -----------------------------

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
  facet_wrap(~model_run) + xlab("Age") + ylab("Selectivity") +
  theme_AFS()

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Figure_x_selectivity.png"),
         width = 90, height = 90, units = "mm", dpi = 300)
}

# Figure X. Average red tide mortality over time -----------------------------

## mean F_5 over time

# prepare data for stat of variable over time plots
# create a data frame of OM means, medians, and sds by year and scenario
OM_lines <- OM_runs %>%
  filter(
    str_detect(model_run, as.character(model_run_selection)) | 
      str_detect(model_run, "_OM")
  ) %>%  
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
  ) %>% 
  mutate(model_type = "OM")

# create a data frame of EM means, medians, and sds by year and scenario
EM_lines <- EM_runs %>%
  filter(
    str_detect(model_run, as.character(model_run_selection)) | 
      str_detect(model_run, "_OM")
  ) %>%  
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
  ) %>% 
  mutate(model_type = "EM")

combined_lines <- rbind(OM_lines, EM_lines)

# Set the factor level order so EM drawn last (on top of plot)
combined_lines$model_type <- factor(
  combined_lines$model_type, 
  levels = c("OM", "EM") 
)

plot_variable_ts <- function(data = combined_lines,
                             variable = "deadB_5",
                             stat_type = "median",
                             years = c(2004, 2025)) {
  #combine the variable and stat_type names together for the y variable
  y_var_sym = sym(paste0(variable, "_", stat_type))
  
  ggplot(data, aes(
    x = year,
    y = !!y_var_sym,
    color = model_type
  )) +
    geom_line(aes(linetype = model_type)) +
    facet_wrap( ~ scenario) +
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


new_labels <- c("flat_x_flat_rt_2" = "flat x flat - known", 
                "flat_x_no_rt" = "flat x no rt",
                "no_rt_x_flat_rt_17" = "no rt x flat - known", 
                "no_rt_x_no_rt" = "no rt x no rt", 
                "no_rt_x_flat_all_yrs" = "no rt x flat - all", 
                "flat_x_flat_all_yrs" = "flat x flat - all")

combined_lines %>%
  filter(scenario %in% c("flat_x_flat_rt_2", "no_rt_x_no_rt", "flat_x_no_rt", "no_rt_x_flat_rt_17", "flat_x_flat_all_yrs", "no_rt_x_flat_all_yrs")) %>%
  mutate(scenario = factor(scenario, levels = c("no_rt_x_no_rt", "flat_x_flat_rt_2", "flat_x_flat_all_yrs", "flat_x_no_rt", "no_rt_x_flat_rt_17", "no_rt_x_flat_all_yrs"))) %>%
  plot_variable_ts(data = ., variable = "F_5", stat_type = "mean") +
  theme_bw() +
  xlab("Year") + ylab("Average Red Tide Mortality") +
  facet_wrap(~scenario, labeller = labeller(scenario = new_labels)) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  theme_AFS()

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Figure_x_average_red_tide_mortality_flat.png"),
         width = 6, height = 4, units = "in", device = "png")
}

# Figure X. Error by removal type -----------------------------

# Join EM_runs and OM_runs by year, scenario, and iteration to calculate error 
# between EM and OM
residual_runs_prop <- EM_runs %>%
  filter(
    str_detect(model_run, as.character(model_run_selection))) %>%
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
# Calculate the proportion error for each error type
all_errors <- residual_runs_prop %>% 
  filter(year %in% seq(min_year, max_year_short_term, 1)) %>%
  group_by(scenario) %>%
  reframe(
    prop_com = (sum(res_com) / sum(com_om))*100,
    prop_rec = (sum(res_rec) / sum(deadB_4_om))*100,
    prop_red = (sum(res_dead_5) /  sum(deadB_5_om))*100,
    raw_total = (sum(res_com)/n_iterations+sum(res_rec)/n_iterations+sum(res_dead_5)/n_iterations),
    raw_prop = (sum(res_com)+sum(res_rec)+sum(res_dead_5)) /  (sum(com_om)+sum(deadB_4_om) + sum(deadB_5_om)) *100
  )

# Add the om/em/exp_type naming conventions back in
all_errors <- all_errors %>%
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
  mutate(  # Clean exp_type
    exp_type = str_remove(exp_type, "^_"), # get rid of the leading _
    exp_type = if_else(str_detect(exp_type, "rt"), "Known", exp_type),
    exp_type = if_else(exp_type == "", "No Years", exp_type),
    exp_type = if_else(exp_type %in% "all_yrs", "All Years", exp_type)
  )

# Convert to longer data format for ggplot and relabel proportions
plot_data <- all_errors %>%
  pivot_longer(
    cols = c(prop_rec, prop_com, prop_red, raw_prop), 
    names_to = "error_type", 
    values_to = "error"
  ) %>%
  mutate(
    error_type = case_when(
      error_type == "prop_rec" ~ "Recreational",
      error_type == "prop_com" ~ "Commercial",
      error_type == "prop_red" ~ "Red Tide",
      error_type == "raw_prop" ~ "Total",
      TRUE ~ error_type
    )
  ) 

# Extract no_rt data and duplicate for each selectivity
no_rt_data_flat <- plot_data %>%
  filter(em_name == "no_rt", om_name != c("no_rt")) %>%
  mutate(em_name = "flat") 

no_rt_data_mid <- plot_data %>%
  filter(em_name == "no_rt", om_name != c("no_rt")) %>%
  mutate(em_name = "mid")

no_rt_data_old <- plot_data %>%
  filter(em_name == "no_rt", om_name != c("no_rt")) %>%
  mutate(em_name = "old")

no_rt_data_young <- plot_data %>%
  filter(em_name == "no_rt", om_name != c("no_rt")) %>%
  mutate(em_name = "young")

no_rt_data_no_rt <- plot_data %>%
  filter(em_name == "no_rt", om_name == "no_rt") %>%
  select(-om_name) %>%
  cross_join(tibble(om_name = c("young", "mid", "flat", "old"))) %>%
  mutate(em_name = om_name, om_name = "no_rt")

no_rt_data <- rbind(no_rt_data_flat, no_rt_data_mid, no_rt_data_old, no_rt_data_young, no_rt_data_no_rt)

# Main dataset without the standalone no_rt em_name rows
# Remove Inf/NaN values
# Reorder om_names and em_names in plot
main_data <- plot_data %>%
  filter(em_name != "no_rt") %>%
  rbind(no_rt_data) %>%
  filter(is.finite(error)) %>% 
  mutate(om_name = factor(om_name, levels = c("young", "mid", "flat", "old", "no_rt")))%>%
  mutate(em_name = factor(em_name, levels = c("old", "flat", "mid", "young")))

# Create the plot
p <- main_data %>% ggplot() +
  # Reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  # Main facet points
  geom_point(
    aes(x = error, y = em_name, shape = exp_type, color = exp_type),
    alpha = 0.85
  ) +
  
  # Facet using by om_name and error type
  facet_grid(om_name ~ error_type, scales = "free") +
  labs(
    x = "Proportional Error",
    y = "Estimation Model"
  ) +
  # Reformatting and labels
  scale_color_manual(values = c("black", "grey30","grey60")) +
  theme_AFS() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) + 
  labs(shape = "Frequency Assumption (EM)", color = "Frequency Assumption (EM)")

# Add a second axis label
# Create the rotated, bold text label
right_label <- wrap_elements(
  panel = textGrob(
    "Operating Model", 
    rot = -90, 
    gp = gpar(fontface = "bold", fontsize = font_size)
  )
)

# Bind the plot and label side-by-side
# The widths ratio keeps the text column narrow relative to the plot
p + right_label + plot_layout(widths = c(25, 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Figure_x_Error_by_removal_type.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

#  Median time series plots -------------------------------

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
              aes(x = year, y = med_val, color = em_name), linewidth = .5) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    #ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed\nSelectivity (EM)", fill = "Assumed\nSelectivity (EM)") + 
    xlab("Year")
}

# generic rt_2 and all years
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# generic rt_2 and all years
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_rt_2, experiment_type = "Correct Years")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, scenario_list = selectivity_all_yrs, experiment_type = "All Years")

# Spawn Bio
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_2, experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_om_lines(min_yr = 2017, max_yr = 2060, col_name = "SpawnBio", scenario_list = selectivity_rt_2, experiment_type = "Correct Years")

# Bratio
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = selectivity_rt_2, experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = selectivity_all_yrs, experiment_type = "Correct Years")

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt", "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years")
plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt", "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

### Add more Lines 
plot_median_ts_em_lines <- function (summary_data = summary$ts, scenario_list, target_em, min_yr = min_year, max_yr = max_year, col_name = "Recreational", experiment_type) {
  
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

plot_median_ts_em_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), target_em = "_2065", experiment_type = "Correct Years")
plot_median_ts_em_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", scenario_list = core_4, target_em = "_2065", experiment_type = "Presense or Absense of Red Tide")
plot_median_ts_em_lines(summary$dq, min_yr = 2017, max_yr = 2060, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years")

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
plot_median_ts_em_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")
plot_median_ts_em_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", target_em = "_2068", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")




#  Decide on SSB Ratio Figures -------------------------------

## Option 1: 3 Figures - no red tide, known, all years,  -----

### Figure X. SSB when the OM or EM is no red tide  -----
# Achieved SSB Ratio over time when the OM or EM is no red tide.  
# True: No Red Tide (OM) for Known and All years, No Red Tide (EM) for No Years
# Legend: Selectivity
plot_median_ts_om_lines_exp <- function(summary_data = summary$ts, 
                                        scenario_list, 
                                        min_yr = min_year, 
                                        max_yr = max_year, 
                                        col_name = "Recreational", 
                                        experiment_type, baseline_scenario = "no_rt_x_no_rt") {
  
  # 1. Filter raw iteration-level data
  raw_filtered_data <- summary_data %>%
    filter(
      scenario %in% scenario_list,
      str_detect(model_run, "OM"),
      year >= min_yr,
      year <= max_yr
    )
  
  if (baseline_scenario %in% raw_filtered_data$scenario) {
    # Get all target exp_type values excluding NA/baseline
    target_exp_types <- unique(na.omit(raw_filtered_data$exp_type[raw_filtered_data$scenario != baseline_scenario]))
    
    # Extract baseline rows
    baseline_data <- raw_filtered_data %>% 
      filter(scenario %in% baseline_scenario)
    
    # Filter out baseline from raw data, then re-add it duplicated for each exp_type
    raw_filtered_data <- raw_filtered_data %>%
      filter(scenario != baseline_scenario) %>%
      bind_rows(
        lapply(target_exp_types, function(exp_val) {
          baseline_data %>% mutate(exp_type = exp_val)
        }) %>% bind_rows()
      )
  }
  
  # 2. Calculate summary statistics (include exp_type in group_by)
  plot_summary_data <- raw_filtered_data %>%
    group_by(om_name, em_name, exp_type, year) %>%
    reframe(
      med_val = median(.data[[col_name]], na.rm = TRUE),
      low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
      high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
      .groups = "drop" 
    )
  
  new_labels <- c("all_yrs" = "All Years", 
                  "rt_17"   = "17 Years",
                  "old"   = "True: Old Selectivity", 
                  "flat"  = "True: Flat Selectivity", 
                  "no_rt" = "True: No Red Tide (OM)")
  
  # 3. Plotting
  ggplot() +
    geom_hline(yintercept = 0.3, linetype = "dashed") +
    geom_line(data = filter(raw_filtered_data, iteration %in% 1:5), 
              aes(x = year, y = .data[[col_name]], color = em_name, 
                  group = interaction(iteration, om_name, em_name)), 
              alpha = 0.2, linewidth = 0.2) + 
    
    geom_ribbon(data = plot_summary_data, 
                aes(x = year, ymin = low, ymax = high, fill = em_name), alpha = 0.1) +
    
    geom_line(data = plot_summary_data, 
              aes(x = year, y = med_val, color = em_name)) + 
    ylab(paste0(col_name, " (MT)")) + 
    
    # Grid faceting: exp_type rows, om_name columns
    facet_grid(~ exp_type, labeller = labeller(exp_type = new_labels)) + 
    
    labs(color = "Selectivity", fill = "Selectivity") + 
    xlab("Year")
}

no_rt_scenarios <- c(
  "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt",
  "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs","flat_x_no_rt", "young_x_no_rt", "old_x_no_rt", "mid_x_no_rt"
)

#rework just the no_rt em_name scenarios to have om_names instead.  
reworked_no_rt <- summary$dq %>%
  mutate(em_name = if_else(scenario %in% c("flat_x_no_rt", "young_x_no_rt", "old_x_no_rt", "mid_x_no_rt"), om_name, em_name))

all_plot <- plot_median_ts_om_lines_exp(
  summary_data = reworked_no_rt, 
  min_yr = 2017, 
  max_yr = 2068, 
  col_name = "Value.Bratio", 
  scenario_list = no_rt_scenarios, 
  experiment_type = "Correct Years"
) + 
  ylab("SSB Ratio") + 
  theme_bw() + 
  scale_color_viridis_d() + 
  scale_fill_viridis_d() +
  theme(
    text = element_text(size = 7),    
    # --- Facet Box / Strip Borders & Backgrounds ---
    strip.background = element_rect(linewidth = 0.3), # Outer border around facet labels
    panel.border     = element_rect(linewidth = 0.3, fill = NA), # Box around each plot panel
    
    # --- Axis Lines & Ticks ---
    axis.line        = element_line(linewidth = 0.3), # Major x and y axis lines
    axis.ticks       = element_line(linewidth = 0.3), # Tick mark lines
    axis.ticks.length = unit(0.08, "cm"),              # Shorter tick mark length
    
    # --- Grid Lines ---
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    
    # --- Text & Legend Sizing ---
    axis.text.x      = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.key.size  = unit(0.3, "cm")
  )

all_plot + 
  theme_AFS() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_1_Figure_x_no_rt_bratio.pdf"),
         width = 140, height = 70, units = "mm", dpi = 300)
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_1_Figure_x_no_rt_bratio.png"),
         width = 140, height = 70, units = "mm", dpi = 300)
}

### Figure X: Known SSB over time --------


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
              aes(x = year, y = med_val, color = em_name), linewidth = .5) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    #ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed\nSelectivity (EM)", fill = "Assumed\nSelectivity (EM)") + 
    xlab("Year")
}

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_1_Figure_x_rt_17_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

### Figure X: All years SSB over time  ----

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_1_Figure_x_all_yrs_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}


## Option 2: 4 Figures - no red tide (OM), no red tide (EM), known, all years,  -----

### Figure X. SSB when the OM is no red tide  -----
# Achieved SSB Ratio over time when the OM or EM is no red tide.  
# True: No Red Tide (OM) for Known and All years, No Red Tide (EM) for No Years
# Legend: Selectivity
plot_median_ts_om_lines_exp <- function(summary_data = summary$ts, 
                                        scenario_list, 
                                        min_yr = min_year, 
                                        max_yr = max_year, 
                                        col_name = "Recreational", 
                                        experiment_type, baseline_scenario = "no_rt_x_no_rt") {
  
  # 1. Filter raw iteration-level data
  raw_filtered_data <- summary_data %>%
    filter(
      scenario %in% scenario_list,
      str_detect(model_run, "OM"),
      year >= min_yr,
      year <= max_yr
    )
  
  if (baseline_scenario %in% raw_filtered_data$scenario) {
    # Get all target exp_type values excluding NA/baseline
    target_exp_types <- unique(na.omit(raw_filtered_data$exp_type[raw_filtered_data$scenario != baseline_scenario]))
    
    # Extract baseline rows
    baseline_data <- raw_filtered_data %>% 
      filter(scenario %in% baseline_scenario)
    
    # Filter out baseline from raw data, then re-add it duplicated for each exp_type
    raw_filtered_data <- raw_filtered_data %>%
      filter(scenario != baseline_scenario) %>%
      bind_rows(
        lapply(target_exp_types, function(exp_val) {
          baseline_data %>% mutate(exp_type = exp_val)
        }) %>% bind_rows()
      )
  }
  
  # 2. Calculate summary statistics (include exp_type in group_by)
  plot_summary_data <- raw_filtered_data %>%
    group_by(om_name, em_name, exp_type, year) %>%
    reframe(
      med_val = median(.data[[col_name]], na.rm = TRUE),
      low  = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[2],
      high = Hmisc::smedian.hilow(.data[[col_name]], conf.int = 0.95)[3],
      .groups = "drop" 
    )
  
  new_labels <- c("all_yrs" = "All Years", 
                  "rt_17"   = "17 Years",
                  "old"   = "True: Old Selectivity", 
                  "flat"  = "True: Flat Selectivity", 
                  "no_rt" = "True: No Red Tide (OM)")
  
  # 3. Plotting
  ggplot() +
    geom_hline(yintercept = 0.3, linetype = "dashed") +
    geom_line(data = filter(raw_filtered_data, iteration %in% 1:5), 
              aes(x = year, y = .data[[col_name]], color = em_name, 
                  group = interaction(iteration, om_name, em_name)), 
              alpha = 0.2, linewidth = 0.2) + 
    
    geom_ribbon(data = plot_summary_data, 
                aes(x = year, ymin = low, ymax = high, fill = em_name), alpha = 0.1) +
    
    geom_line(data = plot_summary_data, 
              aes(x = year, y = med_val, color = em_name)) + 
    ylab(paste0(col_name, " (MT)")) + 
    
    # Grid faceting: exp_type rows, om_name columns
    facet_grid(~ exp_type, labeller = labeller(exp_type = new_labels)) + 
    
    labs(color = "Selectivity\nAssumption (EM)", fill = "Selectivity\nAssumption (EM)") + 
    xlab("Year")
}

no_rt_scenarios <- c(
  "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt",
  "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs"
)

all_plot <- plot_median_ts_om_lines_exp(
  summary_data = summary$dq, 
  min_yr = 2017, 
  max_yr = 2068, 
  col_name = "Value.Bratio", 
  scenario_list = no_rt_scenarios, 
  experiment_type = "Correct Years"
) + 
  ylab("SSB Ratio") + 
  theme_bw() + 
  scale_color_viridis_d() + 
  scale_fill_viridis_d() +
  theme(
    text = element_text(size = 7),    
    # --- Facet Box / Strip Borders & Backgrounds ---
    strip.background = element_rect(linewidth = 0.3), # Outer border around facet labels
    panel.border     = element_rect(linewidth = 0.3, fill = NA), # Box around each plot panel
    
    # --- Axis Lines & Ticks ---
    axis.line        = element_line(linewidth = 0.3), # Major x and y axis lines
    axis.ticks       = element_line(linewidth = 0.3), # Tick mark lines
    axis.ticks.length = unit(0.08, "cm"),              # Shorter tick mark length
    
    # --- Grid Lines ---
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    
    # --- Text & Legend Sizing ---
    axis.text.x      = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.key.size  = unit(0.3, "cm")
  )

all_plot + 
  theme_AFS() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_2_Figure_x_no_rt_bratio.pdf"),
         width = 140, height = 70, units = "mm", dpi = 300)
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_2_Figure_x_no_rt_bratio.png"),
         width = 140, height = 70, units = "mm", dpi = 300)
}

### Figure X: Known SSB over time --------


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
              aes(x = year, y = med_val, color = em_name), linewidth = .5) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    #ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed\nSelectivity (EM)", fill = "Assumed\nSelectivity (EM)") + 
    xlab("Year")
}

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_2_Figure_x_rt_17_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

### Figure X: All years SSB over time  ----

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_2_Figure_x_all_yrs_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

### Figure X: No red tide in the EM

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
            aes(x = year, y = med_val, color = om_name), linewidth = .5) + # Slightly thicker to pop out
  # --- Formatting layers ---
  #ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
  ylab(paste0(col_name, " (MT)")) + 
  labs(color = "True Selectivity (OM)", fill = "True Selectivity (OM)") + 
  xlab("Year") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  facet_grid(~em_name, labeller = labeller(em_name = new_labels)) + 
  scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_2_Figure_x_no_rt_em_true_bratio.png"),
         width = 90, height = 70, units = "mm", dpi = 300)
}

## Option 3: 2 Figures - 5 panels + legend  -----

### Figure X: Known SSB over time  ----

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
              aes(x = year, y = med_val, color = em_name), linewidth = .5) + # Slightly thicker to pop out
    
    # --- Formatting layers ---
    #ggtitle(paste0("Achieved ", col_name, " over time - ", experiment_type)) + 
    ylab(paste0(col_name, " (MT)")) + 
    facet_wrap(~om_name, labeller = labeller(om_name = new_labels)) + 
    labs(color = "Assumed\nSelectivity (EM)", fill = "Assumed\nSelectivity (EM)") + 
    xlab("Year")
}

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt", "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_young_rt_17", "no_rt_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.2), # Adjust x and y (0 to 1 scale) to fit inside your 6th panel spot
    legend.background = element_rect(fill = "transparent", color = NA) # Optional: removes box background
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_3_Figure_x_rt_17_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

### Figure X: All years SSB over time  ----

plot_median_ts_om_lines(summary$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt", "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.2), # Adjust x and y (0 to 1 scale) to fit inside your 6th panel spot
    legend.background = element_rect(fill = "transparent", color = NA) # Optional: removes box background
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Op_3_Figure_x_all_years_bratio.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

# In-text Materials --------------------------------------------------

## Error Tables ------------------------------------------------------------

# These tables were reconfigured as a plot.  The numbers are frequently
# referenced in the text.  

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

#### All kable

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


# Supplemental Materials --------------------------------------------------

## Gradients ###### 

# plot of raw gradients 

# uncomment if you want to check
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

### Table X. Removed Runs #######

bad_grad <- bad_runs %>%
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
bad_grad

if(save == TRUE){
  save_kable(bad_grad, file = file.path(run_SSMSE_dir, plot_folder,"supplemental_table_x_bad_gradients.html"))
}

## R0 ##### 

summary$scalar %>% 
  ggplot(aes(scenario, SR_LN_R0)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

## Supplemental Figures --------------------------------------------------

# Load supplemental_redo for comparison plot
# This run does not include the adjusted base_models so red tide does not 
# exist in the forecast years.  
summary_rec_devs <- readRDS(file = file.path(run_SSMSE_dir, paste0("results_summary_supplemental_redo.rda")))

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

### Figure X. Known SSB Ratio  -----------

#OM Data
plot_median_ts_om_lines(summary_rec_devs$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_rt_2, "no_rt_x_flat_rt_17", "no_rt_x_old_rt_17", "no_rt_x_young_rt_17", "no_rt_x_mid_rt_17", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "Correct Years") + geom_hline(yintercept = 0.3, linetype = "dashed")+ geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + ggtitle("Achieved SSB Ratio over time - Known Years") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.2), # Adjust x and y (0 to 1 scale) to fit inside your 6th panel spot
    legend.background = element_rect(fill = "transparent", color = NA) # Optional: removes box background
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Supp_Figure_x_rt_17_bratio_supplemental.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}

### Figure X. All Years SSB Ratio  -----------

plot_median_ts_om_lines(summary_rec_devs$dq, min_yr = 2017, max_yr = 2068, col_name = "Value.Bratio", scenario_list = c(selectivity_all_yrs, "no_rt_x_flat_all_yrs", "no_rt_x_old_all_yrs", "no_rt_x_young_all_yrs", "no_rt_x_mid_all_yrs", "no_rt_x_no_rt","flat_x_no_rt", "young_x_no_rt", "mid_x_no_rt", "old_x_no_rt"), experiment_type = "All Years") + geom_hline(yintercept = 0.3, linetype = "dashed")+ geom_hline(yintercept = 0.3, linetype = "dashed") +
  ylab("SSB Ratio") + 
  theme_bw() + scale_color_viridis_d() + scale_fill_viridis_d()  +
  theme_AFS() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.2), # Adjust x and y (0 to 1 scale) to fit inside your 6th panel spot
    legend.background = element_rect(fill = "transparent", color = NA) # Optional: removes box background
  )

if(save == TRUE){
  ggsave(file.path(run_SSMSE_dir,plot_folder, "Supp_Figure_x_all_yrs_bratio_supplemental.png"),
         width = 140, height = 120, units = "mm", dpi = 300)
}
