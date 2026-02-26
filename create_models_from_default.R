# Transforming default OM to account for new selectivity values
# This uses blocking to keep the time varying in one fleet
# and uses the emperical pattern, even for flat scenarios.  

# Load packages
library(r4ss)
library(tidyverse)

# Download ss.exe if linux workstation

exe_path <- "base_models/default_sigmaR/ss.exe"

if (!file.exists(exe_path)) {
  get_ss3_exe(dest_dir = "base_models/default_sigmaR/")
  if (.Platform$OS.type == "unix") {
    system(paste("chmod +x", exe_path))
  }
}

# Initial model to modify
mod_path <- file.path("base_models", "default_sigmaR")

##### Flat Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "flat")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
inputs$start$SSversion
ctl <- inputs$ctl

# Alter the block design
# Add a 4th block for just 2014
ctl$N_Block_Designs <- 4
ctl$blocks_per_pattern <- c(1,2,1,1)
ctl$Block_Design[[4]] <- c(1986,2017)

# Change age_selex patterns
# make RedTide fleet (5) a 14 type pattern
ctl$age_selex_types$Pattern[5] <- 14

# Update AgeSelex parms
# Use 2005 selectivity for the base block in AgeSelex
max_age <- 20
num_rows <- max_age + 1 # for ages 0 through 20

# selectivity parameters from Chagaris et al. 2025
rt_past <- c(9, 9, 9, 9, 9, rep(9, 16)) 
rt_future <- c(9, 9, 9, 9, 9, rep(9, 16)) 

# Create the new parameter block
# Make sure to note the 
redtide_rows <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_future, 
  PRIOR = rt_future,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows),
  env_var = rep(0, num_rows),
  use_dev = rep(0, num_rows),
  dev_minyr = rep(0, num_rows),
  dev_maxyr = rep(0, num_rows),
  dev_PH = rep(0, num_rows),
  Block = rep(4, num_rows),
  Blk_Fxn = rep(2, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Update timevary selex parameters
# Use future selectivity for the future time block

# Create the new parameter block
redtide_rows_past <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_past, # Specific values then repeat
  PRIOR = rt_past,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows_past) <- paste0("redtide(5)_age", 0:max_age, "_BLK4repl_2017")
names(redtide_rows_past) <- names(ctl$age_selex_parms[1:7])
ctl$size_selex_parms_tv <- rbind(ctl$size_selex_parms_tv, redtide_rows_past)

inputs$ctl <- ctl

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

r4ss::run(new_mod_path, exe = normalizePath("base_models/default_sigmaR/ss.exe"))


##### Young Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "young")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
inputs$start$SSversion
ctl <- inputs$ctl

# Alter the block design
# Add a 4th block for just 2014
ctl$N_Block_Designs <- 4
ctl$blocks_per_pattern <- c(1,2,1,1)
ctl$Block_Design[[4]] <- c(1986,2017)

# Change age_selex patterns
# make RedTide fleet (5) a 14 type pattern
ctl$age_selex_types$Pattern[5] <- 14

# Update AgeSelex parms
# Use 2005 selectivity for the base block in AgeSelex
max_age <- 20
num_rows <- max_age + 1 # for ages 0 through 20

# selectivity parameters from Chagaris et al. 2025
rt_past <- c(9, 9, 9, 9, 9, rep(9, 16)) 
rt_future <- c(9, 9, 9, 9, 9, rep(-1.1, 16)) 

# Create the new parameter block
# Make sure to note the 
redtide_rows <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_future, 
  PRIOR = rt_future,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows),
  env_var = rep(0, num_rows),
  use_dev = rep(0, num_rows),
  dev_minyr = rep(0, num_rows),
  dev_maxyr = rep(0, num_rows),
  dev_PH = rep(0, num_rows),
  Block = rep(4, num_rows),
  Blk_Fxn = rep(2, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Update timevary selex parameters
# Use future selectivity for the future time block

# Create the new parameter block
redtide_rows_past <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_past, # Specific values then repeat
  PRIOR = rt_past,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows_past) <- paste0("redtide(5)_age", 0:max_age, "_BLK4repl_2017")
names(redtide_rows_past) <- names(ctl$age_selex_parms[1:7])
ctl$size_selex_parms_tv <- rbind(ctl$size_selex_parms_tv, redtide_rows_past)

inputs$ctl <- ctl

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

r4ss::run(new_mod_path, exe = normalizePath("base_models/default_sigmaR/ss.exe"))

##### Old Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "old")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
inputs$start$SSversion
ctl <- inputs$ctl

# Alter the block design
# Add a 4th block for just 2014
ctl$N_Block_Designs <- 4
ctl$blocks_per_pattern <- c(1,2,1,1)
ctl$Block_Design[[4]] <- c(1986,2017)

# Change age_selex patterns
# make RedTide fleet (5) a 14 type pattern
ctl$age_selex_types$Pattern[5] <- 14

# Update AgeSelex parms
# Use 2005 selectivity for the base block in AgeSelex
max_age <- 20
num_rows <- max_age + 1 # for ages 0 through 20

# selectivity parameters from Chagaris et al. 2025
rt_past <- c(9, 9, 9, 9, 9, rep(9, 16)) 
rt_future <- c(-1.1, -1.1, -1.1, -1.1, -1.1, rep(9, 16)) 

# Create the new parameter block
# Make sure to note the 
redtide_rows <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_future, 
  PRIOR = rt_future,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows),
  env_var = rep(0, num_rows),
  use_dev = rep(0, num_rows),
  dev_minyr = rep(0, num_rows),
  dev_maxyr = rep(0, num_rows),
  dev_PH = rep(0, num_rows),
  Block = rep(4, num_rows),
  Blk_Fxn = rep(2, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Update timevary selex parameters
# Use future selectivity for the future time block

# Create the new parameter block
redtide_rows_past <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_past, # Specific values then repeat
  PRIOR = rt_past,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows_past) <- paste0("redtide(5)_age", 0:max_age, "_BLK4repl_2017")
names(redtide_rows_past) <- names(ctl$age_selex_parms[1:7])
ctl$size_selex_parms_tv <- rbind(ctl$size_selex_parms_tv, redtide_rows_past)

inputs$ctl <- ctl

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

r4ss::run(new_mod_path, exe = normalizePath("base_models/default_sigmaR/ss.exe"))

##### Domed Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "domed")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
inputs$start$SSversion
ctl <- inputs$ctl

# Alter the block design
# Add a 4th block for just 2014
ctl$N_Block_Designs <- 4
ctl$blocks_per_pattern <- c(1,2,1,1)
ctl$Block_Design[[4]] <- c(1986,2017)

# Change age_selex patterns
# make RedTide fleet (5) a 14 type pattern
ctl$age_selex_types$Pattern[5] <- 14

# Update AgeSelex parms
# Use 2005 selectivity for the base block in AgeSelex
max_age <- 20
num_rows <- max_age + 1 # for ages 0 through 20

# selectivity parameters from Chagaris et al. 2025
rt_past <- c(9, 9, 9, 9, 9, rep(9, 16)) 
rt_future <- c(1.1, 9, 9, 9, 9, rep(1.1, 16)) 

# Create the new parameter block
# Make sure to note the 
redtide_rows <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_future, 
  PRIOR = rt_future,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows),
  env_var = rep(0, num_rows),
  use_dev = rep(0, num_rows),
  dev_minyr = rep(0, num_rows),
  dev_maxyr = rep(0, num_rows),
  dev_PH = rep(0, num_rows),
  Block = rep(4, num_rows),
  Blk_Fxn = rep(2, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows) <- paste0("redtide(5)_age", 0:max_age)
names(redtide_rows) <- names(ctl$age_selex_parms)
ctl$age_selex_parms <- redtide_rows

# Update timevary selex parameters
# Use future selectivity for the future time block

# Create the new parameter block
redtide_rows_past <- data.frame(
  LO = rep(-5, num_rows),
  HI = rep(9, num_rows),
  INIT = rt_past, # Specific values then repeat
  PRIOR = rt_past,
  PR_SD = rep(0, num_rows),
  PR_type = rep(0, num_rows),
  PHASE = rep(-99, num_rows)
)

# Set the row names to match your preferred format
rownames(redtide_rows_past) <- paste0("redtide(5)_age", 0:max_age, "_BLK4repl_2017")
names(redtide_rows_past) <- names(ctl$age_selex_parms[1:7])
ctl$size_selex_parms_tv <- rbind(ctl$size_selex_parms_tv, redtide_rows_past)

inputs$ctl <- ctl

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

r4ss::run(new_mod_path, exe = normalizePath("base_models/default_sigmaR/ss.exe"))
