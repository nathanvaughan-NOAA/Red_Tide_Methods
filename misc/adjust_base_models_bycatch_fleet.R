#### Copy and Adjust OMs for EMs

library(tidyverse)
library(r4ss)
library(SSMSE)

# Initial model to modify
mod_path <- file.path("base_models", "flat")

##### Flat Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "flat_adj")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
dat <- inputs$dat

# Change the by_catch fleet info

dat$bycatch_fleet_info$Fmult <- 3
dat$bycatch_fleet_info$F_or_first_year <- 2003
dat$bycatch_fleet_info$F_or_last_year <- 2017

inputs$dat <- dat

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

SSMSE::run_ss_model(new_mod_path)

#repeat for other models

# Initial model to modify
mod_path <- file.path("base_models", "old")

##### Flat Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "old_adj")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
dat <- inputs$dat

# Change the by_catch fleet info

dat$bycatch_fleet_info$Fmult <- 3
dat$bycatch_fleet_info$F_or_first_year <- 2003
dat$bycatch_fleet_info$F_or_last_year <- 2017

inputs$dat <- dat

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

SSMSE::run_ss_model(new_mod_path)


# Initial model to modify
mod_path <- file.path("base_models", "mid")

##### Flat Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "mid_adj")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
dat <- inputs$dat

# Change the by_catch fleet info

dat$bycatch_fleet_info$Fmult <- 3
dat$bycatch_fleet_info$F_or_first_year <- 2003
dat$bycatch_fleet_info$F_or_last_year <- 2017

inputs$dat <- dat

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

SSMSE::run_ss_model(new_mod_path)


# Initial model to modify
mod_path <- file.path("base_models", "young")

##### Flat Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "young_adj")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
dat <- inputs$dat

# Change the by_catch fleet info

dat$bycatch_fleet_info$Fmult <- 3
dat$bycatch_fleet_info$F_or_first_year <- 2003
dat$bycatch_fleet_info$F_or_last_year <- 2017

inputs$dat <- dat

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

SSMSE::run_ss_model(new_mod_path)


# Initial model to modify
mod_path <- file.path("base_models", "flat")

##### None Model #####

# Create a new directory to put a new, modified version of the model
new_mod_path <- file.path("base_models", "none")

# Copy default_SigmaR to Emperical_new folder
copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path)

# Load SS files
inputs <- r4ss::SS_read(dir = new_mod_path)
dat <- inputs$dat

# Change the by_catch fleet info

dat$bycatch_fleet_info$Fmult <- 3
dat$bycatch_fleet_info$F_or_first_year <- 2015
dat$bycatch_fleet_info$F_or_last_year <- 2017

inputs$dat <- dat

# Write file
r4ss::SS_write(inputs, dir = new_mod_path, overwrite = TRUE)

SSMSE::run_ss_model(new_mod_path)