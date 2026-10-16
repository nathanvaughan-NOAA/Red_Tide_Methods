# SSMSE Red Grouper Starter File
# Updated version of OMvsEM_complex_example.R
# Major updates:
#   1. Uses Tidyverse for setting up sample_struct
#   2. All data and results are stored in relative paths (base_models, runs_output)
#   3. Compatible with Google Cloud Workstations and Buckets 

# start timer
start_time <- Sys.time()

# For Debugging
#devtools::load_all("/SSMSE")  # path to cloned SSMSE repo
#debug(EnvirEM) # frequently debugged SSMSE function

# Load packages
library(tidyverse)
library(SSMSE)

# Check versions
packageVersion("r4ss")
packageVersion("ss3sim")
packageVersion("SSMSE")

# Create a folder for the output in the working directory.
results_name <- "red_tide_cleaning_starter"
run_SSMSE_dir <- file.path("./runs_output")
run_res_path <- file.path(run_SSMSE_dir, paste0("results_", results_name))
if (!dir.exists(run_res_path)) {
  dir.create(run_res_path, recursive = TRUE)
}

# Update BUCKET_ADDRESS here or in personal .Renviron
# Only used if cloud_use <- TRUE 
# Moves data from workstation to bucket at the end of each scenario
cloud_use <- FALSE
BUCKET_ADDRESS <- "gs://"
bucket_path <- normalizePath(paste0(BUCKET_ADDRESS, "/results_", results_name)) 
mount_path <- file.path("./bucket")

# OM locations and default model used for sourcing parameter names and model structure
model_SSMSE_dir <- file.path("base_models")
default <- file.path(model_SSMSE_dir, "default_sigmaR")

# Update key numbers
projyrs <- 51 # Projected Years
rt_yrs <- 17 # Number of red tide events
my_niter <- 100 # Number of iterations per scenario
n_cores_run <- 100 # Number of cores to use per scenario
nyrs_assess <- 3 # Number of years between stock assessments
# Selectivity Multipliers
# These were calculated by Total ages affected/Ages affected by selectivity type
# used to even out the mortality under different selectivity assumptions.  
young_multiplier <- 21/9
old_multiplier <- 21/17.25
mid_multiplier <- 21/16.75
flat_multiplier <- 21/21

# to get the names of parameter values
ctl <- r4ss::SS_readctl(file.path(default, "red_grouper_1986_2017_RedTideFleet.ctl"), 
                        version = "3.30", 
                        use_datlist = TRUE, 
                        datlist = file.path(default, "data.ss_new"))
dat <- r4ss::SS_readdat(file.path(default, "red_grouper_1986_2017_RedTideFleet.dat"))

# Add process error through rec devs #####
# start from a list created by a helper function
template_mod_change<-SSMSE::create_future_om_list()
# add rec devs
rec_dev_specify<-template_mod_change[[1]]
rec_dev_specify$pars<-"rec_devs"
rec_dev_specify$scen<-c("replicate","all") 
rec_dev_specify$input$first_yr_averaging<-ctl$MainRdevYrFirst
rec_dev_specify$input$last_yr_averaging<-ctl$MainRdevYrLast
# The following 2 lines suggest that this change is immediately applied in year
# 101, with no transitory period for using sd 0 to the new sd.
rec_dev_specify$input$last_yr_orig_val<-dat$endyr
rec_dev_specify$input$first_yr_final_val<-dat$endyr+1
rec_dev_specify$input$ts_param<-"sd" 
rec_dev_specify$input$value<-NA

# future OM list
future_OM_list_recdevs<-list(rec_dev_specify)

# Update sample structure #####
# This section updates the sample structure to work for Red Grouper.  

datfile<-dat
#sample_struct<-create_sample_struct(dat=datfile, nyrs=projyrs)
sample_struct<-SSMSE:::create_sample_struct_envir(dat=datfile, nyrs=projyrs)
# LOTS OF WARNINGS/ERRORS, so need to go input by input and enter errors
# By default, create_sample_struct identifies sampling patterns from the 
# historical period of the OM and replicates those patterns in the projection 
# period. Note that length comp (lencomp) includes an NA value for year. 
# This is because no consistent pattern was identified, so the user must 
# define their own input.
names(sample_struct)
# currently includes catch, CPUE, lencomp, agecomp, meanbodywt, MeanSize_at_Age_obs
# discard_data added for red snapper MSE
# EM2OMcatch_bias and EM2OMdiscard_bias added for FES MSE
# Fixed catch added to handle shrimp bycatch or red tide kills externally


###### Landings Error ####

table(dat$catch$fleet,dat$catch$catch_se)

#Specify SEs as mean of past 20 yrs of observed catch
sample_struct$catch <- sample_struct$catch %>%
  filter(FltSvy %in% c(1,2,4)) %>% # exclude trap since landings stopped in 2007 & red tide
  rename(fleet = FltSvy) %>% # make fleet column name match
  left_join(filter(dat$catch, year > 2000)) %>% # filter only the years after 2000
  summarise(SE = mean(catch_se), .by = c(Yr, Seas, fleet)) %>% # take the mean of the SE
  rename(FltSvy = fleet) # return the fleet column to SS format

###### EM2OMcatch_bias Error###### 

sample_struct$EM2OMcatch_bias 
#Default values of 1 (no bias)

###### Fixed catch Error###### 

sample_struct$FixedCatch <- NULL

###### CPUE Error###### 

# if the survey has operated in the last 2 years, assume yearly operation into the future

proj_indecies <- dat$CPUE %>% 
  filter(year > dat$endyr - 2) %>% # Identify the indecies that have CPUEs in the last 2 years
  distinct(index) # Make a list of those indecies

sample_struct$CPUE <- dat$CPUE %>% 
  filter(index %in% proj_indecies$index) %>% # Filter only the indecies that have CPUEs in the last 2 years
  group_by(index) %>% # group by index
  summarize(SE = mean(se_log[year > 2000], na.rm = TRUE), .groups = "drop") %>% #take the mean of the se_log only for CPUEs after 2000
  rowwise() %>% # go row by row to apply the sumarized SE and index to each year
  mutate(
    projection = list(
      data.frame(
        Yr = (dat$endyr+1):(dat$endyr + projyrs), #the years we want
        Seas = 7, #only one sea
        FltSvy = index, #pulls from the row, duplicates for each year
        SE = SE #pulls from the row, duplicates for each year
      )
    )
  )%>%
  select(projection) %>% # removes the old SE and index rows
  unnest(cols = c(projection)) #takes it out of a nested list format 

sample_struct$CPUE <- as.data.frame(sample_struct$CPUE)

head(sample_struct$CPUE)

###### Discards Error ###### 

table(dat$discard_data$Flt,dat$discard_data$Std_in)
#Fleet 3 is commercial trap, which is no longer active so remove it
sample_struct$discard_data <- sample_struct$discard_data %>% 
  filter(FltSvy != c(3)) 

###### EM2OMdiscard_bias Error###### 

sample_struct$EM2OMdiscard_bias
#Default values of 1 (no bias)

###### Lencomp Error###### 

sample_struct$lencomp
table(datfile$lencomp$Seas)   #7
table(datfile$lencomp$Gender) #1
table(datfile$lencomp$Part)   #0, 1, 2
table(datfile$lencomp$FltSvy,datfile$lencomp$Part)  
# note: month = 7 and sex = 1
sample_struct$lencomp<-sample_struct$lencomp[0,] # reset dataframe
for(i in  unique(dat$lencomp$FltSvy)){ # unique fleet/surveys w len comp data 
  sublc20<-dat$lencomp[dat$lencomp$FltSvy ==i & dat$lencomp$Yr > dat$endyr-20,] # get subset of lencomps over past 20 yrs
  sublc2<-dat$lencomp[dat$lencomp$FltSvy ==i & dat$lencomp$Yr > dat$endyr-2,] # get subset of lencomps over past 2 yrs
  if(nrow(sublc2)>0){ # only assume projection values if they have been observed in the last 2 years, otherwise assume truncated data series
    
    sublc20_P0<-sublc20[sublc20$Part==0,] # get combined (Part==0) lencomp projections
    if(nrow(sublc20_P0)>0){ # only project retained catch if observed over past 20 yrs
      if((dat$endyr+1) %in% sublc20_P0$Yr){ 
        Nsamp_t<- c(sublc20_P0$Nsamp[sublc20_P0$Yr==TermYr], rep(mean(sublc20_P0$Nsamp), projyrs-1)) # use terminal year value if present in dat file
      }else{
        Nsamp_t<- rep(mean(sublc20_P0$Nsamp), projyrs) # otherwise, use mean Nsamps observed over past 20 yrs for all proj yrs
      }
      # get retained or combined lencomp projection dataframe 
      kept0_lf<-data.frame(Yr=(dat$endyr+1):(dat$endyr+projyrs), Seas=rep(7, projyrs), FltSvy=rep(i, projyrs),
                           Sex=rep(0, projyrs), Part=rep(0,projyrs), Nsamp=Nsamp_t)
      
    }else{
      kept0_lf<-NULL # make retained/combined dataframe empty if no observations for fleet i
    } #end if-else retained/combined lencomp data
    
    sublc20_P1<-sublc20[sublc20$Part==1,] # get discarded lencomps when exist
    
    if(nrow(sublc20_P1)>0){
      frq1<-round(10/nrow(sublc20_P1)) # take average frequency of discard lengcomps as observed over past 10 yrs
      Yr_t<-seq(from=dat$endyr+1, to=dat$endyr+projyrs, by=frq1) # proj discard lencomp observations w same frequency 
      # get discard projected lencomps dataframe
      disc_lf<-data.frame(Yr=Yr_t, Seas=rep(7, length(Yr_t)), FltSvy=rep(i, length(Yr_t)), Sex=rep(0, length(Yr_t)),
                          Part=rep(1,length(Yr_t)), Nsamp=rep(mean(sublc20_P1$Nsamp), length(Yr_t)) ) 
    }else{
      disc_lf<-NULL # make discard dataframe empty if no discard observations for fleet i
    } #end if-else discard lencomp data
    
    sublc20_P2<-sublc20[sublc20$Part==2,] # get retained (Part==2) lencomp projections
    if(nrow(sublc20_P2)>0){ # only project retained catch if observed over past 20 yrs
      if((dat$endyr+1) %in% sublc20_P2$Yr){ 
        Nsamp_t<- c(sublc20_P2$Nsamp[sublc20_P2$Yr==TermYr], rep(mean(sublc20_P2$Nsamp), projyrs-1)) # use terminal year value if present in dat file
      }else{
        Nsamp_t<- rep(mean(sublc20_P2$Nsamp), projyrs) # otherwise, use mean Nsamps observed over past 20 yrs for all proj yrs
      }
      # get retained or combined lencomp projection dataframe 
      kept2_lf<-data.frame(Yr=(dat$endyr+1):(dat$endyr+projyrs), Seas=rep(7, projyrs), FltSvy=rep(i, projyrs),
                           Sex=rep(0, projyrs), Part=rep(2,projyrs), Nsamp=Nsamp_t)
      
    }else{
      kept2_lf<-NULL # make retained/combined dataframe empty if no observations for fleet i
    } #end if-else retained/combined lencomp data
    
    sample_struct$lencomp <- rbind(sample_struct$lencomp, disc_lf, kept0_lf,kept2_lf) # append discard and retained/combined lencomp dataframes for fleet i to the previous fleets
    
  }# end if statement
}# end length section

###### Agecomp Error###### 

sample_struct$agecomp
table(datfile$agecomp$Seas)    # 7
table(datfile$agecomp$Gender)  # 1
table(datfile$agecomp$Part)    # 2
table(datfile$agecomp$Ageerr)  # 1 
table(datfile$agecomp$Lbin_lo) # -1
table(datfile$agecomp$Lbin_hi) # -1
# Year values that are before start year or after end year are excluded from model, so the easiest way to include provisional data in a data file is to put a negative sign on its year value.
# negative year age comps are ignored from projections
# note all sex==1, all Part == 2, all all ageerr=1, Lbin_lo=Lbin_hi=-1
sample_struct$agecomp<-sample_struct$agecomp[0,] # reset dataframe
for(i in  unique(dat$agecomp$FltSvy)){ # unique fleet/surveys w len comp data 
  subac20<-dat$agecomp[dat$agecomp$FltSvy ==i & dat$agecomp$Yr > dat$endyr-20,] # get subset of agecomps over past 20 yrs
  subac2<-dat$agecomp[dat$agecomp$FltSvy ==i & dat$agecomp$Yr > dat$endyr-2,] # get subset of agecomps over past 2 yrs
  if(nrow(subac2)>0){ # only assume projection values if they have been observed in the last 2 years, otherwise assume truncated data series
    # create age sampling projection data frame
    agec<-data.frame(Yr=(dat$endyr+1):(dat$endyr+projyrs), Seas=rep(subac2$Seas[1], projyrs), 
                     FltSvy=rep(i, projyrs), Sex=rep(1, projyrs), Part=rep(2,projyrs), 
                     Ageerr=rep(1,projyrs), Lbin_lo=rep(-1,projyrs), Lbin_hi=rep(-1,projyrs),
                     Nsamp=rep(mean(subac20$Nsamp),projyrs) )
    
    sample_struct$agecomp <- rbind(sample_struct$agecomp, agec) # append survey i agecomp dataframe to previous fleets
    
  }#end if projection 
} #end i forloop for agecomp section
sample_struct$agecomp

###### Meanbody Wt Error###### 

sample_struct$meanbodywt

###### Mean Size At Age Error ###### 

sample_struct$MeanSize_at_Age_obs

# Red Tide Sample Structure Changes ####

# Adjusting sample structure to include future red tide events
# This manipulates Fixed Catch and Fixed Catch EM values in sample_struct
# Uses SSMSE::add_sample_struct_FixedCatches()


### Base red tide sample_struct's ####

# This chunk generates the frequency scenarios used as base sample_struct later 
sample_struct_no_rt_x_rt_2 <- add_sample_struct_FixedCatches(sample_struct, om_on = FALSE)
sample_struct_rt_2_x_no_rt <- add_sample_struct_FixedCatches(sample_struct, em_on = FALSE)
sample_struct_rt_2_x_rt_2 <- add_sample_struct_FixedCatches(sample_struct)
sample_struct_no_rt_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, om_on = FALSE, rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_rt_2_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_rep_3_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, rt_year_om = seq(from = 2018, to = 2017+projyrs, by = 3), rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_rt_2_x_rt_2_fixed <- add_sample_struct_FixedCatches(sample_struct, em_fixed = 1)

# This chunk generates the selectivity scenarios used as base sample_struct later 
sample_struct_young_rt_2 <- add_sample_struct_FixedCatches(sample_struct,
                                                           rt_mortality_om = 0.1*young_multiplier,
                                                           rt_mortality_em = 0.1*young_multiplier)
sample_struct_young_all_yrs <- add_sample_struct_FixedCatches(sample_struct,
                                                              rt_mortality_om = 0.1*young_multiplier,
                                                              rt_mortality_em = 0.1*young_multiplier,
                                                              rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_old_rt_2 <- add_sample_struct_FixedCatches(sample_struct,
                                                         rt_mortality_om = 0.1*old_multiplier,
                                                         rt_mortality_em = 0.1*old_multiplier)
sample_struct_old_all_yrs <- add_sample_struct_FixedCatches(sample_struct,
                                                            rt_mortality_om = 0.1*old_multiplier,
                                                            rt_mortality_em = 0.1*old_multiplier,
                                                            rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_mid_rt_2 <- add_sample_struct_FixedCatches(sample_struct,
                                                         rt_mortality_om = 0.1*mid_multiplier,
                                                         rt_mortality_em = 0.1*mid_multiplier)
sample_struct_mid_all_yrs <- add_sample_struct_FixedCatches(sample_struct,
                                                            rt_mortality_om = 0.1*mid_multiplier,
                                                            rt_mortality_em = 0.1*mid_multiplier,
                                                            rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))
sample_struct_flat_rt_2 <- add_sample_struct_FixedCatches(sample_struct,
                                                          rt_mortality_om = 0.1*flat_multiplier,
                                                          rt_mortality_em = 0.1*flat_multiplier)
sample_struct_flat_all_yrs <- add_sample_struct_FixedCatches(sample_struct,
                                                             rt_mortality_om = 0.1*flat_multiplier,
                                                             rt_mortality_em = 0.1*flat_multiplier,
                                                             rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))

### Global run_SSMSE settings ####
# global settings for all SSMSE runs
base_params <- list(
  iter_vec        = my_niter,
  out_dir_scen_vec = normalizePath(run_res_path),
  run_EM_last_yr  = TRUE,
  MS_vec          = "EnvirEM",
  nyrs_vec        = projyrs,
  nyrs_assess_vec = nyrs_assess,
  future_om_list  = future_OM_list_recdevs,
  run_parallel    = TRUE,
  n_cores         = n_cores_run,
  seed            = 12345,
  # Normalize these once here
  OM_in_dir_vec   = normalizePath(default),
  EM_in_dir_vec   = normalizePath(default), 
  file_removal    = TRUE #deletes files not needed for summarizing
)

### Specific scenario run_SSMSE settings ####
# use modifyList() to adjust the run_SSMSE parameters

###### Core 4 ####
# The original core 4 models with just 2 red tide events in 2018 and 2021
no_rt <- modifyList(base_params, list(scen_name_vec = "no_rt", sample_struct_list = list("no_rt" = sample_struct)))
no_rt_x_rt_2 <- modifyList(base_params, list(scen_name_vec = "no_rt_x_rt_2", sample_struct_list = list("no_rt_x_rt_2" = sample_struct_no_rt_x_rt_2)))
rt_2_x_no_rt <- modifyList(base_params, list(scen_name_vec = "rt_2_x_no_rt", sample_struct_list = list("rt_2_x_no_rt" = sample_struct_rt_2_x_no_rt)))
rt_2_x_rt_2 <- modifyList(base_params, list(scen_name_vec = "rt_2_x_rt_2", sample_struct_list = list("rt_2_x_rt_2" = sample_struct_rt_2_x_rt_2)))

###### All years ####
# The original all_yrs models with just 2 red tide events in 2018 and 2021
# or repeating every 3 years starting in 2018
no_rt_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "no_rt_x_all_yrs", sample_struct_list = list("no_rt_x_all_yrs" = sample_struct_no_rt_x_all_yrs)))
rt_2_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "rt_2_x_all_yrs", sample_struct_list = list("rt_2_x_all_yrs" = sample_struct_rt_2_x_all_yrs)))
rep_3_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "rep_3_x_all_yrs", sample_struct_list = list("rep_3_x_all_yrs" = sample_struct_rep_3_x_all_yrs)))

###### Fixed EM ####
# the fixed EM where red tide mortality was fixed at 0.1 magnitude and not estimated
rt_2_x_rt_2_fixed <- modifyList(base_params, list(scen_name_vec = "rt_2_x_rt_2_fixed", sample_struct_list = list("rt_2_x_rt_2_fixed" = sample_struct_rt_2_x_rt_2_fixed)))

###### Orginal Selectivity ####
# the selectivity factorial automatically creates scenarios with every combination
# of selectivity paired, as provided in model_names.  
# There should be an existing base sample_struct above for this to pull from.  

#' Generate Factorial Combinations of Model Scenarios
#'
#' Constructs a grid or list of factorial scenario combinations based on model 
#' structures, structural types, and mortality variations, typically used to set up 
#' simulation experiments or sensitivity analyses.
#'
#' @param model_names Character vector. Names of the model structures to evaluate. 
#'   Defaults to `c("flat", "young")`.
#' @param type_name Character or Character vector, optional. Name(s) identifying specific 
#'   scenario types or classifications. Defaults to `NULL`.
#' @param type_struct List or Vector, optional. Structural settings or configurations 
#'   associated with each scenario type. Defaults to `NULL`.
#' @param varied_mortality Logical. Whether to apply varying mortality rates across 
#'   scenarios (`TRUE`) or keep them constant (`FALSE`). Defaults to `FALSE`.
#' @param base_extras List, optional. Additional baseline parameters or metadata to 
#'   append to each scenario context. Defaults to `NULL`.
#'
#' @return A `data.frame` or `list` containing all factorial combinations of the specified scenario parameters.
#' @export
#'
scenario_factorial <- function(model_names = c("flat", "young"), type_name = NULL, type_struct = NULL, varied_mortality = FALSE, base_extras = NULL) {
  
  # create a grid of all model combos
  grid <- expand.grid(OM = model_names, EM = model_names, stringsAsFactors = FALSE)
  
  # create an SSMSE scenario for each model combo
  scenarios <- lapply(1:nrow(grid), function(i) {
    # create scen_name_vec
    combo <- grid[i, ]
    scen_name <- paste0(combo$OM, "_x_", combo$EM, type_name)
    
    # create or fetch sample_struct
    if(is.null(type_name)) {
      type_struct <- paste0("sample_struct_", scen_name)
      struct_obj <- get(type_struct)
    } else {
      if(varied_mortality == TRUE){
        type_struct <- paste0("sample_struct_", combo$OM, type_name)
        struct_obj <- get(type_struct)
      } else {
        struct_obj <- type_struct
      }
    }
    
    if(!is.null(base_extras)){
      multiplier_name <- paste0(combo$OM, "_multiplier")
      # Use a local copy to modify
      new_extras <- base_extras
      
      # Ensure my_niter exists or use seq_along
      for(iters in seq_along(new_extras$RandomFixedCatch)){ 
        multiplier_val <- get(multiplier_name)
        
        # Perform the multiplication and assign it back to the local object
        new_extras$RandomFixedCatch[[iters]]$Catch <- 
          base_extras$RandomFixedCatch[[iters]]$Catch * multiplier_val
      }
      print(new_extras$RandomFixedCatch[[1]]$Catch[[1]])
      
      # Return the modified list so it ends up in the 'scenarios' output
      return(modifyList(base_params, list(
        scen_name_vec = scen_name,
        sample_struct_list = setNames(list(struct_obj), scen_name),
        OM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, combo$OM)),
        EM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, paste0(combo$EM, "_adj"))), 
        extra = list(new_extras)
      )))
    } else {
      # add new adjustments to SSMSE_run list and OM/EM locations
      modifyList(base_params, list(
        scen_name_vec = scen_name,
        sample_struct_list = setNames(list(struct_obj), scen_name),
        OM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, combo$OM)),
        EM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, paste0(combo$EM, "_adj")))
      ))
    }
  }
  )
}

# The selectivity curve names and types of structures used.  
model_names = c("flat", "young", "old", "mid")
type_name = "_all_yrs"
type_struct = sample_struct_rt_2_x_all_yrs

# The original selectivity scenarios without extras
# extras introduced random red tide events for each iteration
# these scenarios would have only 2 red tide events.  
all_yrs_scenarios <- scenario_factorial(model_names = model_names, type_name = type_name, type_struct = sample_struct_rt_2_x_all_yrs)    
rt_2_scenarios <- scenario_factorial(model_names = model_names, type_name = "_rt_2", type_struct = sample_struct_rt_2_x_rt_2)    
# varied allowed the red tide mortality to be different in specific scenarios.
all_yrs_scenarios_varied <- scenario_factorial(model_names = model_names, type_name = type_name, varied_mortality = TRUE)    
rt_2_scenarios_varied <- scenario_factorial(model_names = model_names, type_name = "_rt_2", varied_mortality = TRUE)   

###### Add random red tides ####

# Random OM 
#' Create a Random Fixed Catch Matrix Across Iterations
#'
#' Generates a matrix or array of random mortality values drawn from a uniform 
#' distribution bounded by specified minimum and maximum mortality limits, 
#' typically used in fisheries management strategy evaluation (MSE) or 
#' population modeling.
#'
#' @param my_niter Integer. The number of iterations/simulations to generate.
#' @param FixedCatch Numeric/Matrix. Baseline input structure.
#' @param rt_fleet Integer or Character vector. Fleet identifier(s) subject to 
#'   random mortality/catch adjustments.
#' @param n_rt_years Integer. The number of years in the projection.
#' @param min_mortality Numeric. The minimum bound for the uniformly distributed random mortality rate.
#' @param max_mortality Numeric. The maximum bound for the uniformly distributed random mortality rate.
#' @param mean_mortality Numeric. Target or baseline mean mortality rate (if used for scaling or reference).
#' @param set_seed Integer, optional. Seed number for random number generation to ensure reproducibility. Default is `NULL`.
#'
#' @return A matrix containing iteration-specific random mortality/catch values across years and fleets.
#' @export
create_RandomFixedCatch <- function (my_niter,
                                     FixedCatch,
                                     rt_fleet,
                                     n_rt_years,
                                     min_mortality,
                                     max_mortality,
                                     mean_mortality, 
                                     set_seed = NULL)
{
  set.seed(set_seed)
  FixedCatch <- FixedCatch %>% filter(FltSvy == rt_fleet)
  RandomFixedCatch <- list()
  rt_year_om = sample(1:length(FixedCatch[, 1]), length(FixedCatch[, 1])) # random years
  #  if (projyrs <= n_rt_years*my_niter){
  for (i in 1:my_niter) {
    this_iter_rt <- rt_year_om[1:n_rt_years]
    rt_year_om <- rt_year_om[-c(1:n_rt_years)]
    
    if(length(rt_year_om) < n_rt_years ){
      rt_year_om <- c(rt_year_om, sample(c(1:length(FixedCatch[,1]))[which(!is.element(1:length(FixedCatch[,1]),rt_year_om))], length(c(1:length(FixedCatch[,1]))[which(!is.element(1:length(FixedCatch[,1]),rt_year_om))])))
    }
    
    RandomFixedCatch[[i]] <- FixedCatch
    
    rt_mortality_om = runif(n_rt_years, min_mortality, max_mortality) # random mortality
    rt_mortality_om = rt_mortality_om * (mean_mortality / mean(rt_mortality_om)) # rescale to the mean
    
    RandomFixedCatch[[i]][this_iter_rt, "Catch"] <- rt_mortality_om
  }
  #  } else { print("There are not enough iterations or years for this method")  }
  return(RandomFixedCatch)
}

# make a blank copy of sample struct with FixedCatches and FixedCatchesEM
sample_struct_fc <- SSMSE::create_sample_struct_envir(
  dat = datfile,
  nyrs = projyrs,
  FixedCatches = TRUE,
  FixedCatchesEM = TRUE
)

# Create extras_base to provide all random red tide mortalities
extras_base <- list()
extras_base$RandomFixedCatch <- create_RandomFixedCatch(
  my_niter = my_niter,
  FixedCatch = sample_struct_fc$FixedCatch,
  rt_fleet = 5,
  n_rt_years = rt_yrs,
  min_mortality = 0.05,
  max_mortality = 0.25,
  mean_mortality = 0.1, 
  set_seed = 12345
)

extras_base$RandomFixedCatchEM <- extras_base$RandomFixedCatch

for (i in 1:my_niter) {
  names(extras_base$RandomFixedCatchEM[[i]])[5] <- "catch_se"
  rt_catch_se <- rep(0.01, length = nrow(extras_base$RandomFixedCatchEM[[i]]))
  extras_base$RandomFixedCatchEM[[i]]$catch_se <- rt_catch_se
  extras_base$RandomFixedCatchEM[[i]]$fixed <- rep(0, length = nrow(extras_base$RandomFixedCatchEM[[i]])) # a new column that indicates if the EM's catch is estimated or not.
}

###### Selectivity with random red tides ####
# Combines the selectivity factorial with added extras data frame for randomness
# These were the selectivity scenarios used for the manuscript

all_yrs_scenarios_extra <- scenario_factorial(model_names = model_names, type_name = type_name, varied_mortality = TRUE, base_extras = extras_base[-2])    
rt_2_scenarios_extra <- scenario_factorial(model_names = model_names, type_name = "_rt_2", varied_mortality = TRUE, base_extras = extras_base) 


###### No Red Tide in OM ####

# Create sample_struct with no red tide in the OM
sample_struct_no_rt_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, om_on = TRUE, rt_year_om = NULL, rt_year_em = seq(from = 2018, to = 2017+projyrs, by = 1))

make_no_rt_all_yrs_model <- function(EM_name = "flat", EM_type = "all_yrs"){
  no_rt_all_yrs_model <- modifyList(
    base_params,
    list(
      scen_name_vec = paste0("no_rt_x_", EM_name,"_", EM_type),
      sample_struct_list = setNames(list(sample_struct_no_rt_x_all_yrs), paste0("no_rt_x_", EM_name,"_", EM_type)),
      OM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, "none")),
      EM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, paste0(EM_name, "_adj")))))
}


no_rt_x_flat_all_yrs <- make_no_rt_all_yrs_model("flat")
no_rt_x_young_all_yrs <- make_no_rt_all_yrs_model("young")
no_rt_x_mid_all_yrs <- make_no_rt_all_yrs_model("mid")
no_rt_x_old_all_yrs <- make_no_rt_all_yrs_model("old")

sample_struct_no_rt_x_rt_17 <- add_sample_struct_FixedCatches(sample_struct, om_on = TRUE, rt_year_om = NULL)
make_no_rt_17_model <- function(EM_name = "flat", EM_type = "rt_17"){
  no_rt_all_yrs_model <- modifyList(
    base_params,
    list(
      scen_name_vec = paste0("no_rt_x_", EM_name,"_", EM_type),
      sample_struct_list = setNames(list(sample_struct_no_rt_x_rt_17), paste0("no_rt_x_", EM_name,"_", EM_type)),
      OM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, "none")),
      EM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, paste0(EM_name, "_adj"))), 
      extra = list(extras_base[-1])
    ))
}

no_rt_x_flat_rt_17 <- make_no_rt_17_model("flat")
no_rt_x_young_rt_17 <- make_no_rt_17_model("young")
no_rt_x_mid_rt_17 <- make_no_rt_17_model("mid")
no_rt_x_old_rt_17 <- make_no_rt_17_model("old")

###### No Red Tide in EM ####

sample_struct_rt_17_x_no_rt <- add_sample_struct_FixedCatches(sample_struct, om_on = TRUE, em_on = FALSE)

make_rt_17_no_model <- function(OM_name = "flat") {
  
  scen_name <- paste0(OM_name,"_x_no_rt")
  
  multiplier_name <- paste0(OM_name, "_multiplier")
  # Use a local copy to modify
  new_extras <- extras_base
  
  # Ensure my_niter exists or use seq_along
  for(iters in seq_along(new_extras$RandomFixedCatch)){ 
    multiplier_val <- get(multiplier_name)
    
    # Perform the multiplication and assign it back to the local object
    new_extras$RandomFixedCatch[[iters]]$Catch <- 
      extras_base$RandomFixedCatch[[iters]]$Catch * multiplier_val
  }
  
  no_rt_all_yrs_model <- modifyList(
    base_params,
    list(
      scen_name_vec = scen_name,
      sample_struct_list = setNames(list(sample_struct_rt_17_x_no_rt), scen_name),
      OM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, OM_name)),
      EM_in_dir_vec   = normalizePath(file.path(model_SSMSE_dir, "none")), 
      extra = list(new_extras[-2])
    ))
}

flat_x_no_rt <- make_rt_17_no_model("flat")
young_x_no_rt <- make_rt_17_no_model("young")
mid_x_no_rt <- make_rt_17_no_model("mid")
old_x_no_rt <- make_rt_17_no_model("old")

###### No Red Tide in OM and EM ####

#redo no_rt_x_no_rt
no_rt_x_no_rt <- flat_x_no_rt
no_rt_x_no_rt$extra <- NULL
no_rt_x_no_rt$sample_struct_list$flat_x_no_rt$FixedCatch <- NULL
names(no_rt_x_no_rt$sample_struct_list)[1] <- "no_rt_x_no_rt"
no_rt_x_no_rt$scen_name_vec <- "no_rt_x_no_rt"

# List all Scenarios ####

# put the scenarios you want to run into a list

# Original scenarios
# 4 core, all_years, fixed
# all_scenarios <- list(
#   no_rt, 
#   no_rt_x_rt_2,
#   rt_2_x_no_rt,
#   rt_2_x_rt_2, 
#   no_rt_x_all_yrs,
#   rt_2_x_all_yrs,
#   rep_3_x_all_yrs,
#   rt_2_x_rt_2_fixed
# )

# Original selectivity Scenarios
# all_scenarios <- c(
#   rt_2_scenarios,
#   list(rt_2_x_rt_2),
#   all_yrs_scenarios, 
#   list(rt_2_x_all_yrs)
# )

# Selectivity varied mortality
# all_scenarios <- c(
#   rt_2_scenarios_varied,
#   list(rt_2_x_rt_2),
#   all_yrs_scenarios_varied, 
#   list(rt_2_x_all_yrs)
# )

# no_rt scenarios  

no_rt_scenarios <- list(
  no_rt_x_no_rt,
  no_rt_x_flat_all_yrs,
  no_rt_x_young_all_yrs,
  no_rt_x_mid_all_yrs,
  no_rt_x_old_all_yrs,
  no_rt_x_flat_rt_17,
  no_rt_x_young_rt_17,
  no_rt_x_mid_rt_17,
  no_rt_x_old_rt_17,
  flat_x_no_rt,
  young_x_no_rt,
  mid_x_no_rt,
  old_x_no_rt
)

# All 45 scenarios listed together
all_scenarios <- c(
  no_rt_scenarios,
  rt_2_scenarios_extra,
  all_yrs_scenarios_extra
)

# Dividing All scenarios for multiple background processes for 
# cloud computing.  
# Background process 1: 100 cores, 30 scenarios
#all_scenarios <- all_scenarios[1:30]
# Backgound process 2: 50 cores, 15 scenarios
#all_scenarios <- all_scenarios[31:45]

# creates a list of all the scenario names
scen_list_str <- all_scenarios %>%
  map_chr(\(x) x$scen_name_vec) %>%
  str_flatten(collapse = ", ", last = ", and ")

# RUN SSMSE #####

# Function that takes in a listed base_params, an input to run_SSMSE()
# Runs run_SSMSE, summarizes scenarios, and transfers to the cloud
run_summary_SSMSE <- function(...) {
  args <- list(...)
  dir_path <- file.path(args$out_dir_scen_vec, args$scen_name_vec)
  scenario_name <- args$scen_name_vec
  
  # Run SSMSE with error handling so execution doesn't halt abruptly
  tryCatch({
    run_SSMSE(...)
  }, error = function(e) {
    warning("run_SSMSE threw an error for scenario ", scenario_name, ": ", e$message)
  })
  
  # Try collecting scenario summary results
  tryCatch({
    ss3sim:::get_results_scenario(scenario_name, run_res_path)
  }, error = function(e) {
    warning("get_results_scenario failed for ", scenario_name, ": ", e$message)
  })
  
  # Transfer data to a google cloud bucket after each scenario
  if(cloud_use == TRUE) {
    # Construct target cloud URI
    target_cloud_iteration_dir <- file.path(bucket_path, scenario_name)
    
    message("Starting sync from local: ", dir_path, " to cloud: ", target_cloud_iteration_dir)
    
    # Sync command
    rsync_cmd <- paste(
      "gcloud storage rsync",
      shQuote(dir_path),
      shQuote(target_cloud_iteration_dir),
      "-r"
    )
    status <- system(rsync_cmd)
    
    if (status == 0) {
      message("Success! Synced contents to cloud.\n")
      contents <- list.files(dir_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      unlink(contents, recursive = TRUE)
    } else {
      warning("gcloud storage rsync failed for scenario ", scenario_name, ".\n")
    }
  }
}

# walk through the scenario list 1 scenario at a time
walk(all_scenarios, ~exec(run_summary_SSMSE, !!!.x))  # !!! makes the scenario list into arguments that can be used by a function

# Summarize all of the results if not using cloud computing
if(cloud_use == FALSE) {
  summary <- SSMSE::SSMSE_summary_all(file.path(run_SSMSE_dir, paste0("results_", results_name)))
  saveRDS(summary, file = file.path(
    run_SSMSE_dir,
    paste0("results_summary_", results_name, ".rda")
  ))
}

# end timer
end_time <- Sys.time()
time_dif <- end_time - start_time

# save the time value so you know how long the process took.  
saveRDS(time_dif, file = "timer_save.rda")

##### END PROCESS #####

# Optional: Email notification to self #####

# # Send email notification with Github Actions
# # Change the tag name on the github repo and trigger an email notification,  
# # then delete the tag so that the repo isn't cluttered.  
# # create a unique tag name using the current timestamp
# tag_name <- paste0("alert-", time_dif)
# 
# # create and push the tag locally pointing to your current commit
# system(paste("git tag", tag_name))
# system(paste("git push origin", tag_name)) # this will send email
# 
# # remove the tag so it doesn't clutter the repo
# system(paste("git tag -d", tag_name))
