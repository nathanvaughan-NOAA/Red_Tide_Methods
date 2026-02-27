# Using the cloud computing test as a template to explore a few different Red Tide Scenarios

# Load packages

#devtools::load_all("/SSMSE/")  # needs to be the cloned SSMSE repo

library(tidyverse)
library(SSMSE)

#Google App Password for emailing
PASSWORD <- ""
email_address <- ""

# Check versions
packageVersion("r4ss")
packageVersion("ss3sim")
packageVersion("SSMSE")

# Create a folder for the output in the working directory.
results_name <- "selectivity_all_yrs"
run_SSMSE_dir <- file.path("./runs_output")
run_res_path <- file.path(run_SSMSE_dir, paste0("results_", results_name))
if (!dir.exists(run_res_path)) {
  dir.create(run_res_path, recursive = TRUE)
}

# OM locations
model_SSMSE_dir <- file.path("base_models")
default <- file.path(model_SSMSE_dir, "default_sigmaR")

# number of simulation years
projyrs<-30

# to get the names of parameter values
ctl <- r4ss::SS_readctl(file.path(default, "red_grouper_1986_2017_RedTideFleet.ctl"), 
                        version = "3.30", 
                        use_datlist = TRUE, 
                        datlist = file.path(default, "data.ss_new"))
dat <- r4ss::SS_readdat(file.path(default, "red_grouper_1986_2017_RedTideFleet.dat"))

##### Add process error through rec devs #####
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

##### sample structure #####
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

##### Sample Structure Changes ####

# Adjusting sample structure to include future red tide events

sample_struct_no_rt_x_rt_2 <- add_sample_struct_FixedCatches(sample_struct, om_on = FALSE)
sample_struct_rt_2_x_no_rt <- add_sample_struct_FixedCatches(sample_struct, em_on = FALSE)
sample_struct_rt_2_x_rt_2 <- add_sample_struct_FixedCatches(sample_struct)
sample_struct_no_rt_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, om_on = FALSE, rt_year_em = seq(from = 2018, to = 2047, by = 1))
sample_struct_rt_2_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, rt_year_em = seq(from = 2018, to = 2047, by = 1))
sample_struct_rep_3_x_all_yrs <- add_sample_struct_FixedCatches(sample_struct, rt_year_om = seq(from = 2018, to = 2047, by = 3), rt_year_em = seq(from = 2018, to = 2047, by = 1))
sample_struct_rt_2_x_rt_2_fixed <- add_sample_struct_FixedCatches(sample_struct, em_fixed = 1)

# create a list of sample structures for each OM/MP run. 
sample_struct_list_all <- list(
  "no_rt" = sample_struct,
  "no_rt_x_rt_2" = sample_struct_no_rt_x_rt_2,
  "rt_2_x_no_rt" = sample_struct_rt_2_x_no_rt,
  "rt_2_x_rt_2" = sample_struct_rt_2_x_rt_2,
  "no_rt_x_all_yrs" = sample_struct_no_rt_x_all_yrs,
  "rt_2_x_all_yrs" = sample_struct_rt_2_x_all_yrs, 
  "rep_3_x_all_yrs" = sample_struct_rep_3_x_all_yrs, 
  "rt_2_x_rt_2_fixed" = sample_struct_rt_2_x_rt_2_fixed
)

##### Scenario Selection #####

# global settings for all SSMSE runs
base_params <- list(
  iter_vec        = 10,
  out_dir_scen_vec = normalizePath(run_res_path),
  run_EM_last_yr  = TRUE,
  MS_vec          = "EnvirEM",
  nyrs_vec        = projyrs,
  nyrs_assess_vec = 3,
  future_om_list  = future_OM_list_recdevs,
  run_parallel    = TRUE,
  seed            = 12345,
  # Normalize these once here
  OM_in_dir_vec   = normalizePath(default),
  EM_in_dir_vec   = normalizePath(default)
)

# use modifyList() to adjust the run_SSMSE parameters

# Core 4
no_rt <- modifyList(base_params, list(scen_name_vec = "no_rt", sample_struct_list = list("no_rt" = sample_struct)))
no_rt_x_rt_2 <- modifyList(base_params, list(scen_name_vec = "no_rt_x_rt_2", sample_struct_list = list("no_rt_x_rt_2" = sample_struct_no_rt_x_rt_2)))
rt_2_x_no_rt <- modifyList(base_params, list(scen_name_vec = "rt_2_x_no_rt", sample_struct_list = list("rt_2_x_no_rt" = sample_struct_rt_2_x_no_rt)))
rt_2_x_rt_2 <- modifyList(base_params, list(scen_name_vec = "rt_2_x_rt_2", sample_struct_list = list("rt_2_x_rt_2" = sample_struct_rt_2_x_rt_2)))

# All years
no_rt_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "no_rt_x_all_yrs", sample_struct_list = list("no_rt_x_all_yrs" = sample_struct_no_rt_x_all_yrs)))
rt_2_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "rt_2_x_all_yrs", sample_struct_list = list("rt_2_x_all_yrs" = sample_struct_rt_2_x_all_yrs)))
rep_3_x_all_yrs <- modifyList(base_params, list(scen_name_vec = "rep_3_x_all_yrs", sample_struct_list = list("rep_3_x_all_yrs" = sample_struct_rep_3_x_all_yrs)))

# Fixed EM
rt_2_x_rt_2_fixed <- modifyList(base_params, list(scen_name_vec = "rt_2_x_rt_2_fixed", sample_struct_list = list("rt_2_x_rt_2_fixed" = sample_struct_rt_2_x_rt_2_fixed)))

# Selectivity

# get selectivity models
flat <- file.path(model_SSMSE_dir, "flat")
young <- file.path(model_SSMSE_dir, "young")
old <- file.path(model_SSMSE_dir, "old")
mid <- file.path(model_SSMSE_dir, "mid")

flat_x_flat_all_yrs <- modifyList(base_params, list(scen_name_vec = "flat_x_flat_all_yrs", sample_struct_list = list("flat_x_flat_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(flat), EM_in_dir_vec = normalizePath(flat)))
young_x_young_all_yrs <- modifyList(base_params, list(scen_name_vec = "young_x_young_all_yrs", sample_struct_list = list("young_x_young_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(young), EM_in_dir_vec = normalizePath(young)))
old_x_old_all_yrs <- modifyList(base_params, list(scen_name_vec = "old_x_old_all_yrs", sample_struct_list = list("old_x_old_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(old), EM_in_dir_vec = normalizePath(old)))
mid_x_mid_all_yrs <- modifyList(base_params, list(scen_name_vec = "mid_x_mid_all_yrs", sample_struct_list = list("mid_x_mid_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(mid), EM_in_dir_vec = normalizePath(mid)))

flat_x_young_all_yrs <- modifyList(base_params, list(scen_name_vec = "flat_x_young_all_yrs", sample_struct_list = list("flat_x_young_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(flat), EM_in_dir_vec = normalizePath(young)))
flat_x_old_all_yrs <- modifyList(base_params, list(scen_name_vec = "flat_x_old_all_yrs", sample_struct_list = list("flat_x_old_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(flat), EM_in_dir_vec = normalizePath(old)))
flat_x_mid_all_yrs <- modifyList(base_params, list(scen_name_vec = "flat_x_mid_all_yrs", sample_struct_list = list("flat_x_mid_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(flat), EM_in_dir_vec = normalizePath(mid)))

young_x_flat_all_yrs <- modifyList(base_params, list(scen_name_vec = "young_x_flat_all_yrs", sample_struct_list = list("young_x_flat_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(young), EM_in_dir_vec = normalizePath(flat)))
young_x_old_all_yrs <- modifyList(base_params, list(scen_name_vec = "young_x_old_all_yrs", sample_struct_list = list("young_x_old_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(young), EM_in_dir_vec = normalizePath(old)))
young_x_mid_all_yrs <- modifyList(base_params, list(scen_name_vec = "young_x_mid_all_yrs", sample_struct_list = list("young_x_mid_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(young), EM_in_dir_vec = normalizePath(mid)))

old_x_flat_all_yrs <- modifyList(base_params, list(scen_name_vec = "old_x_flat_all_yrs", sample_struct_list = list("old_x_flat_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(old), EM_in_dir_vec = normalizePath(flat)))
old_x_young_all_yrs <- modifyList(base_params, list(scen_name_vec = "old_x_young_all_yrs", sample_struct_list = list("old_x_young_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(old), EM_in_dir_vec = normalizePath(young)))
old_x_mid_all_yrs <- modifyList(base_params, list(scen_name_vec = "old_x_mid_all_yrs", sample_struct_list = list("old_x_mid_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(old), EM_in_dir_vec = normalizePath(mid)))

mid_x_flat_all_yrs <- modifyList(base_params, list(scen_name_vec = "mid_x_flat_all_yrs", sample_struct_list = list("mid_x_flat_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(mid), EM_in_dir_vec = normalizePath(flat)))
mid_x_young_all_yrs <- modifyList(base_params, list(scen_name_vec = "mid_x_young_all_yrs", sample_struct_list = list("mid_x_young_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(mid), EM_in_dir_vec = normalizePath(young)))
mid_x_old_all_yrs <- modifyList(base_params, list(scen_name_vec = "mid_x_old_all_yrs", sample_struct_list = list("mid_x_old_all_yrs" = sample_struct_rt_2_x_all_yrs), OM_in_dir_vec = normalizePath(mid), EM_in_dir_vec = normalizePath(old)))

# put the scenarios you want to run into a list

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

# selectivity
all_scenarios <- list(
  rt_2_x_rt_2,
  flat_x_flat_all_yrs,
  young_x_young_all_yrs,
  old_x_old_all_yrs,
  mid_x_mid_all_yrs,
  flat_x_young_all_yrs,
  flat_x_old_all_yrs,
  flat_x_mid_all_yrs,
  young_x_flat_all_yrs,
  young_x_old_all_yrs,
  young_x_mid_all_yrs,
  old_x_flat_all_yrs,
  old_x_young_all_yrs,
  old_x_mid_all_yrs,
  mid_x_flat_all_yrs,
  mid_x_young_all_yrs,
  mid_x_old_all_yrs
)

##### RUN SSMSE #####

# start timer
start_time <- Sys.time()

# walk through the scenario list and run_SSMSE
walk(all_scenarios, ~exec(run_SSMSE, !!!.x))  # !!! makes the scenario list into arguements that can be used by a function

# make a summary with all the outputs in the same folder
summary <- SSMSE::SSMSE_summary_all(run_res_path)
saveRDS(summary, file = file.path(run_SSMSE_dir, paste0("results_summary_", results_name, ".rda")))

# end timer
end_time <- Sys.time()
end_time - start_time

##### Email when done #####

# send email to indicate the run is done
library(blastula)

# Create the email
email <- compose_email(
  body = md("Your R job is **complete!**")
)

Sys.setenv(SMTP_PASSWORD = PASSWORD)

# Send via SMTP (Gmail)
smtp_send(
  email,
  from = email_address,
  to = email_address,
  subject = "R Script Complete",
  credentials = creds_envvar(
    user = email_address,
    provider = "gmail",
    pass_envvar = "SMTP_PASSWORD",
    use_ssl = TRUE
  )
)
