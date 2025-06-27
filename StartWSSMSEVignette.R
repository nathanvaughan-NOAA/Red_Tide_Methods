# Install and load packages
# Might need to uncomment next line to install packages
# options(download.file.method = "libcurl")
# remotes::install_github("r4ss/r4ss")
# remotes::install_github("ss3sim/ss3sim")
# remotes::install_github("nmfs-fish-tools/SSMSE")
devtools::load_all("C://Users/apn26/Documents/SSMSE/")

# library(r4ss)
# library(ss3sim)
# library(SSMSE)
 library(tidyverse)

packageVersion("r4ss") # 1.49.3
packageVersion("ss3sim") # 1.21.0
packageVersion("SSMSE") # 0.2.8

# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("run_SSMSE-ex")
dir.create(run_SSMSE_dir)

OM_mod_path <- file.path("C:","Users", "apn26", "Documents", "CIMAS", "Red_Tide_Methods","SS_models",
                         "Red grouper",
                         "version_3.30.18_reverseTB_upfore_expdat") # get OM file path
#I copied the om in this path
EM_mod_path <- file.path("C:","Users", "apn26", "Documents", "CIMAS", "Red_Tide_Methods","run_SSMSE-ex", "Starting_EM") # get OM file path

# To develop a new OM and modify 1 existing parameter value:
# develop_OMs(OM_name="base", out_dir=run_SSMSE_dir, refit_OMs=FALSE, hess=FALSE)
# to get the names of parameter values
ctl <- r4ss::SS_readctl(file.path(OM_mod_path, "red_grouper_1986_2017_RedTideFleet.ctl"))
str(ctl)
names(ctl)
rownames(ctl$SR_parms)
#Warning message:
#  In SS_readdat_3.30(file = file, verbose = verbose, section = section) :
#  Lines of all zero length comp found. SS will exit on error if a line of comps is all zeroes and year is positive. Line(s) 33
#     Years of composition data where all are zero are included but the year is negative
dat <- r4ss::SS_readdat(file.path(OM_mod_path, "red_grouper_1986_2017_RedTideFleet.dat"))
# names(dat)
#Warning message:
#  In SS_readdat_3.30(file = file, verbose = verbose, section = section) :
#  Lines of all zero length comp found. SS will exit on error if a line of comps is all zeroes and year is positive. Line(s) 33
#     Years of composition data where all are zero are included but the year is negative


## Add process error through rec devs ####
# start from a list created by a helper function
template_mod_change <- create_future_om_list()
template_mod_change
# add rec devs
rec_dev_specify <- template_mod_change[[1]] # unlists the template
rec_dev_specify$pars <- "rec_devs"
rec_dev_specify$scen <- c("replicate", "all")
rec_dev_specify$input$first_yr_averaging <- ctl$MainRdevYrFirst
rec_dev_specify$input$last_yr_averaging <- ctl$MainRdevYrLast
# The following 2 lines suggest that this change is immediately applied in year
# 101, with no transitory period for using sd 0 to the new sd.
rec_dev_specify$input$last_yr_orig_val <- dat$endyr
rec_dev_specify$input$first_yr_final_val <- dat$endyr + 1
rec_dev_specify$input$ts_param <- "sd"
rec_dev_specify$input$value <- NA
rec_dev_specify

#future OM list
future_OM_list_recdevs<-list(rec_dev_specify)


################################################################################
#  Adding observation error: Specify how to sample data from the Operating model
################################################################################

# The argument sample_struct specifies the structure for sampling from the OM
# (and passing to the EM). The function create_sample_struct can be used to 
# construct a simple sampling structure consistent with an input data file:

datfile<-dat
projyrs<-15
#sample_struct<-create_sample_struct(dat=datfile, nyrs=projyrs)
sample_struct<-create_sample_struct_biased(dat=datfile, nyrs=projyrs)
# LOTS OF WARNINGS/ERRORS, so need to go input by input and enter errors
# By default, create_sample_struct identifies sampling patterns from the 
# historical period of the OM and replicates those patterns in the projection 
# period. Note that length comp (lencomp) includes an NA value for year. 
# This is because no consistent pattern was identified, so the user must 
# define their own input.
sample_struct
# currently includes catch, CPUE, lencomp, agecomp, meanbodywt, MeanSize_at_Age_obs
# discard_data added for red snapper MSE
# EM2OMcatch_bias and EM2OMdiscard_bias added for FES MSE
# Fixed catch added to handle shrimp bycatch or red tide kills externally


################
# Landings Error 

sample_struct$catch  #currently NA for 2018-2032
table(dat$catch$fleet,dat$catch$catch_se) # this table shows the number of catch se by fleet
# Specify SEs as mean of past 20 yrs of observed catch
# for(i in unique(sample_struct$catch$FltSvy)){
#    s<-mean(dat$catch[dat$catch$fleet==i & dat$catch$year>2010,]$catch_se)
#   sample_struct$catch[sample_struct$catch$FltSvy==i,]$SE<-rep(s,projyrs)
# }
# Specify SEs from recent error 
# for(i in unique(sample_struct$catch$FltSvy)){
#    s<-dat$catch[dat$catch$fleet==i & dat$catch$year==2017,]$catch_se
#   sample_struct$catch[sample_struct$catch$FltSvy==i,]$SE<-rep(s,projyrs)
#  }
# # recreational will continue to have higher uncertainty than commercial
# sample_struct$catch

sample_struct$catch <- sample_struct$catch %>%
  filter(FltSvy %in% c(1,2,4)) %>% 
  rename(fleet = FltSvy) %>%
  left_join(filter(dat$catch, year > 2000)) %>%
  summarise(SE = mean(catch_se), .by = c(Yr, Seas, fleet)) %>%
  rename(FltSvy = fleet)

sample_struct$catch

# # Code not working above, so need to do this manually
# sample_struct$catch <- sample_struct$catch[0, ] # reset dataframe
# for (i in unique(dat$catch$fleet)[c(1:2, 4)]) {
#   #exclude trap since landings stopped in 2007 & red tide
#   s <- mean(dat$catch[dat$catch$fleet == i &
#                         dat$catch$year > 2000, ]$catch_se)
#   x <- data.frame(
#     Yr = (dat$endyr + 1):(dat$endyr + projyrs),
#     Seas = rep(1, projyrs),
#     FltSvy = rep(i, projyrs),
#     SE = rep(s, projyrs)
#   )
#   sample_struct$catch <- rbind(sample_struct$catch, x)
# }#end if statement
# sample_struct$catch


#######################
# EM2OMcatch_bias Error 

#Remove bias for trap fleet and red tide
# sample_struct$EM2OMcatch_bias<-sample_struct$EM2OMcatch_bias[sample_struct$EM2OMcatch_bias$FltSvy!=3,]
# sample_struct$EM2OMcatch_bias<-sample_struct$EM2OMcatch_bias[sample_struct$EM2OMcatch_bias$FltSvy!=5,]
sample_struct$EM2OMcatch_bias <- filter(sample_struct$EM2OMcatch_bias, FltSvy %in% c(1,2,4))
#Default values of 1 (no bias)
sample_struct$EM2OMcatch_bias

#############
# Fixed catch

sample_struct$FixedCatch
sample_struct$FixedCatch<-NULL


############
# CPUE Error

proj_indecies <- dat$CPUE %>% 
  filter(year > dat$endyr - 2) %>% 
  distinct(index)

sample_struct$CPUE <- dat$CPUE %>% filter(index %in% proj_indecies$index) %>% 
  group_by(index) %>% 
  summarize(SE = mean(se_log[year > 2000], na.rm = TRUE), .groups = "drop") %>%
  rowwise() %>% 
  mutate(
    projection = list(
      data.frame(
        Yr = (dat$endyr+1):(dat$endyr + projyrs),
        Seas = 7, 
        FltSvy = index,
        SE = SE
      )
    )
  )%>%
  select(projection) %>%
  unnest(cols = c(projection))

head(sample_struct$CPUE)
# sample_struct$CPUE <- sample_struct$CPUE[0, ] # reset dataframe
# for (i in  unique(dat$CPUE$index)) {
#   # make loop
#   # delete future survey values if the survey has not operated in 2016 or 2017.
#   # if(nrow(dat$CPUE[dat$CPUE$index==i & dat$CPUE$year > dat$endyr-2,])==0){
#   #   sample_struct$CPUE<-sample_struct$CPUE[sample_struct$CPUE$FltSvy!=i,]
#   # }
#   # if the survey has operated in 2016 or 2017, assume yearly operation into the future
#   if (nrow(dat$CPUE[dat$CPUE$index == i &
#                     dat$CPUE$year > dat$endyr - 2, ]) > 0) {
#     s <- mean(dat$CPUE[dat$CPUE$index == i &
#                          dat$CPUE$year > 2000, ]$se_log)
#     x <- data.frame(
#       Yr = (dat$endyr + 1):(dat$endyr + projyrs),
#       Seas = rep(7, projyrs),
#       FltSvy = rep(i, projyrs),
#       SE = rep(s, projyrs)
#     )
#     sample_struct$CPUE <- rbind(sample_struct$CPUE, x)
#   }#end if statement
# }# END CPUE section
# sample_struct$CPUE


################
# Discards Error

sample_struct$discard_data
table(dat$discard_data$Flt,dat$discard_data$Std_in)
sample_struct$discard_data <- sample_struct$discard_data %>% 
  filter(FltSvy != 3)
#Fleet 3 is commercial trap, which is no longer active so remove it
#sample_struct$discard_data<-sample_struct$discard_data[which(sample_struct$discard_data$FltSvy!=3),]
sample_struct$discard_data


#######################
# EM2OMdiscard_bias Error 

#Remove bias for trap fleet 
sample_struct$EM2OMdiscard_bias <- sample_struct$EM2OMdiscard_bias %>% 
  filter(FltSvy != 3)
sample_struct$EM2OMdiscard_bias
#Default values of 1 (no bias)


###############
# Lencomp Error  

#sample_struct_test$lencomp

last_20yr <- dat$lencomp %>% 
  filter(Yr > dat$endyr - 20)

last_2yr <- dat$lencomp %>% 
  filter(Yr > dat$endyr - 2)



sample_struct$lencomp
table(datfile$lencomp$Seas)   #7
table(datfile$lencomp$Gender) #1
table(datfile$lencomp$Part)   #0, 1, 2
table(datfile$lencomp$FltSvy,datfile$lencomp$Part)  
# note: month = 7 and sex = 1
sample_struct$lencomp <- sample_struct$lencomp[0, ] # reset dataframe
for (i in  unique(dat$lencomp$FltSvy)) {
  # unique fleet/surveys w len comp data
  sublc20 <- dat$lencomp[dat$lencomp$FltSvy == i &
                           dat$lencomp$Yr > dat$endyr - 20, ] # get subset of lencomps over past 20 yrs
  sublc2 <- dat$lencomp[dat$lencomp$FltSvy == i &
                          dat$lencomp$Yr > dat$endyr - 2, ] # get subset of lencomps over past 2 yrs
  if (nrow(sublc2) > 0) {
    # only assume projection values if they have been observed in the last 2 years, otherwise assume truncated data series
    
    sublc20_P0 <- sublc20[sublc20$Part == 0, ] # get combined (Part==0) lencomp projections
    if (nrow(sublc20_P0) > 0) {
      # only project retained catch if observed over past 20 yrs
      if ((dat$endyr + 1) %in% sublc20_P0$Yr) {
        Nsamp_t <- c(sublc20_P0$Nsamp[sublc20_P0$Yr == TermYr], rep(mean(sublc20_P0$Nsamp), projyrs -
                                                                      1)) # use terminal year value if present in dat file
      } else{
        Nsamp_t <- rep(mean(sublc20_P0$Nsamp), projyrs) # otherwise, use mean Nsamps observed over past 20 yrs for all proj yrs
      }
      # get retained or combined lencomp projection dataframe
      kept0_lf <- data.frame(
        Yr = (dat$endyr + 1):(dat$endyr + projyrs),
        Seas = rep(7, projyrs),
        FltSvy = rep(i, projyrs),
        Sex = rep(0, projyrs),
        Part = rep(0, projyrs),
        Nsamp = Nsamp_t
      )
      
    } else{
      kept0_lf <- NULL # make retained/combined dataframe empty if no observations for fleet i
    } #end if-else retained/combined lencomp data
    
    sublc20_P1 <- sublc20[sublc20$Part == 1, ] # get discarded lencomps when exist
    
    if (nrow(sublc20_P1) > 0) {
      frq1 <- round(10 / nrow(sublc20_P1)) # take average frequency of discard lengcomps as observed over past 10 yrs
      Yr_t <- seq(
        from = dat$endyr + 1,
        to = dat$endyr + projyrs,
        by = frq1
      ) # proj discard lencomp observations w same frequency
      # get discard projected lencomps dataframe
      disc_lf <- data.frame(
        Yr = Yr_t,
        Seas = rep(7, length(Yr_t)),
        FltSvy = rep(i, length(Yr_t)),
        Sex = rep(0, length(Yr_t)),
        Part = rep(1, length(Yr_t)),
        Nsamp = rep(mean(sublc20_P1$Nsamp), length(Yr_t))
      )
    } else{
      disc_lf <- NULL # make discard dataframe empty if no discard observations for fleet i
    } #end if-else discard lencomp data
    
    sublc20_P2 <- sublc20[sublc20$Part == 2, ] # get retained (Part==2) lencomp projections
    if (nrow(sublc20_P2) > 0) {
      # only project retained catch if observed over past 20 yrs
      if ((dat$endyr + 1) %in% sublc20_P2$Yr) {
        Nsamp_t <- c(sublc20_P2$Nsamp[sublc20_P2$Yr == TermYr], rep(mean(sublc20_P2$Nsamp), projyrs -
                                                                      1)) # use terminal year value if present in dat file
      } else{
        Nsamp_t <- rep(mean(sublc20_P2$Nsamp), projyrs) # otherwise, use mean Nsamps observed over past 20 yrs for all proj yrs
      }
      # get retained or combined lencomp projection dataframe
      kept2_lf <- data.frame(
        Yr = (dat$endyr + 1):(dat$endyr + projyrs),
        Seas = rep(7, projyrs),
        FltSvy = rep(i, projyrs),
        Sex = rep(0, projyrs),
        Part = rep(2, projyrs),
        Nsamp = Nsamp_t
      )
      
    } else{
      kept2_lf <- NULL # make retained/combined dataframe empty if no observations for fleet i
    } #end if-else retained/combined lencomp data
    
    sample_struct$lencomp <- rbind(sample_struct$lencomp, disc_lf, kept0_lf, kept2_lf) # append discard and retained/combined lencomp dataframes for fleet i to the previous fleets
    
  }# end if statement
}# end length section
sample_struct$lencomp



###############
# Agecomp Error

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
sample_struct$agecomp <- sample_struct$agecomp[0, ] # reset dataframe
for (i in  unique(dat$agecomp$FltSvy)) {
  # unique fleet/surveys w len comp data
  subac20 <- dat$agecomp[dat$agecomp$FltSvy == i &
                           dat$agecomp$Yr > dat$endyr - 20, ] # get subset of agecomps over past 20 yrs
  subac2 <- dat$agecomp[dat$agecomp$FltSvy == i &
                          dat$agecomp$Yr > dat$endyr - 2, ] # get subset of agecomps over past 2 yrs
  if (nrow(subac2) > 0) {
    # only assume projection values if they have been observed in the last 2 years, otherwise assume truncated data series
    # create age sampling projection data frame
    agec <- data.frame(
      Yr = (dat$endyr + 1):(dat$endyr + projyrs),
      Seas = rep(subac2$Seas[1], projyrs),
      FltSvy = rep(i, projyrs),
      Sex = rep(1, projyrs),
      Part = rep(2, projyrs),
      Ageerr = rep(1, projyrs),
      Lbin_lo = rep(-1, projyrs),
      Lbin_hi = rep(-1, projyrs),
      Nsamp = rep(mean(subac20$Nsamp), projyrs)
    )
    
    sample_struct$agecomp <- rbind(sample_struct$agecomp, agec) # append survey i agecomp dataframe to previous fleets
    
  }#end if projection
} #end i forloop for agecomp section
sample_struct$agecomp


############# 
# meanbody wt

sample_struct$meanbodywt
sample_struct$meanbodywt <- NULL


##################
# mean size at age 

sample_struct$MeanSize_at_Age_obs
sample_struct$MeanSize_at_Age_obs <- NULL


# create a list of sample structures for each OM/MP run. 
sample_struct_list_all <- list("base" = sample_struct)


###########################################
#  Examine the management procedure used  #
###########################################

# 1. Conduct a stock assessment every 3 years to get stock status.
# 2. Forecast from this stock assessment using the SS forecast file to get future
# catch. 
# 3. Put this forecasted catch (without implementation error, in the case of 
# this example) back into the OM. Extend the OM forward in time to get the true 
# values for the population.

# Let's take a look at step 2 in the management procedure, which is implemented
# using the forecasting module in SS. We will examine the forecast file to better 
# understand how catches will be forecasted from the assessment. We will use the
# assessment. For a full MSE analysis, it is likely that multiple management 
# procedures would be compared.

fore<-r4ss::SS_readforecast(file=file.path(OM_mod_path, "forecast.ss"), verbose=FALSE)
fore$Forecast
#fore$Forecast = 1 means our forecasts from the assessment will use fishing 
#mortality (F) to achieve an FSPR value. 

fore$Btarget
#Based on fore$Btarget, the relative biomass target is 30% of unfished spawning 
#stock biomass. 

#Note also that the control rule fore$BforconstantF and fore$BfornoF values 
#are set low to make it unlikely that they will be used (these parameters are
#used for a ramp harvest control rule, which we do not want to use here):
fore$BforconstantF
fore$BfornoF

#fore$Flimitfraction is set to 1 so that the forecasted catch is set equal to the 
#overfishing limit (for simplicity):
fore$Flimitfraction

#The number of forecast years:
fore$Nforecastyrs


###########
## RUN ####
###########

run_res_path <- file.path(run_SSMSE_dir, "results")
dir.create(run_res_path)
res<-run_SSMSE(
  scen_name_vec=c('base'), # name of the scenario
  out_dir_scen_vec = run_res_path, # directory in which to run the scenario
  iter_vec = c(3), # run with 3 iterations each
  OM_name_vec = NULL, # specify directories instead
  OM_in_dir_vec = file.path(OM_mod_path), # OM files
  EM_name_vec = NULL, # 
  EM_in_dir_vec = file.path(EM_mod_path),
  run_EM_last_yr = TRUE,
  MS_vec = c("EM"), # The management strategy is specified in the EM
  nyrs_vec = c(projyrs), # Years to project OM forward
  nyrs_assess_vec = c(3), # Years between assessments
  future_om_list = future_OM_list_recdevs,
  run_parallel = TRUE, # Run iterations in parallel
  sample_struct_list = sample_struct_list_all, # How to sample data for running the EM.
  sample_struct_hist_list = NULL, # because this is null, will just use sampling
  # as in the current OM data file for the historical period.
  seed = 12345
) # Set a fixed integer seed that allows replication

# Starting iteration 1.
# Finished running and sampling OM for the historical period for iteration 1.
# Finished getting catch (years 2018 to 2020) to feed into OM for iteration 1.
# Finished updating OM through year 2020. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2020.




# Finished getting catch (years 2021 to 2023) to feed into OM for iteration 1.
# Finished updating OM through year 2023. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2023.
# Finished getting catch (years 2024 to 2026) to feed into OM for iteration 1.
# Finished updating OM through year 2026. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2026.
# Finished getting catch (years 2027 to 2029) to feed into OM for iteration 1.
# Finished updating OM through year 2029. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2029.
# Finished getting catch (years 2030 to 2032) to feed into OM for iteration 1.
# Finished updating OM through year 2032. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2032.
# Finished getting catch (years 2033 to 2035) to feed into OM for iteration 1.
# Finished iteration 1.
# Starting iteration 2.
# Finished running and sampling OM for the historical period for iteration 2.
# Finished getting catch (years 2018 to 2020) to feed into OM for iteration 2.
# Finished updating OM through year 2020. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2020.
# Finished getting catch (years 2021 to 2023) to feed into OM for iteration 2.
# Finished updating OM through year 2023. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2023.
# Finished getting catch (years 2024 to 2026) to feed into OM for iteration 2.
# Finished updating OM through year 2026. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2026.
# Finished getting catch (years 2027 to 2029) to feed into OM for iteration 2.
# Finished updating OM through year 2029. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2029.
# Finished getting catch (years 2030 to 2032) to feed into OM for iteration 2.
# Finished updating OM through year 2032. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2032.
# Finished getting catch (years 2033 to 2035) to feed into OM for iteration 2.
# Finished iteration 2.
# Starting iteration 3.
# Finished running and sampling OM for the historical period for iteration 3.
# Finished getting catch (years 2018 to 2020) to feed into OM for iteration 3.
# Finished updating OM through year 2020. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2020.
# Finished getting catch (years 2021 to 2023) to feed into OM for iteration 3.
# Finished updating OM through year 2023. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2023.
# Finished getting catch (years 2024 to 2026) to feed into OM for iteration 3.
# Finished updating OM through year 2026. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2026.
# Finished getting catch (years 2027 to 2029) to feed into OM for iteration 3.
# Finished updating OM through year 2029. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2029.
# Finished getting catch (years 2030 to 2032) to feed into OM for iteration 3.
# Finished updating OM through year 2032. Now sample values to return as data to EM.
# Finished running and sampling OM through year 2032.
# Finished getting catch (years 2033 to 2035) to feed into OM for iteration 3.
# Finished iteration 3.
# Completed all iterations for scenario base
# Completed all SSMSE scenarios


#Warning messages:
# 1: In SS_readdat_3.30(file = file, verbose = verbose, section = section) :
#    Lines of all zero length comp found. SS will exit on error if a line of comps is all zeroes and year is positive. Line(s) 33
# 3: In clean_init_mod_files(OM_out_dir = OM_out_dir, EM_out_dir = EM_out_dir,  ... :
#    Original OM model files have variance adjustment factors specified.This may have unintended effects such as causeing sample sizes to differ from those specified.If you didn't do this intentionally we suggest turning of variance adjustments.
# 4: In clean_init_mod_files(OM_out_dir = OM_out_dir, EM_out_dir = EM_out_dir,  ... :
#    Original EM model files have variance adjustment factors specified.This may have unintended effects such as causeing sample sizes to differ from those specified.If you didn't do this intentionally we suggest turning of variance adjustments.
# 5: In create_OM(OM_out_dir = OM_out_dir, overwrite = TRUE,  ... :
#   Tail compression was on for some fleets in length comp and/or age comp for the operating model, but needs to beturned off in an operating model. Turning off tail compression. Note that this may change expected values for historical age or  length composition.
# 6: In change_yrs_fcast(fcast, make_yrs_rel = (init_loop ==  ... :
#   Removing ForeCatch from the EM forecasting file.
# 7: In utils::write.table(x = search_log, file = file.path(OM_dir,  ... :
#   appending column names to file


###################
# Process Results #
###################

library(ggplot2) # use install.packages("ggplot2") to install package if needed
library(tidyr) # use install.packages("tidyr") to install package if needed
library(dplyr) # use install.packages("dplyr") to install package if needed
library(gridExtra) # use install.packages("gridExtra") to install package if needed

###################
# Summarize Results

# SSMSE_summary_all = summarize the model results in a list of 3 dataframes,
# one for scalar outputs (named scalar), one for timeseries outputs (ts),
# and one for derived quantities (dq). 

summary <- SSMSE_summary_all(run_res_path)

##########################
# Simple Convergence Check 

#Check there are no params on bounds or SSB that is way too small or way too large

check_convergence <- function(summary, min_yr = 101, max_yr = 120, n_EMs = 5) {
  require(dplyr) # note: not the best way to do this
  if (any(!is.na(summary$scalar$params_on_bound))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary$ts$model_type <- ifelse(grepl("_EM_", summary$ts$model_run), "EM", "OM")
  calc_SSB <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>%
    select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>%
    filter(model_type == "OM") %>%
    rename(SpawnBio_OM = SpawnBio) %>%
    select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>%
    filter(model_type == "EM") %>%
    rename(SpawnBio_EM = SpawnBio) %>%
    select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>%
    mutate(SSB_ratio = SpawnBio_EM / SpawnBio_OM)
  filter_SSB <- bind_vals %>%
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if (nrow(filter_SSB) > 0) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}
values <- check_convergence(summary = summary, min_yr = (dat$endyr+1), max_yr=max(dat$endyr)+projyrs, n_EMs = 2)
values
## No params on bounds
## All SSBs in EM are no greater than double and no less than half SSB vals in the OM
                       
########################## 
# Create folder for plots

if(!file.exists(paste0(run_res_path,"/plots/"))) dir.create(paste0(run_res_path,"/plots/"))

##############################
# Summarize Timeseries Results

colnames(summary$ts)
#ts includes SSB, recruitment, retained biomass per fleet, retained numbers per fleet, 
#dead biomass per fleet, dead numbers per fleet, F per fleet, SPRratio, and rec_devs

# SSB by year and model run (subset for OM vs initEM)
p_SSB<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run)[6:7])), 
              ggplot2::aes(x = year, y = SpawnBio)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Spawning Stock Biomass (metric tons)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_SSB_OMvsinitEM.jpg"),width=6.5,height=4)

# SSB by year and model run
p_SSB<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
              ggplot2::aes(x = year, y = SpawnBio)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Spawning Stock Biomass (metric tons)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_SSB.jpg"),width=6.5,height=4)

# SPR by year and model run
ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
       ggplot2::aes(x = year, y = SPRratio)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Spawner Potential Ratio") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_SPR.jpg"),width=6.5,height=4)

# Recruitment by year and model run
p_Recr<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
               ggplot2::aes(x = year, y = Recruit_0)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Recruitment (1,000s of fish)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_Recruitment.jpg"),width=6.5,height=4)

# Recruitment by year and model run
ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
       ggplot2::aes(x = year, y = rec_dev)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Recruitment deviation") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RecDev.jpg"),width=6.5,height=4)

#Specify fleet names
Fleets<-c("Commercial Vertical Line","Commercial Longline","Commercial Trap",
          "Recreational","","Combined Video Survey","SEAMAP Groundfish Trawl",
          "NMFS Bottom Longline Survey","Recreational Private-Charter Survey",
          "FWRI Repetitive Time Drop Survey")

# Apical F by year and model run
p1_F<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
             ggplot2::aes(x = year, y = F_1)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[1]," Apical F")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ApicalF_",Fleets[1],".jpg"),width=6.5,height=4)

p2_F<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
             ggplot2::aes(x = year, y = F_2)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[2]," Apical F")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ApicalF_",Fleets[2],".jpg"),width=6.5,height=4)

p3_F<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
             ggplot2::aes(x = year, y = F_3)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[3]," Apical F")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ApicalF_",Fleets[3],".jpg"),width=6.5,height=4)

p4_F<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
             ggplot2::aes(x = year, y = F_4)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[4]," Apical F")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ApicalF_",Fleets[4],".jpg"),width=6.5,height=4)

jpeg(paste0(run_res_path,"/plots/_ts_ApicalF.jpg"),res=300,height=2400,width=3000)
grid.arrange(p1_F,p2_F+theme(legend.position="none"),
             p3_F+theme(legend.position="none"),
             p4_F+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

# Retained biomass by year and model run for each fishing fleet
p1_RetBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainB_1)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[1]," Retained Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedBiomass_",Fleets[1],".jpg"),width=6.5,height=4)

p2_RetBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainB_2)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[2]," Retained Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedBiomass_",Fleets[2],".jpg"),width=6.5,height=4)

p3_RetBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainB_3)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[3]," Retained Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedBiomass_",Fleets[3],".jpg"),width=6.5,height=4)

p4_RetBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainB_4)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[4]," Retained Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedBiomass_",Fleets[4],".jpg"),width=6.5,height=4)

jpeg(paste0(run_res_path,"/plots/_ts_RetainedBiomass.jpg"),res=300,height=2400,width=3000)
grid.arrange(p1_RetBio,p2_RetBio+theme(legend.position="none"),
             p3_RetBio+theme(legend.position="none"),
             p4_RetBio+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

# Dead biomass by year and model run for each fishing fleet 
p1_DeadBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadB_1)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[1]," Dead Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadBiomass_",Fleets[1],".jpg"),width=6.5,height=4)

p2_DeadBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadB_2)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[2]," Dead Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadBiomass_",Fleets[2],".jpg"),width=6.5,height=4)

p3_DeadBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadB_3)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[3]," Dead Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadBiomass_",Fleets[3],".jpg"),width=6.5,height=4)

p4_DeadBio<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadB_4)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[4]," Dead Biomass (mt)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadBiomass_",Fleets[4],".jpg"),width=6.5,height=4)

jpeg(paste0(run_res_path,"/plots/_ts_DeadBiomass.jpg"),res=300,height=2400,width=3000)
grid.arrange(p1_DeadBio,p2_DeadBio+theme(legend.position="none"),
             p3_DeadBio+theme(legend.position="none"),
             p4_DeadBio+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

# Retained numbers by year and model run for each fishing fleet
p1_RetNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainN_1)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[1]," Retained Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedNumbers_",Fleets[1],".jpg"),width=6.5,height=4)

p2_RetNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainN_2)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[2]," Retained Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedNumbers_",Fleets[2],".jpg"),width=6.5,height=4)

p3_RetNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainN_3)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[3]," Retained Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedNumbers_",Fleets[3],".jpg"),width=6.5,height=4)

p4_RetNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                  ggplot2::aes(x = year, y = retainN_4)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[4]," Retained Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_RetainedNumbers_",Fleets[4],".jpg"),width=6.5,height=4)

jpeg(paste0(run_res_path,"/plots/_ts_RetainedNumbers.jpg"),res=300,height=2400,width=3000)
grid.arrange(p1_RetNum,p2_RetNum+theme(legend.position="none"),
             p3_RetNum+theme(legend.position="none"),
             p4_RetNum+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

# Dead numbers by year and model run for each fishing fleet
p1_DeadNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadN_1)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[1]," Dead Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadNumbers_",Fleets[1],".jpg"),width=6.5,height=4)

p2_DeadNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadN_2)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[2]," Dead Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadNumbers_",Fleets[2],".jpg"),width=6.5,height=4)

p3_DeadNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadN_3)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[3]," Dead Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadNumbers_",Fleets[3],".jpg"),width=6.5,height=4)

p4_DeadNum<-ggplot(data = subset(summary$ts, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = deadN_4)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y=paste0(Fleets[4]," Dead Numbers (1000s)")) + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_DeadNumbers_",Fleets[4],".jpg"),width=6.5,height=4)

jpeg(paste0(run_res_path,"/plots/_ts_DeadNumbers.jpg"),res=300,height=2400,width=3000)
grid.arrange(p1_DeadNum,p2_DeadNum+theme(legend.position="none"),
             p3_DeadNum+theme(legend.position="none"),
             p4_DeadNum+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

######################################
# Summarize Derived Quantities Results

colnames(summary$dq)
#dq includes SSB, Recr, SPRratio, F, Bratio, Forecatch, OFLCatch, ForeCatchRet,
# and lnSPR

# Fishing Mortality by year and model run
p_F<-ggplot(data = subset(summary$dq, model_run %in% c(unique(summary$ts$model_run))), 
            ggplot2::aes(x = year, y = Value.F)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Exploitation Rate (biomass killed all ages/biomass killed 3+)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_F.jpg"),width=6.5,height=4)

# Forecasted catch by year and model run
p_ForeCatch<-ggplot(data = subset(summary$dq, model_run %in% c(unique(summary$ts$model_run))), 
                    ggplot2::aes(x = year, y = Value.ForeCatch)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Forecasted Catch (mt)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ForeCatch.jpg"),width=6.5,height=4)

# Forecasted retained catch by year and model run
p_ForeCatchret<-ggplot(data = subset(summary$dq, model_run %in% c(unique(summary$ts$model_run))), 
                       ggplot2::aes(x = year, y = Value.ForeCatchret)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Forecasted Retained Catch (mt)") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ForeCatchRet.jpg"),width=6.5,height=4)

# SSB ratio by year and model run
p_SSBratio<-ggplot(data = subset(summary$dq, model_run %in% c(unique(summary$ts$model_run))), 
                   ggplot2::aes(x = year, y = Value.Bratio)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  xlim(min(summary$ts$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="SSB Ratio (SSB / SSBunfished") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_SSBratio.jpg"),width=6.5,height=4)


jpeg(paste0(run_res_path,"/plots/_ts.jpg"),res=300,height=2400,width=3000)
grid.arrange(p_SSB,p_F+theme(legend.position="none"),
             p_SSBratio+theme(legend.position="none"),
             p_Recr+theme(legend.position="none"),
             nrow=2,ncol=3,widths=c(2,1,2),layout_matrix=rbind(c(1,1,2),
                                                               c(3,NA,4)))
dev.off()

###############################
# Summarize Parameter Estimates

colnames(summary$scalar)
#parameters (fixed and estimated)
#NLL
#Variance adjustments
#Model details

# 
for (i in 1:length(colnames(summary$scalar)))
{
  ggplot(data = subset(summary$scalar, model_run %in% c(unique(summary$scalar$model_run))), 
         ggplot2::aes(x = model_run, y = summary$scalar[,i], fill=as.factor(iteration))) +
    geom_bar(stat = "identity",position="dodge") +
    facet_wrap(. ~ scenario) +
    labs(x = "Model Run",y=colnames(summary$scalar[i])) + 
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 90, hjust = 1)) 
  ggsave(paste0(run_res_path,"/plots/",colnames(summary$scalar[i]),".jpg"),width=6.5,height=4)
}



################
# OUTPUT OM DATA

#library(r4ss)

n<-3 # number of simulations
# Combine directories for OM model runs
Direct<-c(paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_OM_OM"))
for (i in 2:n) {
  Direct<-c(Direct,c(paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_OM_OM")))
}

#Process OM model runs
OM <- SSgetoutput(getcomp=FALSE,dirvec=Direct)
OM_summary <- SSsummarize(OM)
str(OM_summary)
# only includes indices

########################
# OM Data Plot - Catch #  
########################

Catch<-rbind(OM$replist1$catch,OM$replist2$catch,OM$replist3$catch)
Catch$Sim<-c(rep(1,dim(Catch)[1]/n),rep(2,dim(Catch)[1]/n),rep(3,dim(Catch)[1]/n))

UnitsCom<-"Landings (Metric Tons)"
UnitsRec<-"Landings (1000s of Fish)"
TermYr<-dat$endyr # not OM end year (OM$replist1$endyr), but assessment terminal year 

#Color-blind friendly
library(RColorBrewer)
cols<-brewer.pal(length(Fleets), "Dark2")
nb.cols <- length(Fleets)
cols <- colorRampPalette(brewer.pal(length(Fleets), "Dark2"))(nb.cols)

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_Catch.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Catch$Fleet)[c(1:3)]){ #Commercial Fisheries only 
  for (j in 1:1){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         las=1,type="l",ylab=UnitsCom,xlab="Year",lty=3,pch=16,col="black",lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],axes=F,
         las=1,type="l",ylab=UnitsCom,xlab="Year",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
  }
  for (j in 2:2){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
  }
  for (j in n:n){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i])))
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$catch$SE[sample_struct$catch$FltSvy==i]),2)),bty="n")
}

for (i in unique(Catch$Fleet)[4]){ #Recreational Fisheries only
  for (j in 1:1){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         las=1,type="l",ylab=UnitsRec,xlab="Year",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         las=1,type="l",ylab=UnitsRec,xlab="Year",pch=16,col=cols[i],lwd=2,axes=F,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
  }
  for (j in 2:2){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
  }
  for (j in n:n){
    plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j],
         type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$catch$SE[sample_struct$catch$FltSvy==i]),2)),bty="n")
}
dev.off()


########################
# OM Data Plot - Index #  
########################

Index<-OM_summary$indices
colnames(Index)[dim(Index)[2]]<-"Sim"

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_Index.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(3,3),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Index$Fleet)){ #Indices Only
  for (j in 1:1){
    plot(Index$Obs[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         las=1,type="b",ylab="Relative Abundance",xlab="Year",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    plot(Index$Exp[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         las=1,type="b",ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
  }
  for (j in 2:2){
    plot(Index$Obs[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    plot(Index$Exp[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
  }
  for (j in n:n){
    plot(Index$Obs[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    plot(Index$Exp[Index$Fleet==i & Index$Sim==j]~Index$Yr[Index$Fleet==i & Index$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$CPUE$SE[sample_struct$CPUE$FltSvy==i]),2)),bty="n")
}
dev.off()
# NOTE: why are projection year indices = 1?

###########################
# OM Data Plot - Discards #  
###########################

Disc<-rbind(OM$replist1$discard,OM$replist2$discard,OM$replist3$discard)
Disc$Sim<-c(rep(1,dim(Disc)[1]/n),rep(2,dim(Disc)[1]/n),rep(3,dim(Disc)[1]/n))

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_Discards.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Disc$Fleet)){ #Fisheries only 
  for (j in 1:1){
    plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         las=1,type="b",ylab="Discards (1000s of Fish)",xlab="Year",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         las=1,type="b",ylab="",xlab="",pch=16,col=cols[i],lwd=2,axes=F,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
  }
  for (j in 2:2){
    plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
  }
  for (j in n:n){
    plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,
         ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$discard_data$SE[sample_struct$discard_data$FltSvy==i]),2)),bty="n")
}
dev.off()


##########################################
# OM Data Plot - Length Comps - Landings #  
##########################################

LC<-rbind(OM$replist1$len_comp_fit_table,OM$replist2$len_comp_fit_table,OM$replist3$len_comp_fit_table)
LC$Sim<-c(rep(1,dim(LC)[1]/n),rep(2,dim(LC)[1]/n),rep(3,dim(LC)[1]/n))

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_LenComp_N.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
# for (i in unique(LC$Fleet)[1:3]){ #Fisheries only
#   for (j in 1:1){
#     plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
#          las=1,type="b",ylab="Length Sample Size",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in 2:2){
#     plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in n:n){
#     plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
#   }
#   grid(col="lightgray")
#   abline(v=TermYr,lty=3,col="red")
#   legend("topright",paste0(Fleets[i]," Landings"),bty="n")
# }
# 
for (i in unique(LC$Fleet)[4:7]){ #Surveys only
  for (j in 1:1){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
         las=1,type="b",ylab="Length Sample Size",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in 2:2){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in n:n){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part %in% c(0,2)],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("topright",Fleets[i],bty="n")
}
dev.off()

##########################################
# OM Data Plot - Length Comps - Discards #  
##########################################

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_LenCompDiscards_N.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(LC$Fleet)[1:3]){
  for (j in 1:1){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part==1]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part==1],
         las=1,type="b",ylab="Length Sample Size",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in 2:2){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part==1]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part==1],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in n:n){
    plot(LC$Nsamp_in[LC$Fleet==i & LC$Sim==j & LC$Part==1]~LC$Yr[LC$Fleet==i & LC$Sim==j & LC$Part==1],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("topright",paste0(Fleets[i]," Discards"),bty="n")
}
dev.off()


#######################################
# OM Data Plot - Age Comps - Landings #  
#######################################

AC<-rbind(OM$replist1$age_comp_fit_table,OM$replist2$age_comp_fit_table,OM$replist3$age_comp_fit_table)
AC$Sim<-c(rep(1,dim(AC)[1]/n),rep(2,dim(AC)[1]/n),rep(3,dim(AC)[1]/n))

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_AgeComp_N.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(AC$Fleet)){
  for (j in 1:1){
    plot(AC$Nsamp_in[AC$Fleet==i & AC$Sim==j]~AC$Yr[AC$Fleet==i & AC$Sim==j],
         las=1,type="b",ylab="Age Sample Size",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in 2:2){
    plot(AC$Nsamp_in[AC$Fleet==i & AC$Sim==j]~AC$Yr[AC$Fleet==i & AC$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
  }
  for (j in n:n){
    plot(AC$Nsamp_in[AC$Fleet==i & AC$Sim==j]~AC$Yr[AC$Fleet==i & AC$Sim==j],
         type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
  }
  grid(col="lightgray")
  abline(v=TermYr,lty=3,col="red")
  legend("topright",Fleets[i],bty="n")
}
dev.off()



# ######################
# #  Mean Body Weight  #
# ######################
# 
# MBW<-rbind(OM$replist1$mnwgt,OM$replist2$mnwgt,OM$replist3$mnwgt)
# MBW$Sim<-c(rep(1,dim(MBW)[1]/n),rep(2,dim(MBW)[1]/n),rep(3,dim(MBW)[1]/n))
# 
# jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_MeanBodyWeight_Obs.jpeg"),res=300,height=2400,width=3000)
# par(mfrow=c(2,1),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
# for (i in unique(MBW$Fleet)){
#   for (j in 1:1){
#     plot(MBW$Obs[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          las=1,type="b",ylab="Mean Body Weight (kg)",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in 2:2){
#     plot(MBW$Obs[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in n:n){
#     plot(MBW$Obs[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
#   }
#   grid(col="lightgray")
#   abline(v=TermYr,lty=3,col="red")
#   legend("topright",Fleets[i],bty="n")
#   legend(2019,max(MBW$Obs[MBW$Fleet==i]*0.95),paste0("SE = ", round(mean(sample_struct$meanbodywt$Std_in[sample_struct$meanbodywt$FltSvy==i]),3)),bty="n")
# }
# dev.off()
# #NOTE: Why are the projection years set to -1?
# 
# jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/OM_MeanBodyWeight_Exp.jpeg"),res=300,height=2400,width=3000)
# par(mfrow=c(2,1),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
# for (i in unique(MBW$Fleet)){
#   for (j in 1:1){
#     plot(MBW$Exp[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          las=1,type="b",ylab="Mean Body Weight (kg)",xlab="Year",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in 2:2){
#     plot(MBW$Exp[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2); par(new=T)
#   }
#   for (j in n:n){
#     plot(MBW$Exp[MBW$Fleet==i & MBW$Sim==j]~MBW$Yr[MBW$Fleet==i & MBW$Sim==j],
#          type="b",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2)
#   }
#   grid(col="lightgray")
#   abline(v=TermYr,lty=3,col="red")
#   legend("topright",Fleets[i],bty="n")
#   legend(2019,max(MBW$Exp[MBW$Fleet==i]*0.95),paste0("SE = ", round(mean(sample_struct$meanbodywt$Std_in[sample_struct$meanbodywt$FltSvy==i]),3)),bty="n")
# }
# dev.off()
# #NOTE: Why are the projection years set to -1?




################
# OUTPUT EM DATA 

#library(r4ss)

n<-3 # number of simulations
# Combine directories for OM model runs
Direct<-c(paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_init"),
          paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_2020"),
          paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_2023"),
          paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_2026"),
          paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_2029"),
          paste0(getwd(),"/run_SSMSE-ex/results/base/",1,"/Starting_EM_EM_2032"))
for (i in 2:n) {
  Direct<-c(Direct,c(paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_init"),
                     paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_2020"),
                     paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_2023"),
                     paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_2026"),
                     paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_2029"),
                     paste0(getwd(),"/run_SSMSE-ex/results/base/",i,"/Starting_EM_EM_2032")))
}

#Process EM model runs
EM <- SSgetoutput(getcomp=FALSE,dirvec=Direct)
EM_summary <- SSsummarize(EM)
str(EM_summary)
# only includes indices

#Confirm Terminal years for each EM
EMdetails<-as.data.frame(cbind(c("Starting_EM_EM_init","Starting_EM_EM_2020",
                                 "Starting_EM_EM_2023","Starting_EM_EM_2026",
                                 "Starting_EM_EM_2029","Starting_EM_EM_2032"),
                               as.numeric(c(EM$replist1$endyr,EM$replist2$endyr,EM$replist3$endyr,
                                            EM$replist4$endyr,EM$replist5$endyr,EM$replist6$endyr))))
colnames(EMdetails)<-c("model_run","termyr")
EMdetails
Yrs<-as.numeric(EMdetails$termyr)
Yrs

########################
# EM Data Plot - Catch #  
########################

EM$replist1$catch$Sim<-1; EM$replist1$catch$run<-1
EM$replist2$catch$Sim<-1; EM$replist2$catch$run<-2
EM$replist3$catch$Sim<-1; EM$replist3$catch$run<-3
EM$replist4$catch$Sim<-1; EM$replist4$catch$run<-4
EM$replist5$catch$Sim<-1; EM$replist5$catch$run<-5
EM$replist6$catch$Sim<-1; EM$replist6$catch$run<-6

EM$replist7$catch$Sim<-2; EM$replist7$catch$run<-1
EM$replist8$catch$Sim<-2; EM$replist8$catch$run<-2
EM$replist9$catch$Sim<-2; EM$replist9$catch$run<-3
EM$replist10$catch$Sim<-2; EM$replist10$catch$run<-4
EM$replist11$catch$Sim<-2; EM$replist11$catch$run<-5
EM$replist12$catch$Sim<-2; EM$replist12$catch$run<-6

EM$replist13$catch$Sim<-3; EM$replist13$catch$run<-1
EM$replist14$catch$Sim<-3; EM$replist14$catch$run<-2
EM$replist15$catch$Sim<-3; EM$replist15$catch$run<-3
EM$replist16$catch$Sim<-3; EM$replist16$catch$run<-4
EM$replist17$catch$Sim<-3; EM$replist17$catch$run<-5
EM$replist18$catch$Sim<-3; EM$replist18$catch$run<-6

Catch<-rbind(EM$replist1$catch,EM$replist2$catch,EM$replist3$catch,EM$replist4$catch,
             EM$replist5$catch,EM$replist6$catch,EM$replist7$catch,EM$replist8$catch,
             EM$replist9$catch,EM$replist10$catch,EM$replist11$catch,EM$replist12$catch,
             EM$replist13$catch,EM$replist14$catch,EM$replist15$catch,EM$replist16$catch,
             EM$replist17$catch,EM$replist18$catch)

UnitsCom<-"Landings (Metric Tons)"
UnitsRec<-"Landings (1000s of Fish)"
TermYr<-dat$endyr # not OM end year (EM$replist1$endyr), but assessment terminal year 

#Color-blind friendly
library(RColorBrewer)
cols<-brewer.pal(length(Fleets), "Dark2")
nb.cols <- length(Fleets)
cols <- colorRampPalette(brewer.pal(length(Fleets), "Dark2"))(nb.cols)


jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/EM_Catch.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Catch$Fleet)[c(1:3)]){ #Commercial Fisheries only 
  for (j in 1:1){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           las=1,type="l",ylab=UnitsCom,xlab="Year",lty=3,pch=16,col="black",lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],axes=F,
           las=1,type="l",ylab=UnitsCom,xlab="Year",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    }
  }
  for (j in 2:2){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    }
  }
  for (j in n:n){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i])));par(new=T)
    }
  }
  grid(col="lightgray")
  abline(v=Yrs,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$catch$SE[sample_struct$catch$FltSvy==i]),2)),bty="n")
}

for (i in unique(Catch$Fleet)[4]){ #Recreational Fisheries only
  for (j in 1:1){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           las=1,type="l",ylab=UnitsRec,xlab="Year",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           las=1,type="l",ylab=UnitsRec,xlab="Year",pch=16,col=cols[i],lwd=2,axes=F,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    }
  }
  for (j in 2:2){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    }
  }
  for (j in n:n){
    for (k in 1:length(Yrs)){
      plot(Catch$Obs[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
      plot(Catch$Exp[Catch$Fleet==i & Catch$Sim==j & Catch$run==k]~Catch$Yr[Catch$Fleet==i & Catch$Sim==j & Catch$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Catch$Obs[Catch$Fleet==i],Catch$Exp[Catch$Fleet==i]))); par(new=T)
    }
  }
  grid(col="lightgray")
  abline(v=Yrs,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$catch$SE[sample_struct$catch$FltSvy==i]),2)),bty="n")
}
dev.off()

########################
# EM Data Plot - Index #  
########################

Index<-EM_summary$indices
#colnames(Index)[dim(Index)[2]]<-"Sim"
head(Index)
Index$Sim<-NA
Index$Sim[which(Index$name %in% c("replist1","replist2","replist3","replist4","replist5","replist6"))]<-1
Index$Sim[which(Index$name %in% c("replist7","replist8","replist9","replist10","replist11","replist12"))]<-2
Index$Sim[which(Index$name %in% c("replist13","replist14","replist15","replist16","replist17","replist18"))]<-3
Index$Sim
Index$run<-NA
Index$run[which(Index$name %in% c("replist1","replist7","replist13"))]<-1
Index$run[which(Index$name %in% c("replist2","replist8","replist14"))]<-2
Index$run[which(Index$name %in% c("replist3","replist9","replist15"))]<-3
Index$run[which(Index$name %in% c("replist4","replist10","replist16"))]<-4
Index$run[which(Index$name %in% c("replist5","replist11","replist17"))]<-5
Index$run[which(Index$name %in% c("replist6","replist12","replist18"))]<-6
Index$run

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/EM_Index.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(3,3),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Index$Fleet)){ #Indices Only
  for (j in 1:1){
    for (k in 1:length(Yrs)){
      plot(Index$Obs[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           las=1,type="l",ylab="Relative Abundance",xlab="Year",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
      plot(Index$Exp[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           las=1,type="l",ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    }
  }
  for (j in 2:2){
    for (k in 1:length(Yrs)){
      plot(Index$Obs[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
      plot(Index$Exp[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    }
  }
  for (j in n:n){
    for (k in 1:length(Yrs)){
      plot(Index$Obs[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
      plot(Index$Exp[Index$Fleet==i & Index$Sim==j & Index$run==k]~Index$Yr[Index$Fleet==i & Index$Sim==j & Index$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Index$Obs[Index$Fleet==i],Index$Exp[Index$Fleet==i])*1.1)); par(new=T)
    }
  }
  grid(col="lightgray")
  abline(v=Yrs,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$CPUE$SE[sample_struct$CPUE$FltSvy==i]),2)),bty="n")
}
dev.off()

###########################
# EM Data Plot - Discards #  
###########################

EM$replist1$discard$Sim<-1; EM$replist1$discard$run<-1
EM$replist2$discard$Sim<-1; EM$replist2$discard$run<-2
EM$replist3$discard$Sim<-1; EM$replist3$discard$run<-3
EM$replist4$discard$Sim<-1; EM$replist4$discard$run<-4
EM$replist5$discard$Sim<-1; EM$replist5$discard$run<-5
EM$replist6$discard$Sim<-1; EM$replist6$discard$run<-6

EM$replist7$discard$Sim<-2; EM$replist7$discard$run<-1
EM$replist8$discard$Sim<-2; EM$replist8$discard$run<-2
EM$replist9$discard$Sim<-2; EM$replist9$discard$run<-3
EM$replist10$discard$Sim<-2; EM$replist10$discard$run<-4
EM$replist11$discard$Sim<-2; EM$replist11$discard$run<-5
EM$replist12$discard$Sim<-2; EM$replist12$discard$run<-6

EM$replist13$discard$Sim<-3; EM$replist13$discard$run<-1
EM$replist14$discard$Sim<-3; EM$replist14$discard$run<-2
EM$replist15$discard$Sim<-3; EM$replist15$discard$run<-3
EM$replist16$discard$Sim<-3; EM$replist16$discard$run<-4
EM$replist17$discard$Sim<-3; EM$replist17$discard$run<-5
EM$replist18$discard$Sim<-3; EM$replist18$discard$run<-6

Disc<-rbind(EM$replist1$discard,EM$replist2$discard,EM$replist3$discard,EM$replist4$discard,
            EM$replist5$discard,EM$replist6$discard,EM$replist7$discard,EM$replist8$discard,
            EM$replist9$discard,EM$replist10$discard,EM$replist11$discard,EM$replist12$discard,
            EM$replist13$discard,EM$replist14$discard,EM$replist15$discard,EM$replist16$discard,
            EM$replist17$discard,EM$replist18$discard)

jpeg(paste0(getwd(),"/run_SSMSE-ex/results/plots/EM_Discards.jpeg"),res=300,height=2400,width=3000)
par(mfrow=c(2,2),mar=c(1,3.8,1,1),oma=c(1,1,0.1,0.1))
for (i in unique(Disc$Fleet)){ #Fisheries only 
  for (j in 1:1){
    for (k in 1:length(Yrs)){
      plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           las=1,type="l",ylab="Discards (1000s of Fish)",xlab="Year",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
      plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           las=1,type="l",ylab="",xlab="",pch=16,col=cols[i],lwd=2,axes=F,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    }
  }
  for (j in 2:2){
    for (k in 1:length(Yrs)){
      plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
      plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    }
  }
  for (j in n:n){
    for (k in 1:length(Yrs)){
      plot(Disc$Obs[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col="black",lty=3,lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
      plot(Disc$Exp[Disc$Fleet==i & Disc$Sim==j & Disc$run==k]~Disc$Yr[Disc$Fleet==i & Disc$Sim==j & Disc$run==k],
           type="l",axes=F,ylab="",xlab="",pch=16,col=cols[i],lwd=2,xlim=c((dat$styr-1),(TermYr+projyrs)),
           ylim=c(0,max(Disc$Obs[Disc$Fleet==i],Disc$Exp[Disc$Fleet==i]))); par(new=T)
    }
  }
  grid(col="lightgray")
  abline(v=Yrs,lty=3,col="red")
  legend("top",Fleets[i],bty="n")
  legend("topright",paste0("SE = ", round(mean(sample_struct$discard_data$SE[sample_struct$discard_data$FltSvy==i]),2)),bty="n")
}
dev.off()


#####################
# Performance metrics

#Not working - NA in SSB_unfished for OM in summary$scalar$SSB_Unfished
#Manually fill in NA for OM runs
subset(OM_summary$quants,OM_summary$quants$Label=="SSB_Virgin")
summary$scalar$SSB_Unfished[c(7,14,21)]
summary$scalar$SSB_Unfished[7]<-as.numeric(subset(OM_summary$quants,OM_summary$quants$Label=="SSB_Virgin")[1])
summary$scalar$SSB_Unfished[14]<-as.numeric(subset(OM_summary$quants,OM_summary$quants$Label=="SSB_Virgin")[2])  
summary$scalar$SSB_Unfished[21]<-as.numeric(subset(OM_summary$quants,OM_summary$quants$Label=="SSB_Virgin")[3])
summary$scalar$SSB_Unfished[c(7,14,21)]

##################################
# Example from SSMSE - average SSB

# The get_rel_SSB_avg calculates the relative SSB in each year for each
# iteration of the operating model, then takes the average over the years from
# min_yr, to max_year. It uses the summary object as input to do these
# calculations.

get_rel_SSB_avg <- function(summary, min_yr, max_yr) {
  # Get just the result for the OMs and not for the EMs.
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  # find the unfished biomass fr the OMs
  B_unfished <- summary$scalar %>%
    filter(model_run %in% OM_vals) %>%
    select(iteration, scenario,SSB_Unfished)
  #  find the spawning stock biomass for the years of interest
  SSB_yr <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>%
    select(iteration, scenario, year, SpawnBio)
  # Calculated the relative spawning stock biomass using B_unfished and SSB_yr
  # dataframes, then take an average over years.
  SSB_yr <- left_join(SSB_yr, B_unfished) %>%
    mutate(Rel_SSB = SpawnBio/SSB_Unfished) %>%
    group_by(iteration, scenario) %>%
    summarize(avg_SSB = mean(Rel_SSB), .groups = "keep") %>%
    ungroup()
  SSB_yr # return SSB averaged over yrs for each iteration and each scenario.
}
rel_SSB <- get_rel_SSB_avg(summary, min_yr = dat$endyr, max_yr = (dat$endyr+projyrs))
## Joining, by = c("iteration", "scenario")

# function to summarize data in plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
# Now, plot the average relative spawning stock biomass for projection years 1-15
ggplot(data = rel_SSB, aes(x = scenario, y = avg_SSB)) +
  geom_hline(yintercept = 0.4, color = "gray") +
  stat_summary(fun.data = data_summary,
               position = position_dodge(width = 0.9), color = "blue") +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(title = "Average relative SSB\n(years 1-15)",
       x = "Scenario", y = "SSB/SSB_unfished") +
  theme_classic()

#################################################################################
# Extract time series data (to get annual OFL estimates, not just forecasted OFL)

TSdat<-summary$ts
unique(TSdat$model_run)
TSdat$Merge<-paste0(TSdat$iteration,"_",TSdat$model_run)
TSdat$OFL<-apply(TSdat[,substr(names(TSdat),1,7)=='retainB'],1,sum)
TSdat$AVY<-NA #annual variability in yield
for (i in 2:length(TSdat$AVY)){
  TSdat$AVY[i]<-(TSdat$OFL[i-1]-(TSdat$OFL[i]))/(TSdat$OFL[i])^2
}
#First year not valid since no previous year for calculation
TSdat$AVY[TSdat$year==dat$styr]<-NA

############################
# Extract derived quantities

DQdat<-summary$dq
unique(DQdat$model_run) 
DQdat$Merge<-paste0(DQdat$iteration,"_",DQdat$model_run)
DQdat$avg.F<-NA
for (i in 3:length(DQdat$avg.F)){
  DQdat$avg.F[i]<-exp(mean(log(c(DQdat$Value.F[i],DQdat$Value.F[i-1],DQdat$Value.F[i-2]))))
}
#First two years not valid since no previous years for calculation
DQdat$avg.F[DQdat$year==dat$styr]<-NA
DQdat$avg.F[DQdat$year==(dat$styr+1)]<-NA


#########################################################
# Extract derived quantities only for assessment interval 

Yrs[2:length(Yrs)] #first year is the end year of actual model

Benchdat<-DQdat %>%
  filter(year %in% (Yrs[2:length(Yrs)]+3)) %>% #OFL years are 3 years after TermYr
  filter(Value.ForeCatchret != "NA") %>% #We only want years where catch was produced (i.e., projyrs)
  select(year,Value.F,avg.F,Value.SSB,Value.ForeCatchret,model_run,iteration)
colnames(Benchdat)<-c("CatchYear","F","geomF","SSB","OFL","model_run","iteration")
unique(Benchdat$model_run) 
unique(Benchdat$Year)
Benchdat$Merge<-paste0(Benchdat$iteration,"_",Benchdat$model_run)
Benchdat
#This may not be right since F & SSB values come from terminal year, not 3+ like the OFL does...

##########################
# Extract reference points

BenchRefdat<-summary$scalar %>%
  select(F_MSY,F_SPR,SSB_Unfished,SSB_MSY,SSB_SPR,Ret_Catch_MSY,model_run,iteration)
unique(BenchRefdat$model_run)
# No data provided for OM - do we need to rerun them at the end?
# BenchRefdat<-BenchRefdat %>%
#   filter(model_run!="Starting_EM_EM_init") %>%
#   filter(model_run!="Starting_OM_OM")
BenchRefdat$Merge<-paste0(BenchRefdat$iteration,"_",BenchRefdat$model_run)
BenchRefdat

################################################################
# Merge reference points with EM results for assessment interval

MergedatAll<-merge(Benchdat,BenchRefdat,by="Merge",all.x=T,all.y=T)
dim(MergedatAll)[1] #confirm 21 rows (7 x 3)
MergedatAll$model_run.x<-NULL; MergedatAll$iteration.x<-NULL;MergedatAll$Merge<-NULL
colnames(MergedatAll)[c((dim(MergedatAll)[2]-1):dim(MergedatAll)[2])]<-c("model_run","iteration")
MergedatAll
Mergedat<-MergedatAll %>%
  filter(model_run!="Starting_EM_EM_init") %>%
  filter(model_run!="Starting_OM_OM")
Mergedat

##########################################
# Calculate Probability of Not Overfishing

#Define overfishing criteria
Mergedat$F_FMSY<-Mergedat$F/Mergedat$F_MSY
Mergedat$NotOverFishing<-0
for (i in 1:length(Mergedat$F_FMSY)){
  if (Mergedat$F_FMSY[i]<1){
    Mergedat$NotOverFishing[i]<-1
  }
}
# Probability of not overfishing = 
# PNOF = (Sum of sims where F/FMSY < 1)/(Nsims*Nprojyrs) *100
PNOF<-100*sum(Mergedat$NotOverFishing)/dim(Mergedat)[1]
PNOF

######################################################
# Calculate Probability of Biomass being above 50%BMSY

#Define overfished criteria
Mergedat$B_BMSY<-Mergedat$SSB/Mergedat$SSB_MSY
Mergedat$NotOverfished<-0
for (i in 1:length(Mergedat$B_BMSY)){
  if (Mergedat$B_BMSY[i]>0.5){
    Mergedat$NotOverfished[i]<-1
  } 
}
#Should we redefine this to be 0.62 for Spanish Mackerel? (MSST = (1-M)*SSBMSY)
#MSST = (1-M)*SSBMSY; Cobia, King Mackerel, Spanish Mackerel
#MSST = 0.5*SSBMSY; Vermilion, Amberjack, Gag, Gray Snapper, Red Grouper, Red Snapper. Gray Triggerfish
#MSST = 0.75*SSBMSY; Scamp, Yellowedge

# Probability of biomass remaining above 50%BMSY = (Sum of sims where B/BMSY > 0.5)/(Nsims*Nprojyrs) *100
Babove50<-100*sum(Mergedat$NotOverfished/dim(Mergedat)[1])
Babove50

######################################################
# Calculate Probability of Biomass being below 20%BMSY

#Define stock collapse criteria
Mergedat$BBelow20<-0
for (i in 1:length(Mergedat$B_BMSY)){
  if (Mergedat$B_BMSY[i]<0.2){
    Mergedat$BBelow20[i]<-1
  } 
}

# Probability of reducing biomass below 20%BMSY = (Sum of sims where B/BMSY < 0.2)/(Nsims*Nprojyrs) *100
Bbelow20<-100*sum(Mergedat$BBelow20/dim(Mergedat)[1])
Bbelow20

######################################################
# Calculate Probability of Yield being above 50% MSY

#Define yield criteria
Mergedat$OFL_MSY<-Mergedat$OFL/Mergedat$Ret_Catch_MSY
Mergedat$Yield<-0
for (i in 1:length(Mergedat$OFL_MSY)){
  if (Mergedat$OFL_MSY[i]>0.5){
    Mergedat$Yield[i]<-1
  } 
}
Mergedat

# Yield = (Sum of sims achieving over 50% FMSY yield over entire period)
Yield50<-100*sum(Mergedat$Yield/dim(Mergedat)[1])
Yield50

#long-term over final 9 years of projections (get 3 runs)
Mergedat_longterm<-Mergedat %>% 
  filter(CatchYear>Yrs[4])
# Long-term Yield = (Sum of sims achieving over 50% FMSY yield over last 9 years)
LTYield50<-100*sum(Mergedat_longterm$Yield/dim(Mergedat_longterm)[1])
LTYield50

#short-term over first 7 years of projection (get 2 runs)
Mergedat_shortterm<-Mergedat %>% 
  filter(CatchYear<=Yrs[4])
STYield50<-100*sum(Mergedat_shortterm$Yield/dim(Mergedat_shortterm)[1])
STYield50

# Variability in yield = (Sum of sims achieving < 15 average annual variability in yield)
summary(TSdat$AVY)
TSdatfore<-TSdat %>% 
  filter(year %in% (Yrs[2:length(Yrs)]+3))  %>%
  filter(model_run!="Starting_EM_EM_init") %>%
  filter(model_run!="Starting_OM_OM")
TSdatfore$AAVY15<-0
for (i in 1:length(TSdatfore$AAVY15)){
  if (abs(TSdatfore$AAVY15[i])<0.15){
    TSdatfore$AAVY15[i]<-1
  } 
}
TSdatfore$AAVY20<-0
for (i in 1:length(TSdatfore$AAVY20)){
  if (abs(TSdatfore$AAVY20[i])<0.2){
    TSdatfore$AAVY20[i]<-1
  } 
}
dim(TSdatfore) #too many lines?
table(TSdatfore$model_run,TSdatfore$year)
TSdatfore<-TSdatfore %>%
  filter(model_run=="Starting_EM_EM_2024" & year==2027)

AAVY15<-100*sum(TSdatfore$AAVY15/dim(TSdatfore)[1])
AAVY15

AAVY20<-100*sum(TSdatfore$AAVY20/dim(TSdatfore)[1])
AAVY20

##################################################################
# Calculate Annual Ratios of F, SSB, and Catch to reference points

DQdat2<-merge(DQdat,BenchRefdat,by="Merge",all.x=T)
DQdat2$F_FMSY<-DQdat2$Value.F/DQdat2$F_MSY
DQdat2$SSB_SSBMSY<-DQdat2$Value.SSB/DQdat2$SSB_MSY
DQdat2$C_CMSY<-DQdat2$Value.ForeCatchret/DQdat2$Ret_Catch_MSY
head(DQdat2)

# Forecasted catch/MSY by year and model run
p_C_CMSY<-ggplot(data = subset(DQdat2, model_run.x %in% c(unique(DQdat2$model_run.x))), 
                 ggplot2::aes(x = year, y = C_CMSY)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration.x), color = model_run.x))+
  xlim(min(DQdat2$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="Forecasted Catch/MSY") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_ForeCatch_MSY.jpg"),width=6.5,height=4)

# F_FMSY by year and model run
p_F_FMSY<-ggplot(data = subset(DQdat2, model_run.x %in% c(unique(DQdat2$model_run.x))), 
                 ggplot2::aes(x = year, y = F_FMSY)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  geom_line(ggplot2::aes(linetype = as.character(iteration.x), color = model_run.x))+
  xlim(min(DQdat2$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="F/FMSY") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_F_FMSY.jpg"),width=6.5,height=4)

# F_FMSY by year and model run
p_SSB_SSBMSY<-ggplot(data = subset(DQdat2, model_run.x %in% c(unique(DQdat2$model_run.x))), 
                     ggplot2::aes(x = year, y = SSB_SSBMSY)) +
  geom_vline(xintercept = dat$endyr, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_hline(yintercept = 0.2, color = "red") +
  geom_line(ggplot2::aes(linetype = as.character(iteration.x), color = model_run.x))+
  xlim(min(DQdat2$year),(max(dat$endyr)+projyrs)) + 
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values = rep("solid", 50)) +
  guides(linetype = FALSE) +
  facet_wrap(. ~ scenario) +
  labs(x = "Year",y="SSB/SSBMSY") + 
  theme_classic()
ggsave(paste0(run_res_path,"/plots/_ts_SSB_SSBMSY.jpg"),width=6.5,height=4)



