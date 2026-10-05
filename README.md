# ECSAI FY25/26 Red Tide Project

This repository was made to store the scripts used for testing the implementation of red tide mortality in the Red Grouper stock assessment using SSMSE and Google Cloud Workstations.  Below is a walkthrough of the methods and a breakdown of the deliverables of this project.  

## Data

The base model was SEDAR 66 Gulf Red Grouper (SEDAR 2020).  The sigmaR was fixed after testing to reduce modeling error (Previous Runs/2025_07_03).  There were 3 additional OM's where the base model was re-run with a red tide bycatch fleet that had selectivity curves targeting young, middle, or old fish (Figure 1).  Each OM had an adjusted version with different forecast settings in the EM to account for red tide mortality in the forecasted years (e.g. mid_adj).  

*Insert note about why the data is not stored here, and where it is stored*  The data should be unzipped and stored in the base_model folder created by the install_packages.R script.  

## Methods

This workflow was run in the Google Cloud Workstations, so some of the methods are specific to Google Cloud Workstations, but this can be adapted to run locally.  

### Set-up Environment 

#### 1. Log into GitHub and clone repository

Script: github_setup.R

Change the username, email, and Personal Access Token (PAT) entries in the script and run the code to log into GitHub and keep it persistent whenever you open or close the workstation.  

*WARNING*: Do not save and push your PAT to this repositiory.  It needs to be confidential.  If you do not like filling out the entries everytime, you can save a copy of this script locally with your confidential infromation, but keep it secure.  

#### 2. Install packages and create folders

Script: package_install_cloud.R

Run this script to install the nessecary packages and create the empty base_model, runs_output, and bucket folders.  base_models will be used to store all of the input data.  runs_output will store the output of the SSMSE model.  bucket is used if a bucket is mounted to the workstation.  

Since this workflow only requires SSMSE, a specific version of r4ss, and some tidyverse packages, I decided against using renv.  

*Note:* The current version does not include the r4ss install because it is already installed on the large workstations.  Should I add that back in as commented code?  

#### 3. Import base model 

No script.  

Manually upload the base stock assessment in the base_model folder.  I currently keep a .zip folder with all of the base models, including the "_adj" models that I upload everytime using RStudios "upload" button under the files tab.  

#### Optional: Create adjusted copies of models

Scripts: misc/create_models_from_default.R and misc/adjust_base_models_bycatch_fleet.R


If you only have the original SEDAR 61 model, you can use .R to generate the 3 models with different selectivity curves.  

If you are running scenarios that require the "_adj" base models they can be generated with .R if the varying selectivity base models exist.  

#### 4. Connect to bucket

Script: mount_bucket.sh

Run "bash mount_bucket.sh" in the terminal to link the bucket to the bucket folder and authenticate google cloud SDK for gcloud functions and file transfers.  

### Step 2: Run SSMSE Scenarios

Script: SSMSE_red_grouper_starter.R

This is the full starter file that walks through all 45 scenarios designed for this project.  

### Step 3: Review Outputs

Scripts: red_tide_2_years_results_review.qmd, misc/check_random_year_red_tide_events.R  

These are the 2 most used scripts for checking trends before moving on to official plots.  

### Step 4: Official Outputs

plots/twg_revision_tables_plots_no_rt.R

This script generates the final plots used in the working papers and publications.  

## Deliverables



Previous runs houses all of the html files of previous runs we chose to highlight throughout the process. For example, the 2025_07_03 runs titled SigmaR, r0_SigmaR, and 2025_07_07_r0 were tests with either SigmaR, R0, or both fixed.

## Disclaimer:

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
