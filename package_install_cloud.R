# Install and load packages
# These should download packages to the home directory
install.packages("remotes")
install.packages("tidyverse")
install.packages("blastula")

# Installing SSMSE and all dependencies
# note: @ allows you to choose the branch of SSMSE you want to download
remotes::install_github("nmfs-fish-tools/SSMSE@red_tide_em")

# Install the older version of r4ss that is compatible with the current SSMSE
remotes::install_github("r4ss/r4ss@v1.49.3", force = TRUE)

# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("runs_output")
dir.create(run_SSMSE_dir)

# Base OM folder
# this is where you will upload the stock assessment you are working with
model_SSMSE_dir <- file.path("base_models")
dir.create(model_SSMSE_dir)

# If you have more than one OM/EM to upload, add the folders here.  