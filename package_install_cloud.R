# Install and load packages

# Installing SSMSE and all dependencies
# note: @ allows you to choose the branch of SSMSE you want to download
remotes::install_github("nmfs-fish-tools/SSMSE@red_tide_em")

# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("runs_output")
dir.create(run_SSMSE_dir)

# Create a folder for mounting the bucket in the working directory.
bucket_dir <- file.path("bucket")
dir.create(bucket_dir)

# Base OM folder
# this is where you will upload the stock assessment you are working with
model_SSMSE_dir <- file.path("base_models")
dir.create(model_SSMSE_dir)

# If you have more than one OM/EM to upload, add the folders here.  