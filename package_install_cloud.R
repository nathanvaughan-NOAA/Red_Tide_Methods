# Install and load packages
# If you are in the RStudio IDE run this code to make packages install faster:  
repo_line <- 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))'
writeLines(repo_line, "~/.Rprofile")

# Restart R session to lock in Rprofile changes
# Session -> Restart R or Ctrl+Shift+F10

# Installing SSMSE and all dependencies
# note: @ allows you to choose the branch of SSMSE you want to download
remotes::install_github("nmfs-fish-tools/SSMSE@SEFSC-dev")

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