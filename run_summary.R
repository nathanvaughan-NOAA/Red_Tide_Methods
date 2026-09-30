library(tidyverse)
library(SSMSE)

# Check versions
packageVersion("r4ss")
packageVersion("ss3sim")
packageVersion("SSMSE")

# Create a folder for the output in the working directory.
results_name <- "supplemental_redo"
# Runs the summary file after all of scenarios finish.  
bucket_path <- paste0("gs://ecsai-red-tide-simulation-project/2026_09_23_supplemental_redo/results_", results_name)

# Path to your parent local folder and your destination GCS bucket
run_SSMSE_dir <- file.path("./runs_output")
run_res_path <- file.path(run_SSMSE_dir, paste0("results_", results_name))

# Construct command string
rsync_cmd <- paste(
  "gcloud storage rsync",
  shQuote(run_res_path),
  shQuote(bucket_path),
  "-r"
)

# Run the system command
status <- system(rsync_cmd)

if (status == 0) {
  message("Successfully synced missing/remaining files!")
} else {
  warning("rsync encountered an error during sync.")
}

results_name <- "supplemental_redo"

run_SSMSE_dir <- "bucket/"
run_res_path <- paste0("bucket/results_", results_name)

start_time <- Sys.time()

# make a summary with all the outputs in the same folder
summary <- SSMSE::SSMSE_summary_all(run_res_path)
saveRDS(summary, file = file.path(run_SSMSE_dir, paste0("results_summary_", results_name, ".rda")))

# end timer
end_time <- Sys.time()
time_dif <- end_time - start_time

saveRDS(time_dif, file = "timer_save_summary.rda")

##### END PROCESS #####

# create a unique tag name using the current timestamp
tag_name <- paste0("alert-", time_dif)

# create and push the tag locally pointing to your current commit
system(paste("git tag", tag_name))
system(paste("git push origin", tag_name)) # this will send email

# remove the tag so it doesn't clutter the repo
system(paste("git tag -d", tag_name))