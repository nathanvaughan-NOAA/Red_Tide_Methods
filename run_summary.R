# Runs the summary file after all of scenarios finish.  

results_name <- "supplemental"

run_SSMSE_dir <- "bucket/"
run_res_path <- paste0("bucket/", results_name)

start_time <- Sys.time()

# make a summary with all the outputs in the same folder
summary <- SSMSE::SSMSE_summary_all(run_res_path, run_parallel = TRUE)
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