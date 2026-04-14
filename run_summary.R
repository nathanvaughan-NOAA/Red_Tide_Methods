

PASSWORD <- 
email_address <- 

results_name <- "selectivity_100_redo"

run_SSMSE_dir <- "bucket/"
run_res_path <- paste0("bucket/", results_name)

start_time <- Sys.time()

# make a summary with all the outputs in the same folder
summary <- SSMSE::SSMSE_summary_all(run_res_path, run_parallel = TRUE)
saveRDS(summary, file = file.path(run_SSMSE_dir, paste0("results_summary_", results_name, ".rda")))

# end timer
end_time <- Sys.time()
time_dif <- end_time - start_time

# send email to indicate the run is done
library(blastula)

# Create the email
email <- compose_email(
  body = md(glue::glue("Your R job is **complete!** It took {time_dif} to run."))
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