# --- CONFIGURATION ---
source_bucket <- "gs://bucket_name/folder_name"
dest_bucket   <- "gs://bucket_name/folder_name"

# 1. Get the list of the 32 top-level folders
# We use 'system' to get the folder names directly from GCS
folders <- system(paste("gcloud storage ls", source_bucket), intern = TRUE)

# 2. Loop through each folder and move it
for (i in seq_along(folders)) {
  current_folder <- folders[i]
  
  # Extract just the folder name for printing
  folder_name <- basename(current_folder)
  message(paste0("[", i, "/", length(folders), "] Starting move for: ", folder_name))
  
  # Execute the move using multithreading (-m) for the subfolders
  # We use 'cp' with '-n' (no-clobber) to be safe
  cmd <- paste("gcloud storage cp --recursive -n", current_folder, dest_bucket)
  exit_code <- system(cmd)
  
  if (exit_code == 0) {
    message(paste("--- Success:", folder_name))
  } else {
    warning(paste("--- Failed or interrupted at:", folder_name))
  }
}

message("ALL BATCHES COMPLETE.")

#Google App Password for emailing
PASSWORD <- ""
email_address <- ""


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
