#Testing moving files

cloud_bucket <- file.path("/cloud bucket")
new_EM_out_dir <- "/results_rt_2/no_rt/4/default_sigmaR_EM_2047"

# create the scenario folder in bucket if needed
scenario_folder <- file.path(cloud_bucket, basename(dirname(dirname(new_EM_out_dir))))
if (!file.exists(scenario_folder)) { dir.create(scenario_folder) }

# create the iteration folder in bucket if needed
iteration_folder <- file.path(scenario_folder, basename(dirname(new_EM_out_dir)))
if (!file.exists(iteration_folder)) { dir.create(iteration_folder) }

# --- Perform the Move Operation ---

# list all of the folders in the original iteration folder
old_dirs <- list.dirs(path = dirname(new_EM_out_dir),
                      full.names = TRUE,
                      recursive = FALSE)

# for each folder in that list of folders we want to copy that entire folder to the new directory
for (dir in old_dirs) {
  
  # create the new om/em in the cloud
  target_dir <- file.path(iteration_folder, basename(dir))
  dir.create(target_dir)
  
  # move all of the files into the new om/em folder
  
  old_files <- list.files(path = dir,
                          full.names = TRUE,
                          recursive = FALSE)

  for (file in old_files) {
    system(paste("mv", shQuote(file), shQuote(file.path(target_dir, basename(file)))))
    if (move_successful) {
      cat("Success! The folder '", dir, "' and all its contents were moved.\n", sep="")
      cat(paste("New location:", file.path(iteration_folder, basename(dir)), "\n"))
    } else {
      cat("Move failed. This might be due to permissions or an open file lock.\n")
    }
  }
}

# file.rename() moves the 'source_folder' and ALL its contents to the 'final_destination'
move_successful <- file.rename(
  from = dirname(new_EM_out_dir), 
  to = iteration_folder
)


if (move_successful) {
  cat("Success! The folder '", folder_name_to_move, "' and all its contents were moved.\n", sep="")
  cat(paste("New location:", final_destination, "\n"))
} else {
  cat("Move failed. This might be due to permissions or an open file lock.\n")
}
  





if (!is.null(cloud_bucket)) {  #cloud_bucket should be the directory of a google bucket
  
  # Set the path to your iteration folder
  iteration_folder <- dirname(new_EM_out_dir)
  
  # List all directories in the iteration folder
  old_dirs <- list.dirs(path = iteration_folder,
                        full.names = TRUE,
                        recursive = FALSE)
  
  cat("Starting file transfer from local directories to mounted bucket:", cloud_bucket, "\n")
  
  # Move all files in the old directories to the mounted bucket directory
  for (dir in old_dirs) {
    # List all files in the directory (non-recursive)
    files_to_move <- list.files(path = dir, full.names = TRUE)
    
    if (length(files_to_move) > 0) {
      cat("Processing directory:", dir, "\n")
      
      # 1. Construct the NEW destination paths (in the mounted bucket)
      # We combine the bucket path with the base file name.
      file_names <- basename(files_to_move)
      new_paths_in_bucket <- file.path(cloud_bucket, file_names)
      
      # 2. Use file.rename() to move the files from the local disk to the mounted bucket
      move_results <- file.rename(files_to_move, new_paths_in_bucket)
      
      # Check and report results
      if (all(move_results)) {
        cat("Successfully moved", length(files_to_move), "files.\n")
      } else {
        cat("Failed to move some files. Check file permissions.\n")
      }
    }
    # Optionally, remove the now-empty local directory after all files are moved
    # This will only work if the directory is empty.
    unlink(dir, recursive = FALSE)
  }
  cat("\n file movement complete.\n")
} else {
  cat("Skipping file transfer because cloud_bucket is NULL.\n")
}
