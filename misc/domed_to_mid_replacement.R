# 1. Set the path to your main directory
parent_dir <- "runs_output/results_selectivity/"

# 2. List everything (files and folders) that contains "domed"
# full.names = TRUE is crucial so R knows exactly where the files live
old_paths <- list.files(path = parent_dir, 
                        pattern = "domed", 
                        include.dirs = TRUE)

# 3. Create the new names by replacing "domed" with "mid"
new_paths <- gsub(pattern = "domed", replacement = "mid", x = old_paths)

# Run this to see exactly what will happen before you do it
data.frame(Old = paste0(parent_dir, old_paths), New = paste0(parent_dir, new_paths))

# 4. Execute the rename
# This returns a logical vector (TRUE/FALSE) for each file processed
results <- file.rename(from = paste0(parent_dir, old_paths), to = paste0(parent_dir, new_paths))

# Check if it worked
table(results)

# 2. List everything (files and folders) that contains "domed"
# full.names = TRUE is crucial so R knows exactly where the files live
old_paths <- list.files(path = parent_dir, 
                        pattern = "domed", 
                        recursive = TRUE,
                        include.dirs = TRUE)

# 3. Create the new names by replacing "domed" with "mid"
new_paths <- gsub(pattern = "domed", replacement = "mid", x = old_paths)

# Run this to see exactly what will happen before you do it
data.frame(Old = paste0(parent_dir, old_paths), New = paste0(parent_dir, new_paths))

# 4. Execute the rename
# This returns a logical vector (TRUE/FALSE) for each file processed
results <- file.rename(from = paste0(parent_dir, old_paths), to = paste0(parent_dir, new_paths))

# Check if it worked
table(results)

results_name <- "selectivity"
run_SSMSE_dir <- file.path("./runs_output")
run_res_path <- file.path(run_SSMSE_dir, paste0("results_", results_name))
summary <- SSMSE::SSMSE_summary_all(run_res_path)
saveRDS(summary, file = file.path(run_SSMSE_dir, paste0("results_summary_", results_name, ".rda")))

