#!/bin/bash

# Define the list of scenarios
scen_list=(
  "no_rt_x_no_rt"
  "no_rt_x_flat_all_yrs"
  "no_rt_x_young_all_yrs"
  "no_rt_x_mid_all_yrs"
  "no_rt_x_old_all_yrs"
  "no_rt_x_flat_rt_17"
  "no_rt_x_young_rt_17"
  "no_rt_x_mid_rt_17"
  "no_rt_x_old_rt_17"
  "flat_x_no_rt"
  "young_x_no_rt"
  "mid_x_no_rt"
  "old_x_no_rt"
  "flat_x_flat_rt_2"
  "young_x_young_rt_2"
  "old_x_old_rt_2"
  "mid_x_mid_rt_2"
  "flat_x_young_rt_2"
  "flat_x_old_rt_2"
  "flat_x_mid_rt_2"
  "young_x_flat_rt_2"
  "young_x_old_rt_2"
  "young_x_mid_rt_2"
  "old_x_flat_rt_2"
  "old_x_young_rt_2"
  "old_x_mid_rt_2"
  "mid_x_flat_rt_2"
  "mid_x_young_rt_2"
  "mid_x_old_rt_2"
  "flat_x_flat_all_yrs"
  "young_x_young_all_yrs"
  "old_x_old_all_yrs"
  "mid_x_mid_all_yrs"
  "flat_x_young_all_yrs"
  "flat_x_old_all_yrs"
  "flat_x_mid_all_yrs"
  "young_x_flat_all_yrs"
  "young_x_old_all_yrs"
  "young_x_mid_all_yrs"
  "old_x_flat_all_yrs"
  "old_x_young_all_yrs"
  "old_x_mid_all_yrs"
  "mid_x_flat_all_yrs"
  "mid_x_young_all_yrs"
  "mid_x_old_all_yrs"
)

echo "Starting backfill processing for ${#scen_list[@]} scenarios..."

# Loop through each scenario in the list
for scen in "${scen_list[@]}"; do
  echo "------------------------------------------------------------"
  echo "Processing Scenario: $scen"
  echo "------------------------------------------------------------"
  
  # Loop through iterations 1 to 4 for each scenario
  for iter in {1..4}; do
    echo "Syncing iteration $iter..."
    
    gcloud storage rsync \
      "$HOME/Red_Tide_Methods/runs_output/results_red_tide_em/${scen}/${iter}/" \
      "gs://ecsai-red-tide-simulation-project/2026_06_24_red_tide_em/results_red_tide_em/${scen}/${iter}/" \
      --recursive
      
  done
done

echo "All listed scenarios and iterations checked and synced!"