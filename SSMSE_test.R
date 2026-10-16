scenarios_to_test_full = c("no_rt_x_no_rt", "flat_x_flat_rt_2", "flat_x_flat_all_yrs")
ssmse_inputs = all_scenarios

library(future)
library(future.apply)
library(SSMSE)
library(tidyverse)

SSMSE_test() <- function(ssmse_inputs = all_scenarios, scenarios_to_test_full = c("no_rt_x_no_rt", "flat_x_flat_rt_2", "flat_x_flat_all_yrs")) {
  
  # list all of the scenarios available
  scen_list_str <- ssmse_inputs %>%
    map_chr(\(x) x$scen_name_vec) %>%
    str_flatten(collapse = ", ", last = ", and ")
  
  # pull just the scenarios that will be fully tested
  test_cases <- keep(ssmse_inputs, ~ .x$scen_name_vec %in% scenarios_to_test_full)
  
  # reduce the # of iterations to 2
  # change all scenarios to only go for 1 core
  test_cases <- lapply(test_cases, function(x) {
    x[["iter_vec"]] <- 2
    x[["n_cores"]] <- 2
    return(x)
  })
  # start timer
  start_time <- Sys.time()
  
  # run just these scenarios all the way through in parallel
  plan(multisession, workers = length(scenarios_to_test_full))

  # execute scenarios in parallel
  results <- future_lapply(scenarios_to_test_full, function(scen) {
    
    one_scen <- keep(test_cases, ~ .x$scen_name_vec %in% scen)
    one_scen <- one_scen[[1]]
    exec(SSMSE::run_SSMSE, !!!one_scen)
    
  })  # Ensures reproducible random seeds across scenarios
  
  # return to sequential processing after test completes
  plan(sequential)
  
  # end timer
  end_time <- Sys.time()
  time_dif <- end_time - start_time
  
  # summarize
  results_name <- "supplemental"
  run_SSMSE_dir <- file.path("./runs_output")
  run_res_path <- file.path(run_SSMSE_dir, paste0("results_", results_name))
  summary <- SSMSE::SSMSE_summary_all(file.path(run_SSMSE_dir, paste0("results_", results_name)))
  
  # plot
  summary$scalar %>% 
    mutate(model_run_year = str_extract(model_run, "\\d+")) %>% #extract year from model_run
    ggplot(aes(model_run_year, max_grad))+
    geom_point() + 
    facet_wrap(~scenario) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  
  # save plot to plot folder
  
  # delete runs from the output folder
  
  # change all scenarios to only go to 6 years
  # change all scenarios to only go for 1 iteration
  test_cases <- lapply(ssmse_inputs, function(x) {
    x[["iter_vec"]] <- 2
    x[["iter_vec"]] <- 2
    return(x)
  })
  
  # run everything exactly as planned
  
  # summarize
  
  # plot
  
  # save plot to plot folder
  
}