# Selectivity Plots

library(tidyverse)
library("nmfspalette")
library(patchwork)
library(knitr)
library(kableExtra)
library(r4ss)
# OM locations

#name of the first OM
model_SSMSE_dir <- file.path("base_models")
default <- SS_output(file.path(model_SSMSE_dir, "default_sigmaR"))
mid <- SS_output(file.path(model_SSMSE_dir, "mid"))
flat <- SS_output(file.path(model_SSMSE_dir, "flat"))
old <- SS_output(file.path(model_SSMSE_dir, "old"))
young <- SS_output(file.path(model_SSMSE_dir, "young"))

default$SelAgeAdj

SSplotSelex(default)
SSplotSelex(mid, subplots = 2)
SSplotSelex(young, subplots = 2)
SSplotSelex(young, subplots = 12)
test <- SSplotSelex(mid)

library(tidyverse)

View(young$ageselex)
young$ageselex %>%
  filter(Fleet == 5, Factor == "Asel") %>%
  pivot_longer(
    cols = matches("^[0-9]+$"), # Selects columns that are purely numbers
    names_to = "age",
    values_to = "selectivity"
  ) %>%
  ggplot(aes(age, selectivity)) + geom_point(aes(color = Yr))

View(young$SelAgeAdj)
young$SelAgeAdj %>%
  filter(Fleet == 5) %>%
  pivot_longer(
    cols = starts_with("Par"),       # Selects Par1, Par2, etc.
    names_to = "age",                # New column name
    names_prefix = "Par",            # Removes "Par" from the names
    values_to = "selectivity",       # Where the cell values go
    names_transform = list(age = as.numeric) # Optional: converts "1", "2" to numbers
  ) %>%
  ggplot(aes(age, selectivity)) + geom_line(aes(color = Yr, group = Yr))
  
