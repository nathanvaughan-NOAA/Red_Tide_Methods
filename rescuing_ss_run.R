#Let's see if I can fix this SS run.  

# Install and load packages
# Might need to uncomment next line to install packages
# options(download.file.method = "libcurl")
# remotes::install_github("r4ss/r4ss") need to edit for previous version
# remotes::install_github("ss3sim/ss3sim")
# remotes::install_github("nmfs-fish-tools/SSMSE")
devtools::load_all("C://Users/apn26/Documents/SSMSE/")  # needs to be the cloned SSMSE repo

library(tidyverse)
packageVersion("r4ss")
packageVersion("ss3sim")
packageVersion("SSMSE")

OM_no_red_tide_path <- file.path("OM_no_red_tide_OM")
get_ss3_exe(dir = OM_no_red_tide_path, version = "v3.30.18")
run(dir = OM_no_red_tide_path, skipfinished = FALSE, show_in_console = TRUE)

#I copied and pasted the missing F_rates into the .par file and it still failed, but it ended the parameters section.  
#changing the Initial parameter values to 0 from the ctl file instead of the par file seems to have worked

test <- SS_output(OM_no_red_tide_path, covar = FALSE)
