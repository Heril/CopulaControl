list.of.packages <- c("copula", "tidyverse", "Hmisc", "foreach", "doMC", "doRNG")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org")