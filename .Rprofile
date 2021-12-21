# Default options (used when running manually from RStudio)
options(ie.worker.local=TRUE)

# Overrides for running in the cloud. 
if(getOption("ie.deployment", "") != "") {
  options(ie.worker.local=FALSE)
  options(ie.digitalocean.access.token="${DIGITALOCEAN_ACCESS_TOKEN}")
}
#### -- Packrat Autoloader (version 0.5.0-21) -- ####
# source("packrat/init.R")
#### -- End Packrat Autoloader -- ####
