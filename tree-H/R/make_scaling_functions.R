##Making functions for global and local scaling of climate variables


# load packages -----------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------

climate_dat <- read_csv("tree-H/data/all_clim_norms.csv")


# global scale function ---------------------------------------------------

global_scale <- function(data, climate_vars) {
  to_scale <- data[climate_vars] 
  scaled_data <- as.data.frame(scale(to_scale))
  scaled_data <- cbind(data[,setdiff(names(data), climate_vars)], scaled_data)
  return(scaled_data)
}

global_scale_clim <- global_scale(climate_dat, c("tmin", "tmax", "ppt", "meantemp", "precip"))

# local scale function ----------------------------------------------------
# this globally scales the normals, but locally scales the time-varying climate variables
local_scale <- function(data, clim_norms, clim_locals) {
  data[, clim_norms] <- scale(data[, clim_norms]) 
  data <- data %>% 
    group_by(PLT_CN) %>% 
    mutate_at(vars(all_of(clim_locals)), scale) %>% 
    ungroup()
  return(data)
}

local_scale_clim <- local_scale(climate_dat, clim_norms = c("meantemp", "precip"), clim_locals = c("tmin", "tmax", "ppt"))


