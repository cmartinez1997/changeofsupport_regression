##Making functions for global and local scaling of climate variables

# global scale function ---------------------------------------------------

global_scale <- function(data, climate_vars) {
  to_scale <- data[climate_vars] 
  scaled_data <- as.data.frame(scale(to_scale))
  scaled_data <- cbind(data[,setdiff(names(data), climate_vars)], scaled_data)
  return(scaled_data)
}

# local scale function ----------------------------------------------------
# this globally scales the normals, but locally scales the time-varying climate variables

local_scale <- function(data, clim_norms, clim_locals) {
  data[, clim_norms] <- scale(data[, clim_norms]) 
  data <- data %>% 
    group_by(PLT_CN) %>% 
    mutate_at(vars(all_of(clim_locals)), scale) %>% 
    ungroup()
  data <- as.data.frame(data)
  return(data)
}

