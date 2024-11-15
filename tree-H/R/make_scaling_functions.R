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
    group_by(PLOT_CN) %>%
    mutate_at(vars(all_of(clim_locals)), scale) %>%
    ungroup()
  data <- as.data.frame(data)
  return(data)
}


# scaled_global <- global_scale(test_data,
#                                  climate_vars = c("temp", "precip"))
# 
# # Local scaling test
# scaled_local <- local_scale(data =test_data, clim_norms = "MAT", clim_locals = c("temp", "precip"))



# ##Testing 
# plot_1_data <- test_data[test_data$PLT_CN == 1, ]
# 
# # Calculate mean and standard deviation for the 'temp' column
# temp_mean <- mean(plot_1_data$temp)
# temp_sd <- sd(plot_1_data$temp)
# 
# # Apply scaling to the 'temp' column
# plot_1_data$temp_scaled <- (plot_1_data$temp - temp_mean) / temp_sd
# 
# # View the results
# print(plot_1_data[, c("year", "temp", "temp_scaled")])
# 
# mat_mean <- mean(test_data$MAT)
# mat_sd <- sd(test_data$MAT)
# 
# # Apply scaling to the 'MAT' column
# test_data$MAT_scaled <- (test_data$MAT - mat_mean) / mat_sd
# 
# # View the results
# print(test_data[, c("PLT_CN", "year", "MAT", "MAT_scaled")])


##YAY this works!!!!


# scaling_climate <- function(data, 
#                             scale_type = c("global", "local"),
#                             global_vars = NULL,
#                             local_group_var = NULL,
#                             local_vars = NULL) {
#   scale_type <- match.arg(scale_type)
#   
#   if (scale_type == "global") {
#     if (is.null(global_vars)) {
#       stop("Please specify the variables to globally scale.")
#     }
#     return(global_scale(data, global_vars))
#   }
#   
#   if (scale_type == "local") {
#     if (is.null(local_group_var) || is.null(local_vars)) {
#       stop("Please specify the grouping variable and variables to locally scale.")
#     }
#     return(local_scale(data, local_group_var, local_vars))
#   }
# }
# 
# 

# 
# # Variables to globally scale
# global_vars <- c("temp", "precip")
# 
# # Variables to locally scale
# local_vars <- c("temp", "precip")
# 
# # Global scaling test
# scaled_global <- scaling_climate(test_data, 
#                                  scale_type = "global", 
#                                  global_vars = global_vars)
# 
# # Local scaling test
# scaled_local <- scaling_climate(test_data, 
#                                 scale_type = "local", 
#                                 local_group_var = "PLOT_CN", 
#                                 local_vars = local_vars)
# 
# # View results
# head(scaled_global)
# head(scaled_local)
