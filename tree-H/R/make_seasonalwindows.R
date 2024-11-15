making_climate_windows <- function(data, variable, start_month, end_month, year_shift) {
  # arguments:
  # data: The data frame containing climate data
  # variable: The name of the climate variable to aggregate (e.g., "ppt" for precipitation)
  # start_month: The start month of the  window (e.g., 6 for June)
  # end_month: The end month of the  window (e.g., 7 for July)
  # year_shift: -1 for previous year, 0 for current year
  
  # create column name based on year_shift and months
  prefix <- ifelse(year_shift == -1, "p_", "c_") # "p_" for previous year, "c_" for current year
  month_range <- paste0(start_month, end_month)  # concatenate months for naming
  col_name <- paste0(prefix, month_range, "_", variable) # Full dynamic column name
  
  data <- data %>%
    mutate(agg_year = if (year_shift == -1) year + 1 else year)
  
  # Filter for the specified months
  filtered_data <- data %>%
    filter(month >= start_month & month <= end_month)
  
  # Perform either summing or averaging based on the variable
  if (variable %in% c("tmax", "tmin")) {
    # Average for temperature variables
    aggregated_data <- filtered_data %>%
      group_by(PLOT_CN, agg_year) %>%
      filter(n_distinct(month) == (end_month - start_month + 1)) %>% # Ensure all specified months are present
      summarise(!!col_name := mean(!!sym(variable), na.rm = TRUE), .groups = "drop") %>%
      rename(year = agg_year)
  } else {
    # Sum for precipitation
    aggregated_data <- filtered_data %>%
      group_by(PLOT_CN, agg_year) %>%
      filter(n_distinct(month) == (end_month - start_month + 1)) %>% # Ensure all specified months are present
      summarise(!!col_name := sum(!!sym(variable), na.rm = TRUE), .groups = "drop") %>%
      rename(year = agg_year)
  }
  
  return(aggregated_data)
  
}


# making_climate_windows <- function(data, variable, start_month, end_month, year_shift) {
#   # Arguments:
#   # data: The data frame containing climate data
#   # variable: The name of the climate variable to aggregate (e.g., "ppt" for precipitation or "tmax" for temperature)
#   # start_month: The start month of the window (e.g., 6 for June)
#   # end_month: The end month of the window (e.g., 8 for August)
#   # year_shift: -1 for previous year, 0 for current year
#   
#   # Create column name based on year_shift and months
#   prefix <- ifelse(year_shift == -1, "p_", "c_") # "p_" for previous year, "c_" for current year
#   month_range <- paste0(start_month, end_month)  # concatenate months for naming
#   col_name <- paste0(prefix, month_range, "_", variable) # Full dynamic column name
#   
#   # Adjust year for the specified year_shift
#   data <- data %>%
#     mutate(agg_year = ifelse(year_shift == -1, year + 1, year))
#   
#   # Step 1: Filter for specified months and proceed with aggregation
#   aggregated_data <- data %>%
#     filter(month >= start_month & month <= end_month) %>%   # Filter only for specified months
#     group_by(PLOT_CN, agg_year) %>%                         # Group by plot and adjusted year
#     filter(n_distinct(month) == (end_month - start_month + 1)) %>% # Ensure all specified months are present
#     summarise(
#       !!col_name := if(variable %in% c("tmax", "tmin")) {
#         mean(!!sym(variable), na.rm = TRUE)                  # Average for temperature variables
#       } else {
#         sum(!!sym(variable), na.rm = TRUE)                   # Sum for precipitation
#       },
#       .groups = "drop"
#     ) %>%
#     rename(year = agg_year)  # Rename for merging into the target year
#   
#   return(aggregated_data)
# }

library(dplyr)

make_climwin <- function(data, variable, reference_month, months_span) {
  # Arguments:
  # data: The data frame containing climate data
  # variable: The name of the climate variable to aggregate (e.g., "ppt" for precipitation)
  # reference_month: The month (1-12) to use as the end of the aggregation period
  # months_span: The length of the aggregation window in months
  
  # Ensure data is sorted by year and month
  data <- data %>%
    arrange(PLOT_CN, year, month)
  
  # Initialize a list to store results
  results <- list()
  
  unique_plots <- unique(data$PLOT_CN)
  for (plot in unique_plots) {
    plot_data <- data %>% filter(PLOT_CN == plot)
    unique_years <- unique(plot_data$year)
    
    for (current_year in unique_years) {
      # Calculate the start month and year for the previous months_span window
      start_month <- (reference_month - months_span + 1 + 12) %% 12
      start_month <- ifelse(start_month == 0, 12, start_month)  # Adjust for month "0" to become "12"
      
      # Adjust the start year if the window spans into the previous year
      start_year <- ifelse(reference_month - months_span + 1 <= 0, current_year - 1, current_year)
      
      # Filter data to include only rows within the previous months_span window up to the reference month
      window_data <- plot_data %>%
        filter(
          (year > start_year | (year == start_year & month >= start_month)) &
            (year < current_year | (year == current_year & month <= reference_month))
        )
      
      # Check if we have the full months_span of data
      if (nrow(window_data) == months_span) {
        # Sum the variable for this window and store it with the current year and plot
        aggregate_sum <- sum(window_data[[variable]], na.rm = TRUE)
        results[[length(results) + 1]] <- data.frame(
          PLOT_CN = plot,
          year = current_year,
          aggregate_sum = aggregate_sum
        )
      }
    }
  }
  
  # Combine all results into a single data frame
  final_results <- bind_rows(results)
  
  return(final_results)
}

  
 
