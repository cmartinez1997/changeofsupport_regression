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
  
  # Step 1: Filter for specified months and proceed with aggregation
  aggregated_data <- data %>%
    filter(month >= start_month & month <= end_month) %>%   # Filter only for specified months
    group_by(PLOT_CN, agg_year) %>%                         # Group by plot and adjusted year
    filter(n_distinct(month) == (end_month - start_month + 1)) %>% # Ensure all specified months are present
    summarise(!!col_name := sum(!!sym(variable), na.rm = TRUE), .groups = "drop") %>% # Sum the specified variable without rounding
    rename(year = agg_year)                                      # Rename for merging into the target year
  
  
}





