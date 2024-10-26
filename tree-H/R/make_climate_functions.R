##Making functions for climate data wrangling and climate normals

# make climate predictors meaningful by incorporating previous year ppt, tmin, tmax--------
# NOTE: data needs to be formatted appropriately, see datawrangling_climategrowth script to do this

climate_growthyear <- function(climate_data) {
  climate_data %>% 
    mutate(growthyear = if_else(month %in% c(9:12), year + 1, year))
}

#meaningful_climate_pred <- function(climate_data) {
#  climate_data %>% 
#    mutate(growthyear = if_else(month %in% c(start_dormant:12), year + 1, year))
#}

# View(new_climate)

# make climate normals function from prism data for each site

climate_normals <- function(climate_data, start, end) {
  normals <- climate_data %>% 
    filter(year >= start & year <= end) %>% 
    group_by(PLOT_CN, year) %>% 
    summarize(mean_ppt = mean(ppt), mean_tmin = mean(tmin), mean_tmax = mean(tmax))
  
  site_normals <- normals %>% group_by(PLOT_CN) %>% summarize(meantemp = mean(mean(mean_tmin + mean_tmax)/2), precip = mean(mean_ppt))
  
  return(site_normals)

}


# center and scale climate variables 
scaling_climate <- function(climate_data, center = TRUE, scale = TRUE, vars_to_scale = NULL) {
  
  # If vars_to_scale is NULL, select all numeric columns except "year" and "month"
  if (is.null(vars_to_scale)) {
    vars_to_scale <- names(climate_data)[sapply(climate_data, is.numeric)]
    vars_to_scale <- vars_to_scale[!vars_to_scale %in% c("year", "month", "growthyear", "PLOT_CN")]
  }
  
  # Apply scaling to the specified columns using scale(x, center, scale)
  climate_data[vars_to_scale] <- lapply(climate_data[vars_to_scale], function(x) {
    as.numeric(scale(x, center = center, scale = scale))
  })
  
  return(climate_data)
}
