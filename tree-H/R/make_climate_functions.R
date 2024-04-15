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
    group_by(PLT_CN, year) %>% 
    summarize(mean_ppt = mean(ppt), mean_tmin = mean(tmin), mean_tmax = mean(tmax))
  
  site_normals <- normals %>% group_by(PLT_CN) %>% summarize(meantemp = mean(mean(mean_tmin + mean_tmax)/2), precip = mean(mean_ppt))
  
  return(site_normals)

}


