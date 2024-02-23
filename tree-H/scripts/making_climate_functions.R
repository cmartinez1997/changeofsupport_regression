##Making functions for climate data wrangling and climate normals


# load packages -----------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------

climate_dat <- read_csv("tree-H/data/climate_data_all.csv")

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

new_climate <- climate_growthyear(climate_dat)
View(new_climate)

# make climate normals function from prism data for each site

climate_normals <- function(climate_data, start, end) {
  normals <- climate_data %>% 
    filter(year >= start & year <= end) %>% 
    group_by(PLT_CN, year) %>% 
    summarize(mean_ppt = mean(ppt), mean_tmin = mean(tmin), mean_tmax = mean(tmax))
  
  site_normals <- normals %>% group_by(PLT_CN) %>% summarize(meantemp = mean(mean(mean_tmin + mean_tmax)/2), precip = mean(mean_ppt))
  
  return(site_normals)

}

#climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)
#export climate normals as csv

ggplot(norms, aes(x = precip, y = meantemp)) +
  geom_point() +
  labs(x = "Mean Annual Precipitation", y = "Mean Annual Temperature") +
  ggtitle("Climate Normals: Mean Annual Temperature vs. Mean Annual Precipitation") + 
  theme_bw()

write_csv(norms, "tree-H/data/climate_norms.csv")
