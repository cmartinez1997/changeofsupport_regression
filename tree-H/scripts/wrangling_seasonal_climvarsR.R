##Data wnralginf for seasonal climate vars


# load packages -----------------------------------------------------------
library(here)
library(Matrix)
library(mgcv)
library(gratia)
library(tidyr)
library(climwin)
library(lme4)
library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(glmnet)

######################### Wrangling Seasonal from Monthly Climate Data ##########################################

# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 
source(here::here("tree-H", "R", "make_annualizeDBH.R"))
source(here::here("tree-H", "R", "make_seasonalwindows.R"))

# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_all_climate_growth_rw.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size_all.csv"))
#if you want climate scaled, then load "wbp_all_climate_data_scaled_all.csv"
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "wbp_all_climate_data_all.csv"))# Check the data sources for overlap and lack of overlap

# Check how the dataframes overlap 
missing_overlap <- check_overlap(dat, dat_climate, dat_bc)

# Drop the missing data # do this for backcaluation
message("About ", round(mean((missing_overlap$tree_CN_missing) | (missing_overlap$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
message("About ", round(mean((missing_overlap$climate_CN_missing) | (missing_overlap$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")
message("About ", round(mean((missing_overlap$bc_tree_CN_missing) | (missing_overlap$bc_year_missing)) * 100, digits = 0), "% of backcalculated data will be dropped")

dat         <- dat[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
dat_climate <- dat_climate[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]
dat_climate <- dat_climate %>% 
  dplyr::select(-tmin)
dat_bc      <- dat_bc[!missing_overlap$bc_tree_CN_missing & !missing_overlap$bc_year_missing, ]


dat_climate$PLOT_CN <- as.character(dat_climate$PLOT_CN)
###MAKING SEASONAL CLIMATE VARIABLES#####
# this is from function that only do within a single calendar year analysis

JunJulAug_ppt          <- making_climate_windows(data = dat_climate, 
                                                 variable = "ppt", 
                                                 start_month = 6, 
                                                 end_month = 8, 
                                                 year_shift = 0)
pJunJulAug_ppt         <- making_climate_windows(data = dat_climate, 
                                                 variable = "ppt", 
                                                 start_month = 6, 
                                                 end_month = 8, 
                                                 year_shift = -1)

seasonal_clim_dat       <- left_join(JunJulAug_ppt, pJunJulAug_ppt, by = c("PLOT_CN", "year"))
seasonal_clim_dat       <- seasonal_clim_dat %>%
                           mutate(prevJunAug_curJunAug_ppt = 
                           c_68_ppt + p_68_ppt) 


# Example usage with sample data
climate_data <- data.frame(
  PLOT_CN = rep(1, 36),
  year = rep(2000:2002, each = 12),
  month = rep(1:12, 3),
  ppt = rpois(36, 6)  # Example precipitation data
)



### okay new function for larger than 12 month aggregations of climate variables
## previous 16 month precipitation data for example

wateryear_climdat       <- make_climwin(data = dat_climate, 
                                        variable = "ppt", 
                                        reference_month = 9, 
                                        months_span = 16)


##okay this is just to make sure its doing what I think it is
# yay it works!!

# plot <- dat_climate %>% filter(PLOT_CN == 11806586010690) %>% 
#   select(PLOT_CN, month, year, ppt)

