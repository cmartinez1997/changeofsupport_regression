##Data wnralginf for seasonal climate vars, this requires that you have followed the data wrangling stesps for 
## The rw dataframe, balccaulation of size, and climate growth df


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
source(here::here("tree-H", "R", "make_scaling_functions.R"))


# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_rw_all.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size_all.csv"))
#if you want climate scaled, then load "wbp_all_climate_data_scaled_all.csv"
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "climate_all.csv"))# Check the data sources for overlap and lack of overlap

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
# updated as of 03/07/2025
JulSep_ppt            <- making_climate_windows(data = dat_climate, 
                                                 variable = "ppt", 
                                                 start_month = 7, 
                                                 end_month = 9, 
                                                 year_shift = 0)
pJulSep_ppt         <- making_climate_windows(data = dat_climate, 
                                                 variable = "ppt", 
                                                 start_month = 7, 
                                                 end_month = 9, 
                                                 year_shift = -1)

seasonal_clim_dat       <- left_join(JulSep_ppt, pJulSep_ppt, by = c("PLOT_CN", "year"))
seasonal_clim_dat       <- seasonal_clim_dat %>%
                           mutate(prevJulSep_curJulSep_ppt = 
                                    c_79_ppt + p_79_ppt) 


### okay new function for larger than 12 month aggregations of climate variables
## previous 16 month precipitation data for example

wateryear_climdat       <- make_climwin(data = dat_climate, 
                                        variable = "ppt", 
                                        reference_month = 9, 
                                        months_span = 16) #these are for months previous Jun through current September 


wateryear_climdat       <- wateryear_climdat %>% 
                           rename(prevJun_currAug_ppt = aggregate_sum)

dat_seasonalclim        <- seasonal_clim_dat %>%
                            left_join(wateryear_climdat, by = c("PLOT_CN", "year"))

## FOR TMAX SEASONAL AGGREGATES

JulAug_tmax            <- making_climate_windows(data = dat_climate, 
                                                 variable = "tmax", 
                                                 start_month = 7, 
                                                 end_month = 8, 
                                                 year_shift = 0)
pJulAug_tmax           <- making_climate_windows(data = dat_climate, 
                                                 variable = "tmax", 
                                                 start_month = 7, 
                                                 end_month = 8, 
                                                 year_shift = -1)


AprMay_tmax             <- making_climate_windows(data = dat_climate, 
                                      variable = "tmax", 
                                      start_month = 4, 
                                      end_month = 5, 
                                      year_shift = 0)

pAprMay_tmax            <- making_climate_windows(data = dat_climate, 
                                                  variable = "tmax", 
                                                  start_month = 4, 
                                                  end_month = 5, 
                                                  year_shift = -1)

seasonal_tmax_dat       <- JulAug_tmax %>% 
                            left_join(pJulAug_tmax, by = c("PLOT_CN", "year")) %>% 
                            left_join(AprMay_tmax, by = c("PLOT_CN", "year")) %>% 
                            left_join(pAprMay_tmax, by = c("PLOT_CN", "year")) 
  
                           


all_seas_dat            <- seasonal_tmax_dat %>% 
                              left_join(dat_seasonalclim, by = c("PLOT_CN", "year")) 

all_seas_dat            <- all_seas_dat %>% 
                              rename(
                              JulAug_tmax = c_78_tmax,
                              pJulAug_tmax = p_78_tmax,
                              AprMay_tmax = c_45_tmax,
                              pAprMay_tmax = p_45_tmax,
                              JulSep_ppt = c_79_ppt,
                              pJulSep_ppt = p_79_ppt,
                              pJulSep_JulSep_ppt = prevJulSep_curJulSep_ppt
                            )


dat_climate_norms <- dat_climate %>% dplyr::select(PLOT_CN, year, meantemp, precip) %>% 
  distinct()

#now add normals to dataframe
all_seas_dat<- all_seas_dat %>%
  left_join(
    dat_climate_norms,
    by = c("PLOT_CN", "year")
  )


#now scale clim vars locally 
# scaled_local <- local_scale(data =test_data, clim_norms = "MAT", clim_locals = c("temp", "precip"))

seas_vars <- c("JulAug_tmax", "pJulAug_tmax", "AprMay_tmax", "pAprMay_tmax", "JulSep_ppt", "pJulSep_ppt", "pJulSep_JulSep_ppt")
clim_local_scale <- local_scale(data = all_seas_dat, clim_norms = c("meantemp", "precip"), clim_locals = seas_vars)
clim_global_scale <- global_scale(data = all_seas_dat, climate_vars = c("JulAug_tmax", "pJulAug_tmax", "AprMay_tmax", "pAprMay_tmax", "JulSep_ppt", "pJulSep_ppt", "pJulSep_JulSep_ppt", "precip", "meantemp"))



##WRITE THIS TO CSV
sapply(clim_local_scale, class)

#need to make this not a matrix for local scale
clim_local_scale <- clim_local_scale %>%
  mutate(across(where(is.list), ~ sapply(., as.character))) %>%
  mutate(across(where(is.matrix), ~ as.vector(.)))

write_csv(clim_local_scale, "tree-H/data/processed/clim_local_scale.csv")
write_csv(clim_global_scale, "tree-H/data/processed/clim_global_scale.csv")
# updated as of 02/03/2025

##okay this is just to make sure its doing what I think it is
# yay it works!!

# plot <- dat_climate %>% filter(PLOT_CN == 11806586010690) %>%
#   select(PLOT_CN, month, year, tmax)

