#wbp model 01 
# making seasonal climate window predictor variables

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

###MAKING SEASONAL CLIMATE VARIABLES#####

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



seasonal_clim_dat   <- left_join(JunJulAug_ppt, pJunJulAug_ppt, by = c("PLOT_CN", "year"))
seasonal_clim_dat <- seasonal_clim_dat %>%
  mutate(prev_current_ppt = 
           c_68_ppt + p_68_ppt)
