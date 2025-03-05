## Wyoming/climwin data exploration/comparison# Make sure you have the RStudio project loaded and active
# Then run renv::activate() and renv::restore() when you first setup the project to install packages


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

# load in tree ring data --------------------------------------------------
#write final csv for the merged data frame
dat_rw <- read_csv(here::here("tree-H", "data", "processed", "wbp_rw_all.csv"))
dat_meta <- read_csv(here::here("tree-H", "data", "processed", "wbp_meta_all.csv"))

# load in climate data ----------------------------------------------------
## Refer to PRISM.R if you need to get climate data

ppt <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_ppt_df.csv"))
tmax <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmax_df.csv"))
tmin <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmin_df.csv"))

ppt$PLOT_CN <- as.character(ppt$PLOT_CN) 
ppt <- ppt %>%  dplyr::select(-TRE_CN) %>% 
  distinct(PLOT_CN, .keep_all = TRUE)
tmax$PLOT_CN <- as.character(tmax$PLOT_CN) 
tmax <- tmax %>% dplyr::select( -TRE_CN) %>% 
  distinct(PLOT_CN, .keep_all = TRUE)
tmin$PLOT_CN <- as.character(tmin$PLOT_CN) 
tmin <- tmin %>% dplyr::select( -TRE_CN) %>% 
  distinct(PLOT_CN, .keep_all = TRUE)

# turn climate data into long form and make new columns for year and month from column headers when the file is in wide format

tmin_long <- tmin %>%
  pivot_longer(cols = starts_with("tmin_"), names_to = "variable", values_to = "tmin") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable)

tmax_long <- tmax %>%
  pivot_longer(cols = starts_with("tmax_"), names_to = "variable", values_to = "tmax") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable)

ppt_long <- ppt %>% 
  pivot_longer(cols = starts_with("ppt_"), names_to = "variable", values_to = "ppt") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  dplyr::select(-variable)

# combine the data frames into one climate dataframe
tmin_long <- distinct(tmin_long, PLOT_CN, year, month, .keep_all = TRUE)
tmax_long <- distinct(tmax_long, PLOT_CN, year, month, .keep_all = TRUE)
ppt_long <- distinct(ppt_long, PLOT_CN, year, month, .keep_all = TRUE)

climate_dat <- left_join(tmin_long, tmax_long, by = c("PLOT_CN", "year", "month")) %>%
  left_join(., ppt_long, by = c("PLOT_CN", "year", "month"))

climate_dat <- climate_dat %>%
  dplyr::select(PLOT_CN, year, month, 
         LON = LON.x, LAT = LAT.x, 
         tmin, tmax, ppt)

# reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  dplyr::select(PLOT_CN, year, month, tmin, tmax, ppt) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

write_csv(climate_dat, "tree-H/data/processed/climate_dat_climwin.csv")

## making seasonal climate variables, refer to climate-growth analyses --> do this at some point, have not done yet
## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match

# load in the functions to add climate normal and biologically meaningful growthyears --------

source(here::here("tree-H", "R", "make_climate_functions.R")) 

unique_climate <- unique(climate_dat$PLOT_CN)
unique_wbp <- unique(dat_rw$PLOT_CN)

plots_match <- (climate_dat$PLOT_CN) %>% 
  intersect((dat_rw$PLOT_CN))  #66 plots match

dat_rw <- dat_rw %>% 
  filter(PLOT_CN %in% plots_match) # this makes sure that the plt_cn match for the climate and the ring width data frame 

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------

## note this will require the make_clim_norm function from 
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)
write_csv(norms, "tree-H/data/processed/climate_norms.csv")


## join climate normals to new_climate dataframe
climate_all <- climate_all %>% 
  left_join(norms, by = "PLOT_CN")

##scale vars 
climate_scaled <- scaling_climate(climate_all, vars_to_scale = c("tmin", "tmax", "ppt", "precip", "meantemp"))
# write and export as csvs --------------------------------------------------------------

write_csv(climate_scaled, "tree-H/data/processed/climate_scaled.csv")
write_csv(climate_all, "tree-H/data/processed/climate_all.csv")
