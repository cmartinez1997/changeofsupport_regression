## This script is to make the climate dataframe for climate-tree ring growth analyses - including seasonal climate variables
## Cecilia Martinez 
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(here) # for relativizing file paths
library(readr)
library(tidyr)
library(dplyr)


# load in final merged dataframes ------------------------------------------------

wbp_meta_all <- read_csv("tree-H/data/processed/wbp_meta_all.csv")
wbp_meta_all$PLOT_CN <- as.character(wbp_meta_all$PLOT_CN)
wbp_rw_all <- read_csv("tree-H/data/processed/wbp_rw_all.csv")

# load in climate data ----------------------------------------------------
# this was most recently updated on 01/27/2025 to include Nick Kichas data for wbp
ppt <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_ppt_df.csv"))
tmax <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmax_df.csv"))
tmin <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmin_df.csv"))

# turn climate data into long form and make new columns for year and month from column headers when the file is in wide format

tmin_long <- tmin %>%
  pivot_longer(cols = starts_with("tmin_"), names_to = "variable", values_to = "tmin") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable, -TRE_CN)

tmax_long <- tmax %>%
  pivot_longer(cols = starts_with("tmax_"), names_to = "variable", values_to = "tmax") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable, -TRE_CN)

ppt_long <- ppt %>% 
  pivot_longer(cols = starts_with("ppt_"), names_to = "variable", values_to = "ppt") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  dplyr::select(-variable, -TRE_CN)

# Combine the data frames into one climate dataframe
tmin_long <- distinct(tmin_long, PLOT_CN, year, month, .keep_all = TRUE)
tmax_long <- distinct(tmax_long, PLOT_CN, year, month, .keep_all = TRUE)
ppt_long <- distinct(ppt_long, PLOT_CN, year, month, .keep_all = TRUE)

climate_dat <- left_join(tmin_long, tmax_long, by = c("PLOT_CN", "year", "month")) %>%
  left_join(., ppt_long, by = c("PLOT_CN", "year", "month"))


#reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  dplyr::select(PLOT_CN, year, month, tmin, tmax, ppt) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

climate_dat$PLOT_CN <- as.character(climate_dat$PLOT_CN)
write_csv(climate_dat, "tree-H/data/processed/climate_dat.csv") ## use this for climwin and for seasonal climate vars
# this was updated last at 02/13/2025

## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match
unique_climate <- unique(climate_dat$PLOT_CN)
unique_wbp <- unique(wbp_rw_all$PLOT_CN)

plots_match <- unique(climate_dat$PLOT_CN) %>% 
  intersect(unique(wbp_rw_all$PLOT_CN))  #128 plots match

wbp_rw_all <- wbp_rw_all %>% 
  filter(PLOT_CN %in% plots_match) # this makes sure that the plt_cn match for the climate and the ring width data frame 

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------
source(here::here("tree-H", "R", "make_climate_functions.R")) 

## note this will require the make_clim_norm function
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2010)
as.data.frame(norms)

#add lat/lon to data frame so we can plot this spatially on a map at some point
norms <- norms %>%
  left_join(wbp_meta_all %>% dplyr::select(PLOT_CN, LAT, LON, dataset), by = "PLOT_CN") %>% 
  drop_na() #drop rows with nas

## join climate normals to new_climate
climate_all <- climate_all %>% 
  left_join(norms, by = "PLOT_CN")

# write and export as csvs --------------------------------------------------------------
#okay this is updated as of 02/03/2024
write_csv(wbp_rw_all, "tree-H/data/processed/wbp_dat_rw.csv")
write_csv(climate_all, "tree-H/data/processed/wbp_datclimate.csv")
write_csv(norms, "tree-H/data/processed/clim_norms.csv")



