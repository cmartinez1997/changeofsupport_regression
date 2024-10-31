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

# data reading and wrangling ----------------------------------------------

# Do analysis for all legacy data ring data and merge with the 2020-2023 data this is from 1980s to 2018
# load the pre2018 data ---------------------------------------------------

# for the infrastructure of our code so far, rename CORE_CN to TRE_CN, which is probably okay since there is only one core per tree anyway
wbp_meta_18 <- read_csv(here::here("tree-H", "data", "raw", "wbp_meta_2018.csv")) %>% 
  dplyr::select(-TRE_CN) %>% 
  rename(TRE_CN = CORE_CN)

wbp_meta_18$PLOT_CN <- as.character(wbp_meta_18$PLOT_CN)
wbp_meta_18$TRE_CN <- as.character(wbp_meta_18$TRE_CN)

wbp_rw_18 <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark_rw.txt"))
wbp_rw_18 <- wbp_rw_18 %>% 
  rename(TRE_CN = CN) %>% 
  dplyr::select(-SPCD)
wbp_rw_18$TRE_CN <- as.character(wbp_rw_18$TRE_CN)


# now filter out all the data that's been verified!
# only keep trees where verified = y
wbp_meta_18 <- wbp_meta_18 %>% 
  filter(VERIFY == "y")

# okay now only keep rw data where TRE_CN matches with the filtered metadata
wbp_rw_18 <- wbp_rw_18 %>%
  filter(TRE_CN %in% wbp_meta_18$TRE_CN)
length(unique(wbp_rw_18$TRE_CN))

wbp_rw_18 <- left_join(wbp_rw_18, wbp_meta_18)
wbp_rw_18 <- wbp_rw_18 %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW)

# load the 2020-2023 data -------------------------------------------------

# load in wbp tree ring data and associated metadata ----------------------

wbp_meta_23 <- read_csv(here::here("tree-H", "data", "raw", "wbp_cores_2020-2023.csv")) 
wbp_meta_23 <- wbp_meta_23 %>% 
  rename(TRE_CN = CN) %>% 
  rename(LAT = LAT_FUZZED) %>% 
  rename(LON = LON_FUZZED) %>% 
  rename(ELEV = ELEV_FUZZED) 

wbp_meta_23$PLOT_CN <- as.character(wbp_meta_23$PLOT_CN)

wbp_rw_23 <- read_csv(here::here("tree-H", "data", "raw", "wbp_new_rwl.csv"))
wbp_rw_23 <- wbp_rw_23 %>% 
  rename(TRE_CN = CN)

length(unique(wbp_rw_23$TRE_CN))

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe
# no TRE_CN 
wbp_rw_23 <- left_join(wbp_rw_23, wbp_meta_23)
wbp_rw_23 <- wbp_rw_23 %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW)

# combine the dataframes --------------------------------------------------

# ring width df 
wbp_rw_all <- bind_rows(wbp_rw_23, wbp_rw_18)
wbp_meta_all <- bind_rows(wbp_meta_23, wbp_meta_18)

# saving the data 
write_csv(wbp_rw_all, "tree-H/data/processed/wbp_rw_all.csv")
write_csv(wbp_meta_all, "tree-H/data/processed/wbp_meta_all.csv")

# truncate the years so that they only span the years of climate data that we have, so 1896 onwards
#wbp_rw <- wbp_rw %>%
#  filter(Year >= 1896), don't do this actually

# load in climate data ----------------------------------------------------

## GO TO PRISM.R to get climate data

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

# reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  dplyr::select(PLOT_CN, year, month, tmin, tmax, ppt) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

write_csv(climate_dat_climwin, "tree-H/data/processed/climate_dat_climwin.csv")

## making seasonal climate variables, refer to climate-growth analyses --> do this at some point, have not done yet
## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match

# load in the functions to add climate normal and biologically meaningful growthyears --------

source(here::here("tree-H", "R", "make_climate_functions.R")) 

unique_climate <- unique(climate_dat$PLOT_CN)
unique_wbp <- unique(wbp_rw_all$PLOT_CN)

plots_match <- (climate_dat$PLOT_CN) %>% 
  intersect((wbp_rw_all$PLOT_CN))  #66 plots match

wbp_rw_all <- wbp_rw_all %>% 
  filter(PLOT_CN %in% plots_match) # this makes sure that the plt_cn match for the climate and the ring width data frame 

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------

## note this will require the make_clim_norm function from 
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)

## join climate normals to new_climate dataframe
climate_all <- climate_all %>% 
  left_join(norms, by = "PLOT_CN")

##scale vars 
climate_scaled <- scaling_climate(climate_all, vars_to_scale = c("tmin", "tmax", "ppt", "precip", "meantemp"))
# write and export as csvs --------------------------------------------------------------

write_csv(wbp_rw_all, "tree-H/data/processed/wbp_all_climate_growth_rw.csv")
write_csv(climate_scaled, "tree-H/data/processed/wbp_all_climate_data_all.csv")


# okay now wrangle size data frame ----------------------------------------

wbp_size_dat <- wbp_rw_all <- left_join(wbp_rw_all, wbp_meta_all)
wbp_size_dat <- wbp_size_dat %>% dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, Year, RW, DIA)
wbp_size_dat$PLOT_CN <- as.character(wbp_size_dat$PLOT_CN)

write_csv(wbp_size_dat, "tree-H/data/processed/wbp_size_all.csv")

