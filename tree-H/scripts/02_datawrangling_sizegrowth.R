

# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_meta <- read_csv(here::here("tree-H", "data", "processed", "wbp_meta_size.csv")) 
wbp_rw <- read_csv(here::here("tree-H", "data", "processed", "wbp_rw_all.csv"))

# load in function --------------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R"))

# class(wbp_meta$CRUISE_DATE) 
# range(wbp_meta$CRUISE_DATE) # so these are in diferent formats, range from 9/9 to 10/17 in 2023

# wrangle data ------------------------------------------------------------

wbp_size_dat <- wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_size_dat <- wbp_size_dat %>% dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, Year, RW, DIA)
wbp_size_dat$PLOT_CN <- as.character(wbp_size_dat$PLOT_CN)

# run the function
# wbp_test <- backcalculate_DBH(wbp_size_dat)
# 
# 
# library(tibble)
# wbp_test_df <- as_tibble(wbp_test)
# write_csv(wbp_test_df, "tree-H/data/processed/wbp_size_all.csv")
# 
# write to csv ------------------------------------------------------------

wbp_size_dat <- wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_size_dat <- wbp_size_dat %>% dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, Year, RW, DIA)
wbp_size_dat$PLOT_CN <- as.character(wbp_size_dat$PLOT_CN)

library(tibble)
wbp_test_df <- as_tibble(wbp_size_dat)
# this creates the dataframe to calcualte the wbp_size_all
write_csv(wbp_size_dat, "tree-H/data/processed/wbp_size_all.csv")

