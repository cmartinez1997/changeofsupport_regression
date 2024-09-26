

# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_meta <- read_csv(here::here("tree-H", "data", "raw", "wbp_cores_2020-2023.csv")) 
wbp_meta <- wbp_meta %>% 
  rename(TRE_CN = CN)
wbp_rw <- read_csv(here::here("tree-H", "data", "raw", "wbp_new_rwl.csv"))
wbp_rw <- wbp_rw %>% 
  rename(TRE_CN = CN)

# load in function --------------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R"))

class(wbp_meta$CRUISE_DATE) 
range(wbp_meta$CRUISE_DATE) # so these are in diferent formats, range from 9/9 to 10/17 in 2023

# wrangle data ------------------------------------------------------------

wbp_size_dat <- wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_size_dat <- wbp_size_dat %>% select(TRE_CN, PLOT_CN, MEASYEAR, Year, RW, DIA)
wbp_size_dat$PLOT_CN <- as.character(wbp_size_dat$PLOT_CN)

# testing the function
wbp_test <- backcalculate_DBH(wbp_size_dat)


# write to csv ------------------------------------------------------------

write_csv(wbp_size_dat, "tree-H/data/processed/wbp_size.csv")

