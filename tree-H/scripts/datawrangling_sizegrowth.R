

# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_meta <- read_csv(here::here("tree-H", "data", "raw", "wbp_cores_2020-2023.csv"))
wbp_rw <- read_csv(here::here("tree-H", "data", "raw", "wbp_new_rwl.csv"))

# load in function --------------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R"))


# wrangle data ------------------------------------------------------------

wbp_size_dat <- wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_size_dat <- wbp_size_dat %>% select(CN, MEASYEAR, Year, RW, DIA) %>% 
  rename(TRE_CN = CN, DIA_t = DIA)

wbp_size_dat$TRE_CN <- as.character(wbp_size_dat$TRE_CN)

# run function -----------------------------------------------------------
annualizedDBH <- wbp_size_dat %>% 
  group_by(TRE_CN) %>% 
  arrange(TRE_CN,Year) %>% 
    mutate(DIA_bc = backcalculate_DBH())

#did DIA_bc = DIA_t when last RW year was 1 year less than MEASYEAR
check_data <- annualizedDBH[which(annualizedDBH$Year + 1 == annualizedDBH$MEASYEAR),]

# write to csv ------------------------------------------------------------

write_csv(annualizedDBH, "tree-H/data/processed/imputed_DIA.csv")

