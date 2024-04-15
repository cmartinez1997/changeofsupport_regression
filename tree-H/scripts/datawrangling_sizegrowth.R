

# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark.txt"))

# load in function --------------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R")) 

# wrangle data ------------------------------------------------------------

wbp_size_dat <- wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_size_dat <- wbp_size_dat %>% select(CN, TRE_CN, MEASYEAR, Year, RW, DIA)
wbp_size_dat <- wbp_size_dat %>% filter(!is.na(TRE_CN))
wbp_size_dat <- wbp_size_dat %>% select(-CN) %>% 
  rename(DIA_t = DIA)

wbp_size_dat$TRE_CN <- as.character(wbp_size_dat$TRE_CN)

# run function -----------------------------------------------------------
annualizedDBH <- wbp_size_dat %>% 
  group_by(TRE_CN) %>% 
  arrange(TRE_CN,Year) %>% 
    mutate(DIA_bc = backcalculate_DBH(TRE_CN = TRE_CN, MEASYEAR = MEASYEAR, Year = Year, RW = RW, DIA_t = DIA_t))





