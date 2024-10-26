## This script is to make the size dataframe for annualization of dbh
## Cecilia Martinez 
## July 15 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

ut_rw <- read_csv(here::here("tree-H", "data", "raw", "Q_Utah_Courtney_ringwidth.csv"))
ut_meta <- read_csv(here::here("tree-H", "data", "raw", "T_Utah_periodic_metadata.csv"))

# load in function --------------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R")) 

# wrangle data ------------------------------------------------------------

ut_size_dat <- ut_rw <- left_join(ut_rw, ut_meta)
ut_size_dat <- ut_size_dat %>% select(CN, TRE_CN, MEASYEAR, Year, RW, DIA)
ut_size_dat <- ut_size_dat %>% filter(!is.na(TRE_CN))
ut_size_dat <- ut_size_dat %>% select(-CN) 

toy_ut_size_dat <- ut_size_dat[1:1000, ]

ut_size_dat$TRE_CN <- as.character(ut_size_dat$TRE_CN)

ut_size_dat <- ut_size_dat %>% 
  mutate(TRE_CN=factor(TRE_CN))

# run function -----------------------------------------------------------
annualizedDBH <- ut_size_dat %>% 
  group_by(TRE_CN) %>% 
  arrange(TRE_CN,Year) %>% 
  mutate(DIA_bc = backcalculate_DBH(TRE_CN = TRE_CN, MEASYEAR = MEASYEAR, Year = Year, RW = RW, DIA = DIA))

backcalculate_DBH(toy_ut_size_dat)

z_mat <- backcalculate_DBH(toy_ut_size_dat)

nrow(dat_climate)
dat_climate %>% 
  unique() %>% 
  nrow()


#did DIA_bc = DIA_t when last RW year was 1 year less than MEASYEAR
check_data <- annualizedDBH[which(annualizedDBH$Year + 1 == annualizedDBH$MEASYEAR),]

# write to csv ------------------------------------------------------------

write_csv(annualizedDBH, "tree-H/data/processed/imputed_DIA.csv")

