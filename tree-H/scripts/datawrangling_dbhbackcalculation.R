#3 Make tree ring df, and tree_df for backcalcualtion
## Cecilia Martinez 
## May 01 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark.txt"))


# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe

wbp_rw_bc <- left_join(wbp_rw, wbp_meta)
wbp_rw_bc <- wbp_rw_bc %>% select(CN, TRE_CN, Year, RW, MEASYEAR)
wbp_rw_bc <- wbp_rw_bc %>% filter(!is.na(TRE_CN))
wbp_rw_bc <- wbp_rw_bc %>% select(-CN)

length(unique(wbp_rw_bc$TRE_CN)) #219 unique trees

# make identifiers characters

wbp_rw_bc$TRE_CN <- as.character(wbp_rw_bc$TRE_CN)
wbp_rw_bc$PLT_CN <- as.character(wbp_rw_bc$PLT_CN)


# make dbh dataframe
wbp_dbh_bc <- wbp_meta %>% select(TRE_CN, MEASYEAR, DIA)
wbp_dbh_bc <- wbp_dbh_bc %>% 
  filter(!is.na(TRE_CN)) %>% 
  rename(DIA_t = DIA)

write_csv(wbp_rw_bc, "tree-H/data/processed/wbp_rw_bc.csv")
write_csv(wbp_dbh_bc, "tree-H/data/processed/wbp_dbh_bc.csv")


# load in function to do backcauclation ----------------------------------------------------

source(here::here("tree-H", "R", "make_Z.R")) 


# run function -----------------------------------------------------------
backcalculate_DBH(dat_rw = wbp_rw_bc, dat_dbh = wbp_dbh_bc)


