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
wbp_rw_bc <- wbp_rw_bc %>% select(CN, TRE_CN, Year, RW, MEASYEAR, DIA)
wbp_rw_bc <- wbp_rw_bc %>% filter(!is.na(TRE_CN))
wbp_rw_bc <- wbp_rw_bc %>% select(-CN) 

length(unique(wbp_rw_bc$TRE_CN)) #219 unique trees

# make identifiers characters

wbp_rw_bc$TRE_CN <- as.character(wbp_rw_bc$TRE_CN)

# make dbh dataframe
wbp_dbh_bc <- wbp_meta %>% select(TRE_CN, MEASYEAR, DIA)
wbp_dbh_bc <- wbp_dbh_bc %>% 
  filter(!is.na(TRE_CN)) %>% 
  rename(DIA_t = DIA)

write_csv(wbp_rw_bc, "tree-H/data/processed/wbp_rw_bc.csv")
write_csv(wbp_dbh_bc, "tree-H/data/processed/wbp_dbh_bc.csv")


# load in function to do backcauclation ----------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R")) 



## do this for utah

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

es_rw <- read_csv("tree-H/data/processed/ut_rw_ES.csv")
es_meta <- read_csv("tree-H/data/processed/ut_meta_es.csv")


# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe

es_rw_bc <- left_join(es_rw, es_meta)
es_rw_bc <- es_rw_bc %>% select(CN, TRE_CN, Year, RW, MEASYEAR, DIA)
es_rw_bc <- es_rw_bc %>% select(-CN) 

length(unique(es_rw_bc$TRE_CN)) #52 unique trees for utah

# make identifiers characters

es_rw_bc$TRE_CN <- as.character(es_rw_bc$TRE_CN)

write_csv(es_rw_bc, "tree-H/data/processed/es_rw_bc.csv")


# load in function to do backcauclation ----------------------------------------------------

source(here::here("tree-H", "R", "make_annualizeDBH.R")) 


# run function -----------------------------------------------------------
backcalculate_DBH(dat_rw = wbp_rw_bc, dat_dbh = wbp_dbh_bc)



dat_bc <- dat_bc |>
  mutate(RW_in = RW * 0.0393701) |> 
  dplyr::arrange(TRE_CN, desc(Year)) |>
  dplyr::group_by(TRE_CN) |>
  dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) |> 
  dplyr::mutate(total_rw_change = 2 * cum_dia_change) |>
  dplyr::mutate(dia_est = DIA - total_rw_change)

