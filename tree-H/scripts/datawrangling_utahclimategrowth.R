## This script is to make the climate dataframe for climate-tree ring growth analyses - including seasonal climate variables
## Cecilia Martinez 
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)
library(DBI) #required to access FIADB

# load in wbp tree ring data and associated metadata ----------------------

ut_rw <- read_csv(here::here("tree-H", "data", "raw", "Q_Utah_Courtney_ringwidth.csv"), col_names = T)
ut_meta <- read_csv(here::here("tree-H", "data", "raw", "T_Utah_periodic_metadata.csv"), col_names = T)

# get FIADB data, connect to database to get tables for utah to find lat/lon

# Getting data from SQL ---------------------------------------------------

# Before we can get csv files, we have to download SQL db for our states of
# interest to our working directory by going to FIA datamart
# https://experience.arcgis.com/experience/3641cea45d614ab88791aef54f3a1849/

# GET UT DATA ----------------------------------------------------------
# Set path to where you want the CSVs to appear
path_out_UT <- "tree-H/data/raw/"

# reference the database and checking the connection and table names
con_UT <-  dbConnect(RSQLite::SQLite(), "SQLite_FIADB_UT.db 2")
dbGetQuery(conn = con_UT, "SELECT name FROM sqlite_master WHERE type='table';")

# subset tables that you want
db_file_names_UT <-
  c("TREE",
    "PLOT",
    "COND")

# loop through specified tables and write them at csvs
for (i in db_file_names_UT) {
  write_csv(dbReadTable(conn = con_UT, name = i),
            file = paste0(path_out_UT, "UT_", i, ".csv"))
}

# close connection
dbDisconnect(con_UT)


# wrangle data frame and add fiadb level values
ut_rw <- left_join(ut_rw, ut_meta)
ut_rw_ES <- ut_rw %>% 
  filter(SPCD == 93) %>% 
  select("TRE_CN", "PLT_CN", "Year", "RW")





length(unique(ut_rw_ES$TRE_CN)) #52 unique trees

UT_tree <- read_csv("tree-H/data/raw/UT_TREE.csv")
UT_plot <- read_csv("tree-H/data/raw/UT_PLOT.csv")
UT_cond <- read_csv("tree-H/data/raw/UT_COND.csv")


tree_red <- UT_tree[,c("CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","CR","SITREE","TPA_UNADJ","DIST","AGENTCD")]
colnames(tree_red)[colnames(tree_red)=="CN"] <- "TRE_CN"

# filter out only trees in the rw dataframe
tree_ES <- tree_red %>%
  filter(TRE_CN %in% ut_rw_ES$TRE_CN)

plot_ES <- plot_red %>%
  filter(PLT_CN %in% ut_rw_ES$PLT_CN)  #46 plots with 52 trees

ut_meta_es <- ut_meta %>%
  filter(TRE_CN %in% ut_rw_ES$TRE_CN) %>% 
  left_join(plot_ES %>% select(PLT_CN, LAT, LON), by = "PLT_CN")

write_csv(ut_rw_ES, "tree-H/data/processed/ut_rw_ES.csv")
write_csv(ut_meta_es, "tree-H/data/processed/ut_meta_ES.csv")

ut_rw_ES <- read_csv(here::here("tree-H", "data", "processed", "ut_rw_ES.csv"))

# load in the climate data ------------------------------------------------

ppt_ut <- read_csv(here::here("tree-H", "data", "raw", "utah_ppt_df.csv"))
tmax_ut <- read_csv(here::here("tree-H", "data","raw", "utah_tmax_df.csv"))
tmin_ut <- read_csv(here::here("tree-H", "data", "raw", "utah_tmin_df.csv"))
vpd_ut <- read_csv(here::here("tree-H", "data", "raw", "utah_vpd_df.csv"))

# turn climate data into long form and make new columns for year and month from column headers when the file is in wide format

ut_tmin_long <- tmin_ut %>%
  pivot_longer(cols = starts_with("tmin_"), names_to = "variable", values_to = "tmin") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  select(-variable, -CORE_CN, -LAT, -LON)


ut_tmax_long <- tmax_ut %>%
  pivot_longer(cols = starts_with("tmax_"), names_to = "variable", values_to = "tmax") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  select(-variable, -CORE_CN, -LAT, -LON)

ut_ppt_long <- ppt_ut %>% 
  pivot_longer(cols = starts_with("ppt_"), names_to = "variable", values_to = "ppt") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  select(-variable, -CORE_CN, -LAT, -LON)

ut_vpd_long <- vpd_ut %>% 
  pivot_longer(cols = starts_with("vpd_"), names_to = "variable", values_to = "vpd") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  select(-variable, -CORE_CN, -LAT, -LON)

# Combine the data frames into one climate dataframe
ut_tmin_long <- distinct(ut_tmin_long, TRE_CN, PLT_CN, year, month, .keep_all = TRUE)
ut_tmax_long <- distinct(ut_tmax_long, TRE_CN, PLT_CN, year, month, .keep_all = TRUE)
ut_ppt_long <- distinct(ut_ppt_long, TRE_CN, PLT_CN, year, month, .keep_all = TRUE)
ut_vpd_long <- distinct(ut_vpd_long, TRE_CN, PLT_CN, year, month, .keep_all = TRUE)

climate_dat <- left_join(ut_tmin_long, ut_tmax_long, by = c("TRE_CN", "PLT_CN", "year", "month")) %>%
  left_join(., ut_ppt_long, by = c( "TRE_CN","PLT_CN", "year", "month")) %>% 
  left_join(., ut_vpd_long, by = c("TRE_CN", "PLT_CN", "year", "month"))

#reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  select(TRE_CN, PLT_CN, year, month, tmin, tmax, ppt, vpd) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

climate_dat$TRE_CN <- as.character(climate_dat$TRE_CN)
climate_dat$PLT_CN <- as.character(climate_dat$PLT_CN)
## making seasonal climate variables, refer to climate-growth analyses --> do this at some point, have not done yet

## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match

unique_climate <- unique(climate_dat$TRE_CN)
unique_es <- unique(ut_rw_ES$TRE_CN)

trees_match <- unique(climate_dat$TRE_CN) %>% 
  intersect(unique(ut_rw_ES$TRE_CN))  #52 trees match, yay

plots_match <- unique(climate_dat$PLT_CN) %>% 
  intersect(unique(ut_rw_ES$PLT_CN))  #46 plots match

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------

climate_dat <- climate_dat %>% 
  select(-TRE_CN)


source(here::here("tree-H", "R", "make_climate_functions.R")) 


## note this will require the make_clim_norm function
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)

## join climate normals to new_climate
climate_all <- climate_all %>% 
  left_join(norms, by = "PLT_CN") %>% 
  relocate(growthyear, .after = year)

# write and export as csvs --------------------------------------------------------------

write_csv(ut_rw_ES, "tree-H/data/processed/utah_climate_growth_rw.csv")
write_csv(climate_all, "tree-H/data/processed/utah_climate_data_all.csv")
