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
write_csv(ut_meta_es, "tree-H/data/processed/ut_meta_es.csv")
