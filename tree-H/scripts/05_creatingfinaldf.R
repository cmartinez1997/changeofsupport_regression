


dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_dat_rw.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size_all.csv"))
#if you want climate scaled, then load "wbp_all_climate_data_scaled_all.csv"
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "clim_local_scale.csv"))#
dat_climate <- dat_climate %>% drop_na()
# for now getting rid of the mis-calibrated data
dat <- dat %>% 
  filter(!(TRE_CN %in% c("23R48", "23T255", "22T1755", "H151B")))
dat_bc <- dat_bc %>% 
  filter(!(TRE_CN %in% c("23R48", "23T255", "22T1755", "H151B")))


# Create the size backcalculated matrix using the function backcalculate_DBH
Z_list     <- backcalculate_DBH(dat_bc)
# rows are trees x years and it is just a single column with backcalculated values
Z          <- Z_list$Z
year_id_Z  <- Z_list$year_id
tree_id    <- Z_list$tree_id
site_id_Z  <- Z_list$site_id
# write_csv(wbp_test, "tree-H/data/processed/wbp_size_all.csv")

str(dat)
str(dat_bc)
data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id) %>% 
  left_join(dat) 

dat$PLOT_CN <- as.character(dat$PLOT_CN)
dat_fit <- dat %>% left_join(dat_bc) %>% left_join(data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id)) %>% 
  rename(year = Year)
dat_climate$PLOT_CN <- as.character(dat_climate$PLOT_CN)

# so we have to create a dataframe to account for seasonal and/or time varying monthly climate variables
###THIS IS THE FINAL DATAFRAME W WILL USE FOR ANALYSIS
dat_all_seas <- dat_fit %>% left_join(dat_climate, by = c("PLOT_CN", "year"))

write_csv(dat_all_seas, "tree-H/data/processed/final_df.csv")

