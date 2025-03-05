## Making WBP data frame from nfi data, not tree ring but occurrence data


# Load necessary packages -------------------------------------------------

library(here) # for relativizing file paths
library(readr)
library(tidyr)
library(dplyr)
library(ggspatial)
library(ggmap)
library(tigris)
library(sf)
library(leaflet)


# load in tree wbp occurrence and plot dfs merged dataframes ------------------------------------------------
tree <- read_csv("tree-H/data/raw/nfi_wbp/TREE_WBP_IW.csv") #37487 trees
plot <- read_csv("tree-H/data/raw/nfi_wbp/PLOT_MT-ID-WY.csv")
cond <- read_csv("tree-H/data/raw/nfi_wbp/COND_MT-ID-WY.csv")

#filtering data frame
plot <- plot %>% 
  rename(PLT_CN = CN) %>% 
  dplyr::select(c(PLT_CN, INVYR, STATECD, COUNTYCD, 
                  PLOT, MEASYEAR, DESIGNCD, LAT, 
                  LON, ELEV, PLOT_STATUS_CD))

tree <- tree %>% 
  dplyr::select(c(TRE_CN, PLT_CN, PREV_TRE_CN, INVYR, STATECD, COUNTYCD, 
                  PLOT, SUBP, TREE, CONDID, DIA, STATUSCD, TPA_UNADJ))
tree_fia <- tree %>% 
  left_join(plot, by = c("PLT_CN", "INVYR", "STATECD", "COUNTYCD", "PLOT"))
  
# filtering condition to only "accessible forest land" by FIA classification
cond <- cond %>% 
  filter(COND_STATUS_CD == 1) #27,328 condition plots? 

# for every state, county, plot, adn subplot in tree df, want to check plot df and pull that associated plot-level info
map_interiorwest <- get_stadiamap(c(-117.2, 42, -108.5, 49), zoom = 4)
ggmap(map_interiorwest)


iw_states <- states(state = c("MT", "ID", "WY") , class = "sf")
ggplot(data = iw_states) +
  geom_sf(col = "navyblue", fill = "lightblue", alpha = 0.3) +
  ggtitle("IW") +
  theme_classic()


leaflet(states) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5) %>%
  setView(-98.5795, 39.8282, zoom=3)
# mapping geograhpically fia vs tree ring ---------------------------------
#plotting whitebark pine occurrence in plots everywehre its been observed in fia against the tree ring data

tree_fia_sf <- st_as_sf(tree_fia, coords = c("LON", "LAT"), crs = 4326) 
