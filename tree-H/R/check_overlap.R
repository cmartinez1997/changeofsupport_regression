
check_overlap <- function(dat, dat_climate, verbose = TRUE) {
  if(!("PLOT_CN" %in% colnames(dat))) stop("dat must contain the variable PLOT_CN") 
  if(!("PLOT_CN" %in% colnames(dat_climate))) stop("dat_climate must contain the variable PLOT_CN") 
  
    tree_CN_missing      <- !(dat$PLOT_CN %in% dat_climate$PLOT_CN)
    tree_year_missing    <- !(dat$Year %in% dat_climate$growthyear)
    climate_CN_missing   <- !(dat_climate$PLOT_CN %in% dat$PLOT_CN)
    climate_year_missing <- !(dat_climate$growthyear %in% dat$Year) 
    if (verbose) {
        if (sum(tree_CN_missing) > 0) message("There are plot IDs in the tree ring data that are missing in the climate data")
        if (sum(tree_year_missing) > 0) message("There are years in the tree ring data that are missing in the climate data")
        if (sum(climate_CN_missing) > 0) message("There are plot IDs in the climate data that are missing in the tree ring data")
        if (sum(climate_year_missing) > 0) message("There are years in the climate data that are missing in the tree ring data")
    }
    return(
        list(
            tree_CN_missing      = tree_CN_missing,
            tree_year_missing    = tree_year_missing,
            climate_CN_missing   = climate_CN_missing,
            climate_year_missing = climate_year_missing
        )
    )
}