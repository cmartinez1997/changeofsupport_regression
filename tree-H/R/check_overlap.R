
check_overlap <- function(dat, dat_climate, verbose = TRUE) {
    tree_CN_missing      <- !(dat$PLT_CN %in% dat_climate$PLT_CN)
    tree_year_missing    <- !(dat$Year %in% dat_climate$year)
    climate_CN_missing   <- !(dat_climate$PLT_CN %in% dat$PLT_CN)
    climate_year_missing <- !(dat_climate$year %in% dat$Year)
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