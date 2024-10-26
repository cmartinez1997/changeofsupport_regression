
check_overlap <- function(dat, dat_climate, dat_bc = NULL, verbose = TRUE) {
  if(!("PLOT_CN" %in% colnames(dat))) stop("dat must contain the variable PLOT_CN") 
  if(!("PLOT_CN" %in% colnames(dat_climate))) stop("dat_climate must contain the variable PLOT_CN") 
  
    tree_CN_missing      <- !(dat$PLOT_CN %in% dat_climate$PLOT_CN)
    tree_year_missing    <- !(dat$Year %in% dat_climate$growthyear)
    climate_CN_missing   <- !(dat_climate$PLOT_CN %in% dat$PLOT_CN)
    climate_year_missing <- !(dat_climate$growthyear %in% dat$Year) 
    
    if (!is.null(dat_bc)) {
      if(!("TRE_CN" %in% colnames(dat_bc))) stop("dat_bc must contain the variable TRE_CN") 
      if(!("PLOT_CN" %in% colnames(dat_bc))) stop("dat_bc must contain the variable PLOT_CN") 
      
      bc_tree_CN_missing  <- !(dat_bc$PLOT_CN %in% dat_climate$PLOT_CN)
      bc_year_missing     <- !(dat_bc$Year %in% dat_climate$growthyear)
    } else {
      bc_tree_CN_missing  <- NULL
      bc_year_missing     <- NULL
    }
    
    if (verbose) {
      if (sum(tree_CN_missing) > 0) message("There are plot IDs in the tree ring data that are missing in the climate data")
      if (sum(tree_year_missing) > 0) message("There are years in the tree ring data that are missing in the climate data")
      if (sum(climate_CN_missing) > 0) message("There are plot IDs in the climate data that are missing in the tree ring data")
      if (sum(climate_year_missing) > 0) message("There are years in the climate data that are missing in the tree ring data")
      if (!is.null(bc_tree_CN_missing) && sum(bc_tree_CN_missing) > 0) message("There are tree IDs in the backcalculated diameter that are missing in the climate data")
      if (!is.null(bc_year_missing) && sum(bc_year_missing) > 0) message("There are years in the backcalculated diameter data that are missing in the climate  data")
    }
    
    return(
      list(
        tree_CN_missing      = tree_CN_missing,
        tree_year_missing    = tree_year_missing,
        climate_CN_missing   = climate_CN_missing,
        climate_year_missing = climate_year_missing,
        bc_tree_CN_missing   = bc_tree_CN_missing,
        bc_year_missing      = bc_year_missing
      )
    )
}