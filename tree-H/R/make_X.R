
make_X <- function(dat_climate, formula = NULL, functional_form = "linear", verbose = TRUE) {
    # NOTE: formula and functional_form are stubs for future development. Currently these don't do anything
    if (!is.null(formula)) stop("Currently we only NULL formulas")
    if(!(functional_form %in% c("linear"))) stop("Currently we only support a linear model")
    if (!(verbose %in% c(TRUE, FALSE))) stop("verbose must be either TRUE or FALSE")
    
    # Example format
    # row 1: year 1, site 1
    # row 2: year 1, site 2
    # etc
    growthyears <- sort(unique(dat_climate$growthyear))
    # NOTE: Ceci will delete first growth year (1895)
    # TO MAKE WORK UNTIL THEN, DELETE 1895 BY HAND AND DELETE THIS CODE LATER
    sites <- unique(dat_climate$PLOT_CN)
    
    n_vars <- dat_climate |> 
        filter(growthyear == growthyears[2], PLOT_CN == sites[1]) |>
        pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
        select(-c("PLOT_CN", "year", "growthyear")) |>
        # formula/functonal form here
        #
        ncol()
    
    var_names <- dat_climate |> 
        filter(growthyear == growthyears[1], PLOT_CN == sites[1]) |> 
        pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
        select(-c("PLOT_CN", "year", "growthyear")) |>
        names()
    
    X             <- matrix(0, length(growthyears) * length(sites), n_vars)
    colnames(X)   <- var_names
    row_names     <- rep(0, length(growthyears) * length(sites))
    growthyear_id <- rep(0, length(growthyears) * length(sites))
    site_id       <- rep(0, length(growthyears) * length(sites))
    
    # This is slow but we can see about making it faster in the future
    idx <- 1
    for (i in 1:length(growthyears)) {
        if (verbose) message("On year ", i, " out of ", length(growthyears))
        for (j in 1:length(sites)) {
            X[idx, ] <- dat_climate |>
                filter(growthyear == growthyears[i], PLOT_CN == sites[j]) |> 
                select(-"year") |>
                pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
                select(-c("PLOT_CN", "growthyear")) |>
                unlist()
            
            row_names[idx]     <- paste(growthyears[i], sites[j])
            growthyear_id[idx] <- growthyears[i]
            site_id[idx]       <- sites[j]
            idx                <- idx + 1  
        }
    }
    rownames(X) <- row_names
    return(list(X = X, year_id = growthyear_id, site_id = site_id))
}