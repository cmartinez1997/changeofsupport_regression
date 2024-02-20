
make_X <- function(dat_climate, formula = NULL, functional_form = "linear", verbose = TRUE) {
    # NOTE: formula and functional_form are stubs for future development. Currently these don't do anything
    if (!is.null(formula)) stop("Currently we only NULL formulas")
    if(!(functional_form %in% c("linear"))) stop("Currently we only support a linear model")
    if (!(verbose %in% c(TRUE, FALSE))) stop("verbose must be either TRUE or FALSE")
    
    # Example format
    # row 1: year 1, site 1
    # row 2: year 1, site 2
    # etc
    years <- sort(unique(dat_climate$year))
    sites <- unique(dat_climate$PLT_CN)
    
    n_vars <- dat_climate |> 
        filter(year == years[1], PLT_CN == sites[1]) |> 
        pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
        select(-c("PLT_CN", "year")) |>
        # formula/functonal form here
        #
        ncol()
    
    var_names <- dat_climate |> 
        filter(year == years[1], PLT_CN == sites[1]) |> 
        pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
        select(-c("PLT_CN", "year")) |>
        names()
    
    X           <- matrix(0, length(years) * length(sites), n_vars)
    colnames(X) <- var_names
    row_names   <- rep(0, length(years) * length(sites))
    year_id     <- rep(0, length(years) * length(sites))
    site_id     <- rep(0, length(years) * length(sites))
    
    # This is slow but we can see about making it faster in the future
    idx <- 1
    for (i in 1:length(years)) {
        if (verbose) message("On year ", i, " out of ", length(years))
        for (j in 1:length(sites)) {
            X[idx, ] <- dat_climate |>
                filter(year == years[i], PLT_CN == sites[j]) |> 
                pivot_wider(names_from = month, values_from = c("tmin", "tmax", "ppt")) |>
                select(-c("PLT_CN", "year")) |>
                unlist()
            
            row_names[idx] <- paste(years[i], sites[j])
            year_id[idx]   <- years[i]
            site_id[idx]   <- sites[j]
            idx            <- idx + 1  
        }
    }
    rownames(X) <- row_names
    return(list(X = X, year_id = year_id, site_id = site_id))
}