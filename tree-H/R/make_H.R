
make_H <- function(dat, fit_list, verbose = TRUE, n_message = 1000) {
    # Create a matrix to align the ring width data with the climate data
    # Eventually make this sparse
    H <- matrix(0, nrow(dat), length(fit_list$year_id))
    for (i in 1:nrow(dat)) {
        if (verbose) {
            if (i %% n_message == 0){
                message("On record ", i, " out of ", nrow(dat))  
            } 
        }
        dat_row <- dat[i, ]
        idx <- which((dat_row$Year == fit_list$year_id) & (dat_row$PLOT_CN == fit_list$site_id))
        if (length(idx) > 1) {
            stop(paste("Multiple records exist for record", i))
        }
        
        H[i, idx] <- 1
    }
    return(H)
}