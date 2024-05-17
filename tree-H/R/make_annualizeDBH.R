## Annualizing DBH
## Cecilia Martinez 
## April 15 2024
## cecimartinez333@gmail.com

## initial code from Courntey Giebink github repo: 
## modeified by Ceci Martinez
## backcalculate DBH usng ringwidth data

backcalculate_DBH <- function(dat_bc, verbose = TRUE){
  # this only handles one core per tree
  # TO DO: throw warning if more than one core per tree - add this
  dat <- dat_bc |>
    mutate(RW_in = RW * 0.0393701) |> 
    dplyr::arrange(TRE_CN, desc(Year)) |>
    dplyr::group_by(TRE_CN) |>
    dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) |> 
    dplyr::ungroup() %>% 
      # dig into this lag function, verify that it produces the correct order of cum_dia_change
    dplyr::mutate(total_rw_change = 2 * cum_dia_change) |> #look into dplyr time series tools 
    dplyr::mutate(dia_est = DIA - total_rw_change) # this deals with only one DBH/DRC measurement at time of coring, think about more than one DBH meas
  

  years <- sort(unique(dat$Year))
  trees <- unique(dat$TRE_CN)
  
  # n_vars <- dat |> 
  #   filter(Year == years[1], TRE_CN == trees[1]) |> 
  #   select(-c( "MEASYEAR", "DIA", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
  #   # formula/functonal form here
  #   #
  #   ncol()
  # 
  # 
  # var_names <- dat |> 
  #   filter(Year == years[1], TRE_CN == trees[1]) |> 
  #   select(-c( "MEASYEAR", "DIA", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
  #   
  
  Z             <- matrix(0, length(years) * length(trees), 1)
  colnames(Z)   <- "dbh_bc"
  row_names     <- rep(0, length(years) * length(trees))
  year_id       <- rep(0, length(years) * length(trees))
  tree_id       <- rep(0, length(years) * length(trees))
  
  
  idx <- 1
  for (i in 1:length(years)) {
    if (verbose) message("On year ", i, " out of ", length(years))
    for (j in 1:length(trees)) {
      
      
      temp <-   dat |>
        filter(Year == years[i], TRE_CN == trees[j]) |>  #assumes every year has a ring width measurement
        select(-"Year") |>
        select(-c("PLT_CN", "TRE_CN", "MEASYEAR", "DIA", "TRE_CN", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
        unlist() #
    
    
      if (length(temp) > 0 ) { 
        Z[idx, ] <- temp
        row_names[idx]     <- paste(years[i], trees[j])
        year_id[idx]       <- years[i]
        tree_id[idx]       <- trees[j]
        idx                <- idx + 1  
        
      }
      
      
      
    }
  }
  Z <- Z[1:(idx-1), ]
  year_id <- year_id[1:(idx-1)]
  tree_id <- tree_id[1:(idx-1)]
  
  
  
  # rownames(X) <- row_names
  return(list(Z = Z, year_id = year_id, tree_id = tree_id))
}
  


# dat_bc <- wbp_rw_bc
# wbp_rw_bc_1896 <- wbp_rw_bc %>% 
#   filter(Year >= 1896)
# 
# backcalculate_DBH(wbp_rw_bc_1896)
# 
# 
# wbp_rw_bc <- wbp_rw_bc |>
#   mutate(RW_in = RW * 0.0393701) |> 
#   dplyr::arrange(TRE_CN, desc(Year)) |>
#   dplyr::group_by(TRE_CN) |>
#   dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) |> 
#   dplyr::mutate(total_rw_change = 2 * cum_dia_change) |>
#   dplyr::mutate(dia_est = DIA - total_rw_change)
#   
# 
# 
#   