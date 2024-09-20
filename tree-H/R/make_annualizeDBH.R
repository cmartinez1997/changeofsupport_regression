## Annualizing DBH
## Cecilia Martinez 
## April 15 2024
## cecimartinez333@gmail.com
## backcalculate tree diameter measurements using FIA DBH and ringwidth data


## add an argument, if_fast -- TRUE ..
backcalculate_DBH <- function(dat_bc, verbose = TRUE){
  # this only handles one core per tree
  # TO DO: throw warning if more than one core per tree - add this
  dat <- dat_bc |>
    mutate(RW_in = RW * 0.0393701) |> 
    dplyr::group_by(TRE_CN) |>
    dplyr::arrange(desc(Year)) |>
    dplyr::mutate(cum_dia_change = cumsum(dplyr::lag(RW_in, default = 0))) |> 
    dplyr::mutate(total_rw_change = 2 * cum_dia_change) |> #look into dplyr time series tools 
    dplyr::mutate(dia_est = DIA - total_rw_change) |> # this deals with only one DBH/DRC measurement at time of coring, think about more than one DBH meas
    ungroup()
  
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
  
  # Z <- dat %>%
  #   arrange(match(Year, years), match(TRE_CN, trees)) %>%
  #   # complete(Year, TRE_CN, fill = 0) %>%
  #   pull(dia_est, years, trees)
  
  for (i in 1:length(years)) {
    if (verbose) message("On year ", i, " out of ", length(years))
    for (j in 1:length(trees)) {
      
      
      temp <-   dat |>
        filter(Year == years[i], TRE_CN == trees[j]) |>  #assumes every year has a ring width measurement
        select(-"Year") |>
        select(-c("MEASYEAR", "DIA", "TRE_CN", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
        unlist() 
      
     
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


# wbp_rw_bc_final <- wbp_rw_bc |>
#   mutate(RW_in = RW * 0.0393701) |> # creates a new RW that converts from mm to inches
#   dplyr::arrange(TRE_CN, desc(Year)) |> # this sorts by the tree_id column and then by the year descending 
#   dplyr::group_by(TRE_CN) |> # grouping by the tree to perform following calculations separately for each tree, instead of summing across whole data set
#   dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) |> #cumulative sum of the lagged ring width values, cum sum restarts at 0 for each tree
#   dplyr::mutate(total_rw_change = 2 * cum_dia_change) |> 
#   dplyr::mutate(dia_est = DIA - total_rw_change)
# 
# # to possible consider multiple dbh code 
# 
# # Calculate proportional diameter estimates
# wbp_rw_bc_final <- wbp_rw_bc_final |>
#   group_by(TRE_CN) |> 
#   mutate(
#     proportion = ifelse(
#       Year > MEASYEAR1 & Year <= MEASYEAR2,
#       (Year - MEASYEAR1) / (MEASYEAR2 - MEASYEAR1),
#       NA_real_
#     ),
#     dia_est = case_when(
#       Year <= MEASYEAR1 ~ DIA1 - total_rw_change,
#       Year > MEASYEAR1 & Year <= MEASYEAR2 ~ DIA1 - proportion * (DIA1 - DIA2) - total_rw_change,
#       TRUE ~ DIA2 - total_rw_change
#     )
#   )

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