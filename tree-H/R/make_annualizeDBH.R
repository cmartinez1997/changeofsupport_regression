## Annualizing DBH
## Cecilia Martinez 
## April 15 2024
## cecimartinez333@gmail.com

## initial code from Courntey Giebink github repo: 
## modeified by Ceci Martinez
## backcalculate DBH usng ringwidth data

backcalculate_DBH <- function(dat_bc, verbose = TRUE){
  
  dat_bc <- dat_bc |>
    mutate(RW_in = RW * 0.0393701) %>% 
    dplyr::arrange(TRE_CN, desc(Year)) |>
    dplyr::group_by(TRE_CN) |>
    dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) %>% 
    dplyr::mutate(total_rw_change = 2 * cum_dia_change) |>
    dplyr::mutate(dia_est = DIA - total_rw_change)
  

  years <- sort(unique(dat_bc$Year))
  # NOTE: Ceci will delete first growth year (1895)
  # TO MAKE WORK UNTIL THEN, DELETE 1895 BY HAND AND DELETE THIS CODE LATER
    

  trees <- unique(dat_bc$TRE_CN)
  
  n_vars <- dat_bc |> 
    filter(Year == years[1], TRE_CN == trees[1]) |> 
    select(-c("Year", "MEASYEAR", "DIA", "TRE_CN", "RW", "RW_in", "cum_dia_change", "total_rw_change" )) |>
    # formula/functonal form here
    #
    ncol()
  
  
  var_names <- dat_bc |> 
    filter(Year == years[1], TRE_CN == trees[1]) |> 
    select(-c("Year", "MEASYEAR", "DIA", "TRE_CN", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
    
    
    names()
  
  Z             <- matrix(0, length(years) * length(trees), n_vars)
  colnames(Z)   <- var_names
  row_names     <- rep(0, length(years) * length(trees))
  year_id       <- rep(0, length(years) * length(trees))
  tree_id       <- rep(0, length(years) * length(trees))
  
  
  idx <- 1
  for (i in 1:length(years)) {
    if (verbose) message("On year ", i, " out of ", length(years))
    for (j in 1:length(trees)) {
      X[idx, ] <- dat_bc |>
        filter(Year == years[i], TRE_CN == trees[j]) |> 
        select(-"Year") |>
        select(-c("MEASYEAR", "DIA", "TRE_CN", "RW", "RW_in", "cum_dia_change", "total_rw_change")) |>
        unlist()
      
      row_names[idx]     <- paste(years[i], trees[j])
      year_id[idx] <- years[i]
      tree_id[idx]       <- trees[j]
      idx                <- idx + 1  
    }
  }
  rownames(X) <- row_names
  return(list(Z = Z, year_id = year_id, tree_id = tree_id))
}
  
backcalculate_DBH(wbp_rw_bc)
  


  