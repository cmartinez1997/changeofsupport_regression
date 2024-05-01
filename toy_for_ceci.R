

toy_size_dat <- rbind(toy_size_dat, toy_size_dat2)

toy_size_dat <- wbp_rw_bc |>
  mutate(RW_in = RW * 0.0393701) %>% 
  dplyr::arrange(TRE_CN, desc(Year)) |>
  dplyr::group_by(TRE_CN) |>
  dplyr::mutate(cumulative_dia_change = cumsum(RW_mm)) |>
  dplyr::mutate(total_rw_change = 2 * cumulative_dia_change) |>
  dplyr::mutate(dia_est = DIA - total_rw_change)

                

toy_size_dat_lag <- wbp_rw_bc %>%
  mutate(RW_in = RW * 0.0393701) %>% 
  dplyr::arrange(TRE_CN, desc(Year)) |>
  dplyr::group_by(TRE_CN) |>
  dplyr::mutate(cum_dia_change = cumsum(lag(RW_in, default = 0))) %>% 
  dplyr::mutate(total_rw_change = 2 * cum_dia_change) |>
  dplyr::mutate(dia_est = DIA - total_rw_change)
