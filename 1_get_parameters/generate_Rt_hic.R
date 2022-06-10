
generate_Rt_hic <- function(name){
  # read in R profile
  R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
  
  max_Rt <- 3
  max_Rt_omicron <- 3
  
  if (name == "rq1_hic_newvariant") {
    max_Rt_var2 <- max_Rt_omicron * 1.1
  } else {
    max_Rt_var2 <- max_Rt_omicron
  }
  
  mult <- max_Rt_omicron/max_Rt
  mult_v2 <- max_Rt_var2 / max_Rt
  
  vfr_time1 <- "11/27/2021"
  vfr_time2 <- "12/31/2021"
  vfr2_time1 <- "7/1/2022"
  vfr2_time2 <- "7/31/2022"
  vfr_time1 <- as.Date(x = vfr_time1, format = "%m/%d/%Y")
  vfr_time2 <- as.Date(x = vfr_time2, format = "%m/%d/%Y")
  vfr2_time1 <- as.Date(x = vfr2_time1, format = "%m/%d/%Y")
  vfr2_time2 <- as.Date(x = vfr2_time2, format = "%m/%d/%Y")
  
  tmax_date <- as.Date(x = "12/31/2024", format = "%m/%d/%Y")
  time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
  
  R_profile <- read_csv("data/category_1_Rt.csv") 
  R_profile$date <- as.Date(R_profile$date, format = "%d/%m/%Y")
  
  dates <- R_profile$date
  rt <- R_profile$Rt
  rt_base_out <- safir::interpolate_rt(dates=dates, rt=rt, max_date=tmax_date)
  
  omicron_dates <- c(R0_t0, vfr_time1, vfr_time2)
  rt_omicron <- c(1,1,mult)
  rt_omicron_mult <- safir::interpolate_rt(dates=omicron_dates, rt=rt_omicron, max_date=tmax_date)
  
  var2_dates <- c(R0_t0, vfr2_time1, vfr2_time2)
  rt_var2 <- c(1,1,mult_v2)
  rt_var2_mult <- safir::interpolate_rt(dates=var2_dates, rt=rt_var2, max_date=tmax_date)
  
  rt_out <- data.frame(Rt = rt_base_out$Rt * rt_omicron_mult$Rt * rt_var2_mult$Rt, Rt_tt = rt_base_out$Rt_tt)
  
  if (name == "rq1_hic_newvariant") {
    saveRDS(rt_out, "data/rt_out_hic_newvariant.rds")
  } else {
    saveRDS(rt_out, "data/rt_out_hic.rds")
  }
  # these time points are used further down to define the start of the simulation and the small pulses that reduce stochasticity
  R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
  R0_t2 <- as.Date(x = "10/1/2020", format = "%m/%d/%Y")
  R0_t3 <- as.Date(x = "9/1/2021", format = "%m/%d/%Y")
  R0_t4 <- as.Date(x = "7/1/2022", format = "%m/%d/%Y")
  
  # daily per-capita prob of external infection
  lambda_external <- rep(0.0000001, time_period)
  
  # first pulse, spread out hazard of 0.001 over 10 days right before 1st wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t1, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  # second pulse, spread out hazard of 0.001 over 10 days right before 2nd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t2, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  #  third pulse, spread out hazard of 0.001 over 20 days right before 3rd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  saveRDS(lambda_external, "data/lambda_external_hic.rds")
}


