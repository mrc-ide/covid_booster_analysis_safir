
R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")

max_Rt <- 4
max_Rt_omicron <- 4
max_Rt_var2 <- 4.4
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

tmax_date <- as.Date(x = "12/31/2023", format = "%m/%d/%Y")
time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))

R_profile <- read_csv("data/category_2_Rt.csv") 
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

saveRDS(rt_out, "data/rt_out_lmic_newvar.rds")