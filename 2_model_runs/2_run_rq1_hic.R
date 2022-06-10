name <- "rq1_hic"

fit1 <- "UKHSA_v3_20220301_AZPD2=FALSE_SB=FALSE_NewDecay=TRUE_AddBst=FALSE"
fit <- "UKHSA_v3_20220301_AZPD2=FALSE_SB=FALSE_NewDecay=TRUE_AddBst=FALSE_SD1"

#### Get vaccine parameters  ##############################################
vaccine <- "Pfizer"

vacc_names <- data.frame(vaccine = c("Pfizer", "Oxford-AstraZeneca"), vacc = c("PF", "AZ"))
vaccine_set <- vaccine
vacc_params <- readRDS(paste0("data/param_list_",fit1,".rds")) %>%
  rename(vacc = vaccine) %>%
  left_join(vacc_names, by = "vacc") %>%
  filter(vaccine == vaccine_set) %>%
  mutate(std10 = 0.44) %>%
  filter(vfr > 1) %>%
  select(-c(vacc))

#### Set up other simulation parameters  ##############################################
target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Absent"
dt <- 0.25
repetition <-  1:20
vacc_start <- "1/1/2021"
vaccine_doses <- c(3,6,9)
max_coverage <- 0.9
age_groups_covered <- c(15)#,16)
age_groups_covered_d4 <- c(2, 5, 15)#, 16)
seeding_cases <- 10
vacc_per_week <- 0.05
strategy <- "realistic"
t_d3 <- 227 
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr2_time1 <- "07/1/2022" # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- "07/31/2022"
vfr <- unique(vacc_params$vfr)[1]
vfr2 <- vfr
ICU_scal_vfr <-  0.3
hosp_scal_vfr <- 0.3
ICU_scal_vfr2 <- 0.3
hosp_scal_vfr2 <- 0.3
mu_ab_infection <- 1.2
max_ab <- 5
rt_out_filename <- "rt_out_hic"
lambda_external_filename <- "lambda_external_hic"
omicron_vaccine <- 0
dose_4_fold_increase <- 1.5

#### Create scenarios ##########################################################

scenarios <- expand_grid(fit = fit,
                         income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         vaccine = vaccine,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         age_groups_covered_d4 = age_groups_covered_d4,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         vacc_per_week = vacc_per_week,
                         t_d3 = t_d3,
                         vfr = vfr,
                         vfr2 = vfr2,
                         vfr_time1 = vfr_time1,
                         vfr_time2 = vfr_time2,
                         vfr2_time1 = vfr2_time1,
                         vfr2_time2 = vfr2_time2,
                         mu_ab_infection = mu_ab_infection,
                         max_ab = max_ab,
                         hosp_scal_vfr = hosp_scal_vfr,
                         ICU_scal_vfr = ICU_scal_vfr,
                         hosp_scal_vfr2 = hosp_scal_vfr2,
                         ICU_scal_vfr2 = ICU_scal_vfr2,  
                         rt_out_filename = rt_out_filename,
                         lambda_external_filename = lambda_external_filename,
                         omicron_vaccine = omicron_vaccine,
                         dose_4_fold_increase = dose_4_fold_increase
) %>%
  mutate(age_groups_covered_d3 = age_groups_covered,
         age_groups_covered_d5 = age_groups_covered_d4,
         age_groups_covered_d6 = age_groups_covered_d4,
         age_groups_covered_d7 = age_groups_covered_d4,
         age_groups_covered_d8 = age_groups_covered_d4,
         age_groups_covered_d9 = age_groups_covered_d4 ) %>%
  mutate(t_d4 = if_else(vaccine_doses == 9, 181, 365), 
         t_d5 = if_else(t_d4 == 181, 184, 365), 
         t_d6 = if_else(t_d4 == 181, 181, 366),
         t_d7 = if_else(t_d4 == 181, 184, 365),
         t_d8 = if_else(t_d4 == 181, 182, 365),
         t_d9 = if_else(t_d4 == 181, 184, 365) ) %>%
  filter((vaccine_doses==3 & age_groups_covered_d4 == 2)| (vaccine_doses == 6) | (vaccine_doses ==9 & age_groups_covered_d4 <= 5)) %>%
  filter(age_groups_covered_d4 <= age_groups_covered) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

scenarios <- left_join(scenarios, vacc_params, by = c("vaccine", "vfr"))

nrow(scenarios)

write.csv(scenarios, paste0("scenarios_", name, ".csv"), row.names = FALSE)

## test on PC
source("R/run_function_main.R")
source("R/utils.R")
source("R/vaccine_strategy.R")
# plan(multicore, workers = 4)
# system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_main.R", "R/utils.R", "R/vaccine_strategy.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire", "data.table"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")

# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Run
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)
runs$status()

