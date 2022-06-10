library(rstudioapi)
library(tidyverse)
library(patchwork)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(drjacoby)
library(matrixStats)
library(purrr)

setwd(dirname(getActiveDocumentContext()$path)) 

source("R/vx_profile.R")

##################################
##### LOAD THE PARAMETERS 
##################################

load("../Model Fits/UKHSA_v3_20220301_AZPD2=FALSE_SB=FALSE_NewDecay=TRUE_AddBst=FALSE_mcmc_chain.Rdata")   


chain <- mcmc$output %>%
  filter(phase == "sampling") %>%
  subset(select=-c(chain,phase,iteration,logprior,loglikelihood))

## Calculate parameter estimates and bounds of transformed parameters from 10,000 MCMC samples

draws <- sample_chains(mcmc, 10000)

draws_transform <- draws %>%
  select(-sample, -AZ_ns_off ) %>%
  mutate(d2_AZ_log10 = log10(32/59) - fold_red_AZ,
         d2_PF_log10 = log10(223/94) - fold_red_PF,
         d2_MD_log10 = log10(654/158) - fold_red_MD,
         d2_PF = 10^(d2_PF_log10),
         d2_AZ = 10^(d2_AZ_log10),
         d2_MD = 10^(d2_MD_log10),
         d1_AZ = 10^(d2_AZ_log10 + d1_AZ),
         d1_PF = 10^(d2_PF_log10 + d1_PF),
         d1_MD = 10^(d2_MD_log10 + d1_MD),
         d3_AZ = 10^(bst_AZ),
         d3_PF = 10^(bst_PF),
         d3_MD = 10^(bst_MD),
         
         ab50 = 10^(d2_PF_log10 + ni50),
         ab50_s = 10^(d2_PF_log10 + ns50), 
         ab50_d = 10^(d2_PF_log10 + nd50),
         om_red = 10^(om_red),
         fold_red_PF = 10^(fold_red_PF),
         fold_red_AZ = 10^(fold_red_AZ),
         fold_red_MD = 10^(fold_red_MD)) %>%
  select(-ni50, -ns50, -nd50, -d2_AZ_log10, -d2_PF_log10, -d2_MD_log10) 
  
posterior_median_transform <-draws_transform %>%
  summarise( 
    across(where(is.numeric), median)
  )%>%
  mutate(measure = "median")

posterior_upper <- draws_transform %>%
  summarise( 
    across(where(is.numeric), quantile, 0.975)
  )%>%
  mutate(measure = "upper")

posterior_lower <- draws_transform %>%
  summarise( 
    across(where(is.numeric), quantile, 0.025)
  ) %>%
  mutate(measure = "lower")

params_est <- posterior_median_transform %>%
  rbind(posterior_upper) %>%
  rbind(posterior_lower) %>%
  pivot_longer(cols = c(d1_AZ, d1_PF, d1_MD,d2_PF, d2_AZ,d2_MD, d3_AZ, d3_PF, d3_MD, 
                        fold_red_AZ, fold_red_PF, fold_red_MD,
                        bst_AZ, bst_PF, bst_MD,
                        om_red, ab50, ab50_s, ab50_d, k, hl_s, hl_l, period_s, period_l)) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(median = round(median,digits=3), lower = round(lower,digits=3), upper = round(upper,digits=3)) %>%
  flextable()


if(data=="Imperial") {
save_as_docx("Posterior Parameter Estimates" = params_est,
             path = "../Tables/TableS1_imp_model5a.docx")
}
if(data=="PHE") {
save_as_docx("Posterior Parameter Estimates" = params_est,
               path = "../Tables/TableS1_ukhsa.docx")
}

## Calculate Table estimates from full chain and bounds from 2,000 MCMC samples


posterior_median <- chain %>%
  summarise( 
    across(where(is.numeric), median)
  ) %>%
  mutate(sample=0)
draws <- sample_chains(mcmc,1000)

df_raw <- bind_rows(posterior_median, draws) %>%
          mutate(AZ_ns_off = 0)     # turn off the AZ_ns_off parameter 

df_AZ_AZ <- df_raw %>%
  mutate(vaccine = "AZ_AZ",
         d2 = log10(32/59),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(32/59) - fold_red_AZ,
         d3 = bst_AZ,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50, 
         ab50_d = log10_d2_PF + nd50) %>% 
  select(-c(d1_PF, d1_MD,fold_red_PF,fold_red_MD,bst_PF,bst_AZ, bst_MD,
            AZ_ns_off,log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l)) %>%
  rename(d1=d1_AZ, fold_red=fold_red_AZ) 

df_AZ_PF <- df_raw %>%
  mutate(vaccine = "AZ_PF",
         d2 = log10(32/59),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(32/59) - fold_red_AZ,
         d3 = bst_PF,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50, 
         ab50_d = log10_d2_PF + nd50) %>% 
  select(-c(d1_PF, d1_MD,fold_red_PF,fold_red_MD,bst_PF,bst_AZ, bst_MD,
            AZ_ns_off,log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l)) %>%
  rename(d1=d1_AZ, fold_red=fold_red_AZ) 

df_AZ_MD <- df_raw %>%
  mutate(vaccine = "AZ_MD",
         d2 = log10(32/59),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(32/59) - fold_red_AZ,
         d3 = bst_MD,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50, 
         ab50_d = log10_d2_PF + nd50) %>% 
  select(-c(d1_PF, d1_MD,fold_red_PF,fold_red_MD,bst_PF,bst_AZ, bst_MD,
            AZ_ns_off,log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l)) %>%
  rename(d1=d1_AZ, fold_red=fold_red_AZ) 

df_PF_PF <- df_raw %>%
  mutate(vaccine = "PF_PF",
         d2 = log10(223/94),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(223/94) - fold_red_PF,
         d3 = bst_PF,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50,
         ab50_d = log10_d2_PF + nd50) %>%
    select(-c(d1_AZ, d1_MD, fold_red_AZ, fold_red_MD, bst_PF, bst_AZ, bst_MD,AZ_ns_off,
              log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l )) %>%
  rename(d1 = d1_PF, fold_red = fold_red_PF) 

df_PF_MD <- df_raw %>%
  mutate(vaccine = "PF_MD",
         d2 = log10(223/94),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(223/94) - fold_red_PF,
         d3 =  bst_MD,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50,
         ab50_d = log10_d2_PF + nd50) %>%
  select(-c(d1_AZ, d1_MD, fold_red_AZ, fold_red_MD, bst_PF, bst_AZ, bst_MD,AZ_ns_off,
            log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l )) %>%
  rename(d1 = d1_PF, fold_red = fold_red_PF) 

df_MD_PF <- df_raw %>%
  mutate(vaccine = "MD_PF",
         d2 = log10(654/158),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(654/158) - fold_red_MD,
         d3 =  bst_PF,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50,
         ab50_d = log10_d2_PF + nd50) %>%
  select(-c(d1_AZ, d1_PF, fold_red_AZ, fold_red_PF, bst_PF, bst_AZ, bst_MD,AZ_ns_off,
            log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l )) %>%
  rename(d1 = d1_MD, fold_red = fold_red_MD) 

df_MD_MD <- df_raw %>%
  mutate(vaccine = "MD_MD",
         d2 = log10(654/158),
         log10_d2_PF = log10(223/94) - fold_red_PF,
         log10_d2 = log10(654/158) - fold_red_MD,
         d3 =  bst_MD,
         ab50 = log10_d2_PF + ni50,
         ab50_s = log10_d2_PF + ns50,
         ab50_d = log10_d2_PF + nd50) %>%
  select(-c(d1_AZ, d1_PF, fold_red_AZ, fold_red_PF, bst_PF, bst_AZ, bst_MD,AZ_ns_off,
            log10_d2_PF,log10_d2,ni50,ns50,nd50, period_l )) %>%
  rename(d1 = d1_MD, fold_red = fold_red_MD) 

df <- bind_rows(df_AZ_AZ, df_AZ_PF, df_AZ_MD, df_PF_PF, df_PF_MD, df_MD_PF, df_MD_MD) 


r1 <- 
  # Create input options
  expand_grid(
  vfr = c(0,1),
  vaccine = c("AZ_AZ", "AZ_PF", "AZ_MD", "PF_PF", "PF_MD", "MD_PF", "MD_MD"),
  sample = unique(df$sample)) %>%
  # Join with MCMC samples
  left_join(df, by = c("vaccine", "sample")) %>%
  # Apply vaccine profile function to each row
  mutate(profile = pmap(., vx_profile)) %>%
  # Format
  unnest(cols = c(profile)) %>%
  pivot_longer(cols = c(Titre_d1, Titre_d2, Titre_d3, Efficacy_d1, Efficacy_d2, Efficacy_d3, Severe_Efficacy_d1,Severe_Efficacy_d2, Severe_Efficacy_d3, Death_Efficacy_d1, Death_Efficacy_d2,Death_Efficacy_d3), names_to = "group", values_to = "Value") %>% 
  mutate(dose = case_when(group == "Titre_d1" | group == "Efficacy_d1" | group == "Severe_Efficacy_d1" | group == "Death_Efficacy_d1"~ 1,
                          group == "Titre_d2" | group == "Efficacy_d2" | group == "Severe_Efficacy_d2" | group == "Death_Efficacy_d2" ~ 2,
                          group == "Titre_d3" | group == "Efficacy_d3" | group == "Severe_Efficacy_d3" | group == "Death_Efficacy_d3"~ 3 )) %>%
  mutate(group = substr(group, 1, nchar(group) - 3))



# saveRDS(r1, "mcmc_samples.rds")

################################################################################
### TABLE 1 ####################################################################
################################################################################

# r1 <- readRDS("mcmc_samples.rds")


r1subset <- r1 %>%
      filter(t==30 & vaccine=="PF_PF" & dose==3 & group=="Efficacy" & vfr==0)

r1test <- r1subset %>%
   summarise(upper = quantile(Value, 0.025),
             lower = quantile(Value, 0.975))

# posterior median
r1a <- r1 %>%
  filter(sample==0) %>%
  mutate(median = Value)

  # cross-check code
  #look <- r1a %>%
  #filter(t==0 & dose==3)

# mcmc samples
r1b <- r1 %>%
  filter(sample>0) %>%
  group_by(vfr,vaccine,group,dose,t) %>%
  summarise(upper = quantile(Value, 0.975),
            lower = quantile(Value, 0.025),
  ) 

summary_stats <- left_join(r1a,r1b)


table1 <- summary_stats %>%
  filter(group=="Efficacy"| group=="Severe_Efficacy" | group=="Death_Efficacy") %>%
  mutate(median = round(median,digits=1), lower = round(lower,digits=1), upper = round(upper,digits=1)) %>%
  mutate(vaccine_efficacy = paste0(median, " (",lower,"-",upper,")")) 

tb_dose_2 <- table1 %>%
  filter(dose==2) %>%
  filter(t==1+90 | t==1+180 ) %>%
  mutate(period = case_when(t==91 ~ "90d pd2",
                            t==181 ~ "180d pd2") )

tb_dose_3 <- table1 %>%
  filter(dose==3) %>% 
  filter(t==1+30 | t==1+60  | t==1+90 | t==1+120 | t==1+150 | t==1+180 | t==1+365) %>%
  mutate(period = case_when(t==31 ~ "30d pb",
                            t==61 ~ "60d pb",
                            t==91 ~ "90d pb",
                            t==121 ~ "120d pb",
                            t==151 ~ "150d pb",
                            t==181 ~ "180d pb",
                            t==366 ~ "365d pb") )

tb <- rbind(tb_dose_2,tb_dose_3)
tb <- subset(tb,select=c(vfr,vaccine,group,vaccine_efficacy,period))


tb.inf <- tb %>%
  filter(group == "Efficacy") %>%
  pivot_wider(id_cols = c(vaccine, vfr), names_from = period, values_from = vaccine_efficacy) %>%
  arrange(vaccine, vfr) %>%
  rename(Vaccine = vaccine, "0 = delta, 1 = omicron" = vfr) %>% 
  flextable()

tb.sev <- tb %>%
  filter(group == "Severe_Efficacy") %>%
  pivot_wider(id_cols = c(vaccine, vfr), names_from = period, values_from = vaccine_efficacy) %>%
  arrange(vaccine, vfr) %>%
  rename(Vaccine = vaccine, "0 = delta, 1 = omicron" = vfr) %>% 
  flextable()

tb.death <- tb %>%
  filter(group == "Death_Efficacy") %>%
  pivot_wider(id_cols = c(vaccine, vfr), names_from = period, values_from = vaccine_efficacy) %>%
  arrange(vaccine, vfr) %>%
  rename(Vaccine = vaccine, "0 = delta, 1 = omicron" = vfr) %>% 
  flextable()



save_as_docx("Efficacy against infection" = tb.inf,
               "Efficacy against severe disease" = tb.sev,
               "Efficacy against death" = tb.death,
               path = "../Tables/TableS3.docx")
  







