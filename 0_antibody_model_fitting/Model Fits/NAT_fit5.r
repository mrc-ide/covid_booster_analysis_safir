library(rstudioapi)
library(drjacoby)
library(matrixStats)
library(dplyr)
library(ggplot2)


setwd(dirname(getActiveDocumentContext()$path)) 
cores <- 8

# Parameters & log like--------------------------------------------------------------

source("NAT_fit_priors_inits_loglik_new.r")
source("NAT_fit_main_fn_new.r")

### model version

misc=list(fit=TRUE,AZ_PD2=FALSE,SingleBoost=FALSE,NewDecay=TRUE,AdditiveBoost=FALSE)

set.seed(1790917)
run_name=paste0("UKHSA_v3_20220301_AZPD2=",misc$AZ_PD2,"_SB=",misc$SingleBoost,"_NewDecay=",misc$NewDecay,"_AddBst=",misc$AdditiveBoost)
data_file <- "UKHSA_VE_Mar22.csv"
r_fitmodel(run_name, data_file, misc)


