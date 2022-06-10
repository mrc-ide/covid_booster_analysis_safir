# Analysis of impact of COVID-19 booster doses on epidemic dynamics in different global settings, using the individual-based model of SARS-CoV-2 transmission, "safir"
# Authors: AB Hogan, SL Wu, AC Ghani, P Doohan, P Winskill, OJ Watson
# Date: 25 May 2022

library(rstudioapi)
library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)
library(zoo)
library(tibble)
library(dplyr)
library(tidyr)
library(wesanderson)

#setwd(dirname(getActiveDocumentContext()$path)) 

source("R/plotting_utils.R")
source("R/utils.R")
