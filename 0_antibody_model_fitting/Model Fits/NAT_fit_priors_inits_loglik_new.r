
# Parameters --------------------------------------------------------------

# define parameters dataframe
df_params <- define_params(name = "d1_AZ",       min =-3,        max = 0, init=-log10(2)+runif(cores,-0.2,0.2), block=1,
                           name = "d1_PF",       min =-3,        max = 0, init=-log10(2)+runif(cores,-0.2,0.2), block=1,
                           name = "d1_MD",       min =-3,        max = 0, init=-log10(2)+runif(cores,-0.2,0.2), block=1,
                           name = "fold_red_AZ", min = 0,        max = 2, init= 0.5911+runif(cores,-0.1,0.1), block=1,
                           name = "fold_red_PF", min = 0,        max = 2, init= 0.5911+runif(cores,-0.1,0.1), block=1,
                           name = "fold_red_MD", min = 0,        max = 2, init= 0.5911+runif(cores,-0.1,0.1), block=1,
                           name = "om_red",      min = 0,        max = 2, init= 1.0+runif(cores,-0.2,0.2), block=1,
                           name = "bst_AZ",      min = -2,       max = 3, init= log10(6)+runif(cores,-0.2,0.2), block=1,
                           name = "bst_PF",      min = -2,       max = 3, init= log10(6)+runif(cores,-0.2,0.2), block=1,
                           name = "bst_MD",      min = -2,       max = 3, init= log10(6)+runif(cores,-0.2,0.2), block=1,
                           name = "ni50",        min =-5,        max = 0, init=-0.479+runif(cores,-0.2,0.2), block=1,
                           name = "ns50",        min =-5,        max = 0, init=-1.307+runif(cores,-0.2,0.2), block=1,
                           name = "nd50",        min =-5,        max = 0, init=-1.307+runif(cores,-0.2,0.2), block=1,
                           name = "AZ_ns_off",   min = -2,       max = 2, init=runif(cores,-0.2,0.2), block =1,
                           name = "k",           min = 0.5,      max = 10, init= 2.94+runif(cores,-0.05,0.05), block=1,
                           name = "hl_s",        min = 1,        max = 150, init=90+runif(cores,10,10), block=1,
                           name = "hl_l",        min = 150,      max = 3650, init=500+runif(cores,-50,50), block=1,
                           name = "period_s",    min = 1,        max = 150, init=90+runif(cores,-10,10), block=1,
                           name = "period_l",    min = 150,      max = 1000, init=365+runif(cores,-50,50), block=1)

print(df_params)
#>    name min max
#> 1    mu -10  10
#> 2 sigma   0 Inf


# Priors ------------------------------------------------------------------

# define log-prior function
r_logprior <- function(params, misc) {
  
  params <- as.list(params)
  # calculate log-prior
  ret <- dnorm(params$d1_AZ,        mean =-log10(2),  sd = log10(4), log = TRUE)+
    dnorm(params$d1_PF,        mean =-log10(2),  sd = log10(4), log = TRUE)+
    dnorm(params$d1_MD,        mean =-log10(2),  sd = log10(4), log = TRUE)+
    dnorm(params$fold_red_AZ,  mean = 0.5911,  sd = log10(1.5),log = TRUE)+
    dnorm(params$fold_red_PF,  mean = 0.5911,  sd = log10(1.5),log = TRUE)+
    dnorm(params$fold_red_MD,  mean = 0.5911,  sd = log10(1.5),log = TRUE)+
    dnorm(params$om_red,       mean = 1.0,       sd =log10(4),log = TRUE)+    
    dnorm(params$bst_AZ,       mean = log10(6),     sd = log10(4),log = TRUE)+
    dnorm(params$bst_PF,       mean = log10(6),     sd = log10(4),log = TRUE)+
    dnorm(params$bst_MD,       mean = log10(6),     sd = log10(4),log = TRUE)+
    dnorm(params$ni50,         mean =-0.479,  sd = log10(2),log = TRUE)+
    dnorm(params$ns50,         mean =-1.307,  sd = log10(2),log = TRUE)+
    dnorm(params$nd50,         mean =-1.307,  sd = log10(2),log = TRUE)+
    dnorm(params$AZ_ns_off,    mean =0,      sd = log10(2),log = TRUE)+
    dnorm(params$k,            mean = 2.94,    sd = 1.0, log = TRUE)+
#   dnorm(params$hl_s,         mean = 90,     sd = 20.0,log = TRUE)+
    dnorm(params$hl_s,         mean = 58,     sd = 5.0,log = TRUE)+
    dnorm(params$hl_l,         mean = 500,     sd = 100, log = TRUE)+
    dnorm(params$period_s,     mean = 90,      sd = 20.0,log = TRUE)+
    dnorm(params$period_l,     mean = 365,     sd = 80, log = TRUE)
  
  # return
  return(ret)
}

# Likelihood --------------------------------------------------------------

# define log-likelihood function
r_loglike <- function(params, data, misc) {
  
  nic50=rep(0,3) # endpoints
  d <- rep(0,15) # peak Ab 1-3 = dose 1, 4-6 = dose 2, 7-15 = dose 4 combs
  # extract parameter values
  params <- as.list(params) ## to allow parameter samples to be sent easily for calculating predicted vals
  
  d[4] <- log10(32/59)- params$fold_red_AZ
  d[5] <- log10(223/94) - params$fold_red_PF
  d[6] <- log10(654/158) - params$fold_red_MD
  d[1] <- d[4] + params$d1_AZ # d1 Ab relative to PD2
  d[2] <- d[5] + params$d1_PF
  d[3] <- d[6] + params$d1_MD
  if(misc$AdditiveBoost) {
    bst <- ifelse(misc$SingleBoost,params$bst_PF,params$bst_AZ)
    d[7] <- d[4] + bst # boost adds to PD2 Ab
    d[8] <- d[5] + bst
    d[9] <- d[6] + bst
    bst <- params$bst_PF
    d[10] <- d[4] + bst
    d[11] <- d[5] + bst
    d[12] <- d[6] + bst
    bst <- ifelse(misc$SingleBoost,params$bst_PF,params$bst_MD)  
    d[13] <- d[4] + bst
    d[14] <- d[5] + bst
    d[15] <- d[6] + bst
  } else {
    bst <- ifelse(misc$SingleBoost,params$bst_PF,params$bst_AZ)
    d[7] <- bst # boost adds to PD2 Ab
    d[8] <- bst
    d[9] <- bst
    bst <- params$bst_PF
    d[10] <- bst
    d[11] <- bst
    d[12] <- bst
    bst <- ifelse(misc$SingleBoost,params$bst_PF,params$bst_MD)  
    d[13] <- bst
    d[14] <- bst
    d[15] <- bst    
  }
  
  om_red      <- params$om_red
  nic50[1]    <- params$ni50 +  d[5]  # all relative to PD2 PF
  nic50[2]    <- params$ns50 +  d[5]
  nic50[3]    <- params$nd50 +  d[5]
  k           <- params$k
  hl_s        <- params$hl_s
  hl_l        <- params$hl_l
  period_s    <- params$period_s
  period_l    <- params$period_l
  
  variant_red = c(0,om_red) # variant specific peak Ab offset
  
  # Khoury model
  max_t     <- max(data$t)+2 # add 2 to ensure at least 1 slot in dr_vec for each decay phase
  t         <- 0:(max_t-1) #vaccinated on day 0
  lg10      <- log(10)
  dr_s      <- -log(2)/hl_s # Corresponding decay rate in days for half life above
  dr_l      <- -log(2)/hl_l
  
  per1      <-  min(max_t,round(period_s)-1)
  per2      <-  min(max_t+1,round(period_l))
  per3      <-  max(1,max_t+3-round(period_l))
  
  if(misc$NewDecay) {
  # simple biphasic decay implemented as sum of decaying exponentials
    denom=log10(exp(dr_l*period_s)+exp(dr_s*period_s))
    cum_dr_vec=log10(exp(dr_s*t+dr_l*period_s)+exp(dr_l*t+dr_s*period_s))-denom
    dr_vec=c(0,diff(cum_dr_vec,1))*lg10
  } else {
  # original Khoury model
    dr_vec    <- c(0,#no decay on day 0
                   rep(dr_s,per1),
                   seq(dr_s,dr_l,length.out=per2-per1),
                   rep(dr_l,per3))
    cum_dr_vec=cumsum(dr_vec)/lg10 #Ab decay profile
 }

  # now add peak Ab offset to profile, transform to atomic vector
  Ab <- as.numeric(d[data$vacc_lookup])+cum_dr_vec[data$t]
  
  # vector of NIC50s for each endpoint
  nic <- as.numeric(nic50[data$endpoint])
  
  # treat AZ PD2 severe differently if enabled
  if(misc$AZ_PD2) {
    sel <- data$vaccine_num==1 & data$dose==2 & data$endpoint>1
    nic[sel]=nic[sel]+params$AZ_ns_off
  }
  
  # predicted VE
  pred<- 1/(1+exp(-k*(Ab-variant_red[data$variant]-nic)))
  
  # calculate log-probability of data
  if(misc$fit) { ## return log lik when fitting
 #   ret <- sum(dbinom(round(data$N*data$RR), data$N, 1-pred, log = TRUE)) # dbinom vers
    ret <- sum(data$N*(data$RR*log(1-pred)+(1-data$RR)*log(pred))) # faster and equiv. Also avoids rounding data
  } else { ## otherwise predicted values
    ret <- list(pred,dr_vec[data$t])
  }
  
  # return
  return(ret)
}
