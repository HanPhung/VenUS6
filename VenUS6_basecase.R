if(!require ("R2WinBUGS")) install.packages("R2WinBUGS")

rm(list=ls())
library(R2WinBUGS)
library(coda)

#### [finale] Log normal FE model network2 -  with covariates ####
getwd()

#### data ###

# IPD
v1 <- read.csv("V1_ipd.CSV", header = T)

v6 <- read.csv("V6_Jan25.CSV", header = T)

treat11   =c(1,2,  1,8,11) # RE
baseline11=c(1,1,  1,1,1) # RE

n.arms1 <- 2 #v1

n.arms3 <- 3 #v6

n.subjects1 <- nrow(v1)

n.subjects3 <- nrow(v6)
zu.ipd1 <-  4.774911  # days
shape.ipd1 <- 1.358708 

zu.ipd3 <- 4.916613   #day
shape.ipd3 <- 1.03985

C1 <- max(v1$centre1)
C3 <- max(v6$centre3)

# AD

ad <- read.csv("V6_AD_network2.csv", header = T)

a.s <- ad$a.s
a.treat <- ad$a.treat
r <- ad$r
n <- ad$n
a.base <- ad$a.base
a.time <- ad$a.time

n.agg.arm = length(a.s)
n.agg.trials = max(a.s)

max.treat <- max(c(a.treat, treat11))


# format data list

data <- list(id1=v1$id1, study1=v1$study1,treat1=v1$treat1, baseline1=v1$baseline1,
             centre1=v1$centre1,
             logarea1 = v1$logarea1, logdurm1=v1$logdurm1, 
             # age1=v1$age1, 
             index1=v1$index1, t.obs1=v1$t.obs1/4.333, t.cen1=v1$t.cen1/4.333,
             
             id3=v6$id3, study3=v6$study3,treat3=v6$treat3, baseline3=v6$baseline3,
             centre3=v6$centre3,
             logarea3 = v6$logarea3, logdurm3=v6$logdurm3, 
             # age3=v6$age3, 
             index3=v6$index3, t.obs3=v6$t.obs3/4.333, t.cen3=v6$t.cen3/4.333,
             
             
             C1=C1,  C3=C3, 
             
             n.subjects1=n.subjects1,  n.subjects3=n.subjects3,
             a.s=a.s, a.treat=a.treat, r=r, n=n, a.base=a.base, a.time=a.time/4.333,
             n.agg.arm=n.agg.arm, n.agg.trials=n.agg.trials, max.treat=max.treat,
             zu.ipd1=zu.ipd1, shape.ipd1=shape.ipd1, zu.ipd3=zu.ipd3, shape.ipd3=shape.ipd3)

### WinBUGS model ###


# V6_RE_WinBUGS_model.txt"

### Inits



betac1 <- rep(0, C1)
betac3 <- rep(0, C3)

inits1 <- list( 
  d = c(NA,rep(2, max.treat-1)), 
  mu1 = 1,  mu3 = 1,
  mu.a = rep(1, n.agg.trials),
  beta_logarea = 0, beta_logdurm = 0, #beta_age =0,
  shape = 0.5 ,
  betac.new = 0, 
  betac1 = betac1, betac3 = betac3 
  
)

inits2 <- list( 
  d = c(NA,rep(1, max.treat-1)), 
  mu1 = 2,  mu3 = 2, 
  mu.a = rep(2, n.agg.trials),
  beta_logarea = 0, beta_logdurm = 0,  #beta_age =0,
  shape = 0.5 ,
  betac.new = 0, 
  betac1 = betac1,  betac3 = betac3
  
)

inits <- list(inits1, inits2)

### parameters to save
parameters <- c("hr", "shape",  "SUCRA", "totresdev", "d",
                "beta_logarea", "beta_logdurm", "betac.new" ,"mu3" #, "beta_age" 
)

# parameters <- c("lhr", "totresdev", "shape", "beta_logarea", "beta_logdurm","beta_mob2", "beta_mob3")


### run model ###

v6.sim <- bugs(data = data, inits = inits, model.file = "V6_FE_LogNormal_basecase.txt", 
               parameters.to.save=parameters,
               n.chains = 2, n.burnin = 5000, n.iter = 10000, n.thin = 1,
               DIC = T,debug=TRUE,
               bugs.directory = "C:/Users/tp994/Downloads/WinBUGS14",
               working.directory = "C:/Users/tp994/Downloads/NMA")
