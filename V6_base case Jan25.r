##################################################
## PEDRO & MARTA  21 September 2013 #####
#### ADOPTED FOR V6 by HAN PHUNG OCT 2024###
############################################
### CE DECISION MODEL FOR VLU TREATMENTS ###
###           BASE CASE                  ###
############################################

rm(list=ls(all=TRUE))

set.seed(5)
library(survival)
library(Matrix)
library(stats)
# install.packages("tidyverse")
library(readxl)

setwd("C:/Users/tp994/Downloads/R")
#P=Treatment
#1=4LB 
#2=SSB
#3=HH
#4=Paste ### keep it here but won't be reported
#5=2LB
#6=CW
#7=EBC

#S=Health state
#1=Healed
#2=Unhealed
#3=Death

#N=Number of patients
#C=Cycle (months)

#pi_[C, S, P]
#mtrace[C, S(t), S(t-1), P]

#----------------------------------------------------
##### DATA 1: constants and vectors definition ######
#----------------------------------------------------

#Simulations
Nsim<- 5000

## PARAMETER TYPE: OTHER

# Size of the cohort simulation going through the model
N <- 1000
# Number of prevention strategies being evaluated
P <- 7
# Number of cycles
C <- 12*29            # 120 months = 10 years
# Number of health states in the model
S <- 15
# Number of ceiling ratio values (from 0 to ?200,000)
K <- 101
# Threshold for NMB calculation
threshold <- 20000

# Discount rate for utilities
uDR <- 0.035
# Discount rate for costs
cDR <- 0.035

age <- 70.25 #V6 Jan 25
male <- 0.551 #new parameter, for utility estimates

## for FE
logarea <- 2.052119  #V6 Jan 25
logdurm <- 1.469  #V6 Jan 25
durm<- 13.46777 #V6 Jan 25


##
## PARAMETER TYPE: PROBABILITIES
##

#Recurrence 
#STATA mean parameter estimates from a Log-normal survival distribution, without regressors
## keep the variable name gompertz for ease 
gompertz_results<- read.table('C:/Users/tp994/Downloads/R/recur_lognormal.txt', header=T, sep="\t", quote="\"", 
                            dec=".", fill=T, na.strings=c(""))

#STATA cholesky decomposition matrix
gompertz_chol<- read.table('C:/Users/tp994/Downloads/R/Chol_matrix.txt', header=T, sep="\t", quote="\"", 
                            dec=".", fill=T, na.strings=c(""))

randnorm = array(NA, dim=c(Nsim,2))
Tz_gomp = array(NA, dim=c(Nsim,2)) 
lv_gomp_ct = array(NA, dim=c(Nsim))
lv_gomp_gamma = array(NA, dim=c(Nsim))

for(i in 1:Nsim) {
    randnorm[i,] <- qnorm(runif(2),0,1)
    Tz_gomp[i,] <- t(gompertz_chol) %*% randnorm[i,]
    lv_gomp_ct[i] <- as.numeric(Tz_gomp[i,1] + gompertz_results[1,2])
    lv_gomp_gamma[i] <- as.numeric(Tz_gomp[i,2] + gompertz_results[1,3])
}

##
## PARAMETER TYPE: RESOURCE COST
##


## below whout SEis indeed alpha, and se_ is beta for Log normal distribution
#Treatment cost per month once unhealed
t_cost <- read_excel('C:/Users/tp994/Downloads/R/Resource_use.xlsx', sheet="Treatment_cost")
t_dur <- as.data.frame(c(t_cost[2,4], t_cost[6,4],  t_cost[3,4], 1, t_cost[5,4] , t_cost[1,4] , t_cost[4,4] )) ## TBC for SSB 
se_t_dur <- as.data.frame(c(t_cost[2,5], t_cost[6,5],  t_cost[3,5], 1, t_cost[5,5] , t_cost[1,5] , t_cost[4,5]) ) ## TBC for SSB

#Nurse visits #mean (se) per month once unhealed
nurse <- read_excel('C:/Users/tp994/Downloads/R/Resource_use.xlsx', sheet="Nurse_visit")
nursev <-as.data.frame( c(nurse[2,4], nurse[2,4],  nurse[3,4], nurse[2,4], nurse[5,4] , nurse[1,4] , nurse[4,4]) )# TBC for SSB
se_nursev <- as.data.frame(c(nurse[2,5], nurse[2,5],  nurse[3,5], nurse[2,4], nurse[5,5] , nurse[1,5] , nurse[4,5] ) ) # TBC for SSB

#Duration of nurse visit
duration <- read_excel('C:/Users/tp994/Downloads/R/Resource_use.xlsx', sheet="Nurse_duration")
nurse_dur <- as.data.frame(c(duration[2,4], duration[6,4],  duration[3,4], duration[2,4], duration[5,4] , duration[1,4], duration[4,4] ))
se_nurse_dur <- as.data.frame(c(duration[2,5], duration[6,5],  duration[3,5], duration[2,4], duration[5,5] , duration[1,5] , duration[4,5] ))

#Unit cost of Band of nurse
band <- read_excel('C:/Users/tp994/Downloads/R/Resource_use.xlsx', sheet="Nurse_band")
c_nurse <- as.data.frame(c(band[2,3], band[6,3],  band[3,3], band[2,3], band[5,3] , band[1,3] , band[4,3]))# TBC for SSB

#Doctor visits #mean (se) number of doctor visits per month per patient
#GP <- read_excel('C:/Users/tp994/Downloads/R/Resource_use.xlsx', sheet="GP")

# Number of GP contact
#GP_home <- as.data.frame(GP[1,2])
#GP_home[1, 1] <- as.numeric(GP_home[1, 1])
#GP_clinic <- data.frame(GP[2,2])
#GP_clinic[1, 1] <- as.numeric(GP_clinic[1, 1])
#Outpts <- as.data.frame(GP[3,2])
#Outpts[1, 1] <- as.numeric(Outpts[1, 1])

#se_GP_home <- as.data.frame(GP[1,3])
#se_GP_home[1, 1] <- as.numeric(se_GP_home[1, 1])
#se_GP_clinic <- as.data.frame(GP[2,3])
#se_GP_clinic[1, 1] <- as.numeric(se_GP_clinic[1, 1])
#se_Outpts <- as.data.frame(GP[3,3])
#se_Outpts[1, 1] <- as.numeric(se_Outpts[1, 1])

#cost of GP contact
#c_GP_home <- as.data.frame(GP[1,4])
#c_GP_home[1, 1] <- as.numeric(c_GP_home[1, 1])
#c_GP_clinic <- as.data.frame(GP[2,4])
#c_GP_clinic[1, 1] <- as.numeric(c_GP_clinic[1, 1])
#c_Outpts <- as.data.frame(GP[3,4])
#c_Outpts[1, 1] <- as.numeric(c_Outpts[1, 1])

GP_home <- log(0.37^2/sqrt(0.54^2+0.37^2))          
se_GP_home <- sqrt(log(1+(0.54^2/0.37^2)))
GP_clinic <-  log(0.03^2/sqrt(0.02^2+0.03^2) )               
se_GP_clinic <- sqrt(log(1+(0.03^2/0.02^2)))
Outpts <- log(0.25^2/sqrt(0.37^2+0.25^2))
se_Outpts <- sqrt(log(1+(0.37^2/0.25^2)))

c_GP_home <- 95.2
c_GP_clinic <- 42
c_Outpts <- 170.27

#Hospital visits (daycare) #mean (se) number of hospital visits per month per patient
hospv <- log(0.04^2/sqrt(0.03^2+0.04^2))  # source V6
se_hospv <- sqrt(log(1+(0.03^2/0.04^2)))

#cost of daycare
c_hospv <- 460 # source V6  460 * 0.12 
#460 hospital admission without overnight stay
#0.12 average monthly hospital contacts in V6

#Hospital stay #mean (se) number of hospital visits per month per patient
hospstay <- log(0.01^2/sqrt(0.04^2+0.01^2))  # source V6
se_hospstay <- sqrt(log(1+(0.04^2/0.01^2)))

c_hospstay <- 601

##
## PARAMETER TYPE: UTILITY PARAMATERS PER CYCLE
##
# V6 utility decrement, GEE model (inverse gaussian, identity)
u_unheal <- 0.0869696  
u_se_unheal <- 0.0162706
u_unheala <- u_unheal * (u_unheal * (1 - u_unheal) / (u_se_unheal^2) - 1)
u_unhealb <- u_unheal * (1 - u_unheal) / (u_se_unheal^2) - 1 - u_unheala

#---------------------------------------------------------------
##### DATA 2: Mortality and Standard Population Utilities ######
#---------------------------------------------------------------

mortutil_data<-read.table('C:/Users/tp994/Downloads/R/V6_mortality.txt',header=T,sep="\t",quote="\"",dec=".",fill=T,na.strings=c(""),as.is=1:3)
## from VenUS 6
mort_ratio <- 1.79

#-------------------------------------------------------
##### DATA 4: Probability treatment is effective ######
#-------------------------------------------------------

#MTC_coda<-read.table('CHE_2012\\Projects\\HSciences\\VenUS4\\VenUS4_SM\\Report\\Phase2\\IncV4\\MTC_ADIPD9\\CODA_MTC.txt', header=F, sep="\t", quote="\"", dec=".", fill=T,na.strings=c(""),as.is=1:1)
MTC_coda<-read.table('C:/Users/tp994/Downloads/R/coda1.txt', header=F)


sims <- seq(1,Nsim,1)
ProbMatrix<- matrix(data=NA, nrow=5000, ncol=11, dimnames=list(sims, c("beta_logarea", "beta_logdurm", "betac.new", "hr2",
                    "hr3", "hr4", "hr5", "hr6", "hr7", "mu", "shape")))




### for FE
for (i in 1:5000) {
  ProbMatrix[i,1] <- MTC_coda[i + 55000, 2]
  ProbMatrix[i,2] <- MTC_coda[i + 60000, 2]

  ProbMatrix[i,3] <- MTC_coda[i + 65000, 2]
  ProbMatrix[i,4] <- MTC_coda[i + 125000, 2] ## SS
  ProbMatrix[i,5] <- 1  ## HH
  ProbMatrix[i,6] <- MTC_coda[i + 135000, 2] ### paste
  ProbMatrix[i,7] <- MTC_coda[i + 155000, 2] ## 2LB
  ProbMatrix[i,8] <- MTC_coda[i + 170000, 2]
  ProbMatrix[i,9] <- 1
  ProbMatrix[i,10] <- MTC_coda[i + 400000, 2]
  ProbMatrix[i,11] <- MTC_coda[i + 405000, 2]
}

ProbMatrix <- cbind(ProbMatrix, "hr1"=rep(1,5000))

## for Log normal, log logistic ###
ProbMatrix <- cbind(ProbMatrix, "lambda_heal"= ProbMatrix[,"mu"] + ProbMatrix[,"betac.new"] + ProbMatrix[,"beta_logarea"] * logarea +  ProbMatrix[,"beta_logdurm"] * logdurm )
ProbMatrix<- cbind(ProbMatrix,  lv_gomp_ct, lv_gomp_gamma)
ProbMatrix <- cbind(ProbMatrix, "lambda_recur"= ProbMatrix[,"lv_gomp_ct"]  )

#-------------------------------------------------------
#-------------------------------------------------------
################### DECISION MODEL FOR VLU #############
#-------------------------------------------------------
#-------------------------------------------------------

Rc = array(NA, dim=c(K), dimnames=list(seq(1,K,1)))
u_pop = array(NA, dim=c(Nsim,C), dimnames=list(seq(1,Nsim,1),seq(1,C,1))) 
p_heal = array(NA, dim=c(P,C), dimnames=list(seq(1,P,1),seq(1,C,1))) 
p_recur = array(NA, dim=c(P,C+12), dimnames=list(seq(1,P,1),seq(1,C+12,1)))
mtrace = array(0, dim=c(C,S,S,P), dimnames=list(seq(1,C,1),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10","heal11","heal12",
                                              "heal13","unheal","death"),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10",
                                              "heal11","heal12","heal13","unheal","death"),seq(1,P,1))) 
TOTAL = array(NA, dim=c(C,S,P), dimnames=list(seq(1,C,1),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10","heal11","heal12",
                                              "heal13","unheal","death"),seq(1,P,1)))
pi_ = array(0, dim=c(C,S,P), dimnames=list(seq(1,C,1),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10","heal11","heal12",
                                              "heal13","unheal","death"),seq(1,P,1)))
CHECK = array(NA, dim=c(C,P), dimnames=list(seq(1,C,1),seq(1,P,1)))
u = array(0, dim=c(C,S,P), dimnames=list(seq(1,C,1),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10","heal11","heal12",
                                              "heal13","unheal","death"),seq(1,P,1)))
ct_ = array(NA, dim=c(C,P), dimnames=list(seq(1,C,1),seq(1,P,1)))
ut_ = array(NA, dim=c(C,P), dimnames=list(seq(1,C,1),seq(1,P,1)))
TotC = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
mean_C = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
TotU = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
mean_U = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))


inc_C = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
inc_U = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
NMB = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))
INMB = array(NA, dim=c(P), dimnames=list(seq(1,P,1)))

cost = array(NA, dim=c(C,S,P), dimnames=list(seq(1,C,1),c("heal1","heal2","heal3","heal4","heal5","heal6","heal7","heal8","heal9","heal10","heal11","heal12",
                                              "heal13","unheal","death"),seq(1,P,1)))


ProbMatrix <- cbind(ProbMatrix, "u_decunh"=  rbeta(Nsim, u_unheala, u_unhealb))
#4 * abs(rnorm(Nsim,u_unheal, u_se_unheal))) 
#rgamma(Nsim, u_unheala, scale=u_unhealb))
ProbMatrix <- cbind(ProbMatrix, "t_dur1"=rlnorm(Nsim, t_dur[,1], se_t_dur[,1]))
ProbMatrix <- cbind(ProbMatrix, "t_dur2"=rlnorm(Nsim, t_dur[,2], se_t_dur[,2]))
ProbMatrix <- cbind(ProbMatrix, "t_dur3"=rlnorm(Nsim, t_dur[,3], se_t_dur[,3]))
ProbMatrix <- cbind(ProbMatrix, "t_dur4"=rlnorm(Nsim, t_dur[,4], se_t_dur[,4]))
ProbMatrix <- cbind(ProbMatrix, "t_dur5"=rlnorm(Nsim, t_dur[,5], se_t_dur[,5]))
ProbMatrix <- cbind(ProbMatrix, "t_dur6"=rlnorm(Nsim, t_dur[,6], se_t_dur[,6]))
ProbMatrix <- cbind(ProbMatrix, "t_dur7"=rlnorm(Nsim, t_dur[,7], se_t_dur[,7]))
ProbMatrix <- cbind(ProbMatrix, "nursev1"=rlnorm(Nsim, nursev[,1], se_nursev[,1]))
ProbMatrix <- cbind(ProbMatrix, "nursev2"=rlnorm(Nsim, nursev[,2], se_nursev[,2]))
ProbMatrix <- cbind(ProbMatrix, "nursev3"=rlnorm(Nsim, nursev[,3], se_nursev[,3]))
ProbMatrix <- cbind(ProbMatrix, "nursev4"=rlnorm(Nsim, nursev[,4], se_nursev[,4]))
ProbMatrix <- cbind(ProbMatrix, "nursev5"=rlnorm(Nsim, nursev[,5], se_nursev[,5]))
ProbMatrix <- cbind(ProbMatrix, "nursev6"=rlnorm(Nsim, nursev[,6], se_nursev[,6])) 
ProbMatrix <- cbind(ProbMatrix, "nursev7"=rlnorm(Nsim, nursev[,7], se_nursev[,7]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur1"=rlnorm(Nsim, nurse_dur[,1], se_nurse_dur[,1]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur2"=rlnorm(Nsim, nurse_dur[,2], se_nurse_dur[,2]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur3"=rlnorm(Nsim, nurse_dur[,3], se_nurse_dur[,3]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur4"=rlnorm(Nsim, nurse_dur[,4], se_nurse_dur[,4]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur5"=rlnorm(Nsim, nurse_dur[,5], se_nurse_dur[,5]))
ProbMatrix <- cbind(ProbMatrix, "nurse_dur6"=rlnorm(Nsim, nurse_dur[,6], se_nurse_dur[,6])) 
ProbMatrix <- cbind(ProbMatrix, "nurse_dur7"=rlnorm(Nsim, nurse_dur[,7], se_nurse_dur[,7]))
#ProbMatrix <- cbind(ProbMatrix, "nursev_after"=rnorm(Nsim, nursev_after, se_nursev_after))
ProbMatrix <- cbind(ProbMatrix, "GP_home"=rlnorm(Nsim, GP_home, se_GP_home))
ProbMatrix <- cbind(ProbMatrix, "GP_clinic"=rlnorm(Nsim, GP_clinic, se_GP_clinic))
ProbMatrix <- cbind(ProbMatrix, "Outpts"=rlnorm(Nsim, Outpts, se_Outpts))
ProbMatrix <- cbind(ProbMatrix, "hospv"=rlnorm(Nsim, hospv, se_hospv))
ProbMatrix <- cbind(ProbMatrix, "hospstay"=rlnorm(Nsim, hospstay, se_hospstay))

for(l in 1:100) {
	u_pop[,l] <- (male*(rnorm(Nsim,0.85020,0.03190)+rnorm(Nsim,-0.008,0.0124)*l/10+rnorm(Nsim,-0.0007,0.0011)*(l/10)^2)
	+(1-male)*(rnorm(Nsim,0.87080,0.0279)+rnorm(Nsim,-0.0147,0.0114)*l/10+rnorm(Nsim,-0.0003,0.0011)*(l/10)^2))*0.69/0.757 ## adjust for a ratio of 0.69/0.757
	ProbMatrix <- cbind(ProbMatrix, u_pop[,l])
  colnames(ProbMatrix)[ncol(ProbMatrix)] <- paste("u_pop",l,sep="")
}

#mortutil_data <- read.table('C:/Users/tp994/Downloads/R/V4_DAM_mortutildata_scen1_v3.txt', header=T, sep="\t", quote="\"", dec=".",
  #                          fill=T, na.strings=c(""), as.is=1:3)
#for(l in 1:100) {
#  u_pop[,l] <-rnorm(Nsim,mortutil_data[l,"mnu_pop"], mortutil_data[l,"u_pop.se"])
#  ProbMatrix <- cbind(ProbMatrix, u_pop[,l])
#  colnames(ProbMatrix)[ncol(ProbMatrix)] <- paste("u_pop",l,sep="")
#}


#ProbMatrix1 <- colMeans(ProbMatrix)


CEmodelSA <- function(i,CEmodeldata= ProbMatrix) {  
# i=1 ; CEmodeldata= ProbMatrix; input <- CEmodeldata
input <- CEmodeldata[i,]

#Transition probabilities

for (p in 1:P){
	for(c in 1:C){
	  ### check whether HR=1/exp(d) or =exp(d) before running this code ###
	  ##### Log Normal FE ######
	  p_heal[p,c]<- 1- (1-pnorm(((log(c) - input["lambda_heal"]  + log((input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))
	                   , 0, 1) ) / (1-min(0.9999, pnorm(((log((c-1)) 
	                  - input["lambda_heal"] + log((input[paste("hr",p, sep="")])))*sqrt(input["shape"])) , 0, 1) ))
	  #
	  
	  ### check ### 
	  #p_heal[p,c]<- 1-(1-pnorm(((log(c) - input["lambda_heal"]  + log((input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))
	   #                        , 0, 1) ) # check
	  
	  
	   ##### for predictive RE #####
	  #p_heal[p,c]<- 1- (1-pnorm(((log(c) - input["lambda_heal"]  + log((1/input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))
	      #                      , 0, 1) ) / (1-min(0.9999, pnorm(((log((c-1)) 
	           #                                                    - input["lambda_heal"] + log((1/input[paste("hr",p, sep="")])))*sqrt(input["shape"])) , 0, 1) ))
	  
	  ##### for log logistic ######
  #p_heal[p,c]<- 1- (1/(1+exp(log(c) - input["lambda_heal"]/1.35  + log((input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))) / 
	 #   (1/(1+exp(log(c-1) - input["lambda_heal"]/1.35  + log((input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))) 
	  
	  ## check ##
  #  p_heal[p,c]<- 1-(1/(1+exp(log(c) - input["lambda_heal"]/1.3  + log((input[paste("hr",p, sep="")]))) *sqrt(input["shape"]))) # check 
	  
	  
	  #### for Weibull ####
	  #p_heal[p,c]<- 1- exp(-input["lambda_heal"] * input[paste("hr",p, sep="")] * (((c-1))^input["shape"] - (c)^input["shape"]))
	 
	  #p_heal[p,c]<-  1-exp(-input["lambda_heal"] * input[paste("hr",p, sep="")] * ((c)^input["shape"]))
	  
	 # p_heal[p,c]<-  1-(exp(-input["lambda_heal"] * input[paste("hr",p, sep="")] * ((c)^input["shape"])))/(exp(-input["lambda_heal"] * input[paste("hr",p, sep="")] * ((c-1)^input["shape"])))
	  
	  
	  
  }
}

 

for(c in 1:12){
  p_recur[1,c]<-   1-(1 - pnorm(((log(c) - input["lambda_recur"]) /input["lv_gomp_gamma"]) , 0, 1) )/(1 - pnorm(((log((c-1)) - input["lambda_recur"]) / input["lv_gomp_gamma"]), 0, 1) )
  p_recur[2,c]<-p_recur[1,c]
  p_recur[3,c]<-p_recur[1,c] 
  p_recur[4,c]<-p_recur[1,c]
  p_recur[5,c]<-p_recur[1,c]
  p_recur[6,c]<-p_recur[1,c] 
  p_recur[7,c]<-p_recur[1,c] 
}

for (p in 1:P){
  for(c in 13:(C+12)){
    p_recur[p,c]<- 0
  }
}


for (p in 1:P){
	for(c in 1:(C)){
    #prob of death/year to rate; rate/12; monthly rate to monthly probab
    aux1<- 1 - exp(log(1 - mortutil_data[floor(((c - 1)/12  + age)),"p_allcause"])* mort_ratio/12)    
       
    #From 'healed 1' state	
      mtrace[c,1,1,p]<- 0
			mtrace[c,1,2,p]<- (1 - p_recur[p,1] - aux1)
			mtrace[c,1,3:13,p]<- 0
			mtrace[c,1,14,p]<- p_recur[p,1]
			mtrace[c,1,15,p]<- aux1

      			TOTAL[c,1,p]<-mtrace[c,1,1,p]+mtrace[c,1,2,p]+mtrace[c,1,3,p]+mtrace[c,1,4,p]+mtrace[c,1,5,p]+mtrace[c,1,6,p]+mtrace[c,1,7,p]+mtrace[c,1,8,p]+
                          mtrace[c,1,9,p]+mtrace[c,1,10,p]+mtrace[c,1,11,p]+mtrace[c,1,12,p]+mtrace[c,1,13,p]+mtrace[c,1,14,p]+mtrace[c,1,15,p]

    #From 'healed 2' state	
      mtrace[c,2,1:2,p]<- 0
			mtrace[c,2,3,p]<- (1 - p_recur[p,2] - aux1)
      mtrace[c,2,4:13,p]<- 0
			mtrace[c,2,14,p]<- p_recur[p,2]
			mtrace[c,2,15,p]<- aux1

      			TOTAL[c,2,p]<-mtrace[c,2,1,p]+mtrace[c,2,2,p]+mtrace[c,2,3,p]+mtrace[c,2,4,p]+mtrace[c,2,5,p]+mtrace[c,2,6,p]+mtrace[c,2,7,p]+mtrace[c,2,8,p]+
                          mtrace[c,2,9,p]+mtrace[c,2,10,p]+mtrace[c,2,11,p]+mtrace[c,2,12,p]+mtrace[c,2,13,p]+mtrace[c,2,14,p]+mtrace[c,2,15,p]
      			
    #From 'healed 3' state	
      mtrace[c,3,1:3,p]<- 0
      mtrace[c,3,4,p]<- (1 - p_recur[p,3] - aux1)
			mtrace[c,3,5:13,p]<- 0
			mtrace[c,3,14,p]<- p_recur[p,3]
			mtrace[c,3,15,p]<- aux1

      			TOTAL[c,3,p]<-mtrace[c,3,1,p]+mtrace[c,3,2,p]+mtrace[c,3,3,p]+mtrace[c,3,4,p]+mtrace[c,3,5,p]+mtrace[c,3,6,p]+mtrace[c,3,7,p]+mtrace[c,3,8,p]+
                          mtrace[c,3,9,p]+mtrace[c,3,10,p]+mtrace[c,3,11,p]+mtrace[c,3,12,p]+mtrace[c,3,13,p]+mtrace[c,3,14,p]+mtrace[c,3,15,p]

    #From 'healed 4' state	
      mtrace[c,4,1:4,p]<- 0
			mtrace[c,4,5,p]<- (1 - p_recur[p,4] - aux1)
			mtrace[c,4,6:13,p]<- 0
			mtrace[c,4,14,p]<- p_recur[p,4]
			mtrace[c,4,15,p]<- aux1

      			TOTAL[c,4,p]<-mtrace[c,4,1,p]+mtrace[c,4,2,p]+mtrace[c,4,3,p]+mtrace[c,4,4,p]+mtrace[c,4,5,p]+mtrace[c,4,6,p]+mtrace[c,4,7,p]+mtrace[c,4,8,p]+
                          mtrace[c,4,9,p]+mtrace[c,4,10,p]+mtrace[c,4,11,p]+mtrace[c,4,12,p]+mtrace[c,4,13,p]+mtrace[c,4,14,p]+mtrace[c,4,15,p]
      			
    #From 'healed 5' state	
      mtrace[c,5,1:5,p]<- 0
			mtrace[c,5,6,p]<- (1 - p_recur[p,5] - aux1)
      mtrace[c,5,7:13,p]<- 0
			mtrace[c,5,14,p]<- p_recur[p,5]
			mtrace[c,5,15,p]<- aux1

      			TOTAL[c,5,p]<-mtrace[c,5,1,p]+mtrace[c,5,2,p]+mtrace[c,5,3,p]+mtrace[c,5,4,p]+mtrace[c,5,5,p]+mtrace[c,5,6,p]+mtrace[c,5,7,p]+mtrace[c,5,8,p]+
                          mtrace[c,5,9,p]+mtrace[c,5,10,p]+mtrace[c,5,11,p]+mtrace[c,5,12,p]+mtrace[c,5,13,p]+mtrace[c,5,14,p]+mtrace[c,5,15,p]
      			
    #From 'healed 6' state	
      mtrace[c,6,1:6,p]<- 0
			mtrace[c,6,7,p]<- (1 - p_recur[p,6] - aux1)
      mtrace[c,6,8:12,p]<- 0
			mtrace[c,6,14,p]<- p_recur[p,6]
			mtrace[c,6,15,p]<- aux1

      			TOTAL[c,6,p]<-mtrace[c,6,1,p]+mtrace[c,6,2,p]+mtrace[c,6,3,p]+mtrace[c,6,4,p]+mtrace[c,6,5,p]+mtrace[c,6,6,p]+mtrace[c,6,7,p]+mtrace[c,6,8,p]+
                          mtrace[c,6,9,p]+mtrace[c,6,10,p]+mtrace[c,6,11,p]+mtrace[c,6,12,p]+mtrace[c,6,13,p]+mtrace[c,6,14,p]+mtrace[c,6,15,p]
      			
    #From 'healed 7' state	
      mtrace[c,7,1:7,p]<- 0
			mtrace[c,7,8,p]<- (1 - p_recur[p,7] - aux1)
      mtrace[c,7,9:13,p]<- 0
			mtrace[c,7,14,p]<- p_recur[p,7]
			mtrace[c,7,15,p]<- aux1

      			TOTAL[c,7,p]<-mtrace[c,7,1,p]+mtrace[c,7,2,p]+mtrace[c,7,3,p]+mtrace[c,7,4,p]+mtrace[c,7,5,p]+mtrace[c,7,6,p]+mtrace[c,7,7,p]+mtrace[c,7,8,p]+
                          mtrace[c,7,9,p]+mtrace[c,7,10,p]+mtrace[c,7,11,p]+mtrace[c,7,12,p]+mtrace[c,7,13,p]+mtrace[c,7,14,p]+mtrace[c,7,15,p]
      			
    #From 'healed 8' state	
      mtrace[c,8,1:8,p]<- 0
			mtrace[c,8,9,p]<- (1 - p_recur[p,8] - aux1)
      mtrace[c,8,10:13,p]<- 0
			mtrace[c,8,14,p]<- p_recur[p,8]
			mtrace[c,8,15,p]<- aux1

      			TOTAL[c,8,p]<-mtrace[c,8,1,p]+mtrace[c,8,2,p]+mtrace[c,8,3,p]+mtrace[c,8,4,p]+mtrace[c,8,5,p]+mtrace[c,8,6,p]+mtrace[c,8,7,p]+mtrace[c,8,8,p]+
                          mtrace[c,8,9,p]+mtrace[c,8,10,p]+mtrace[c,8,11,p]+mtrace[c,8,12,p]+mtrace[c,8,13,p]+mtrace[c,8,14,p]+mtrace[c,8,15,p]
      			
    #From 'healed 9' state	
      mtrace[c,9,1:9,p]<- 0
			mtrace[c,9,10,p]<- (1 - p_recur[p,9] - aux1)
      mtrace[c,9,11:13,p]<- 0
			mtrace[c,9,14,p]<- p_recur[p,9]
			mtrace[c,9,15,p]<- aux1
			
            TOTAL[c,9,p]<-mtrace[c,9,1,p]+mtrace[c,9,2,p]+mtrace[c,9,3,p]+mtrace[c,9,4,p]+mtrace[c,9,5,p]+mtrace[c,9,6,p]+mtrace[c,9,7,p]+mtrace[c,9,8,p]+
                          mtrace[c,9,9,p]+mtrace[c,9,10,p]+mtrace[c,9,11,p]+mtrace[c,9,12,p]+mtrace[c,9,13,p]+mtrace[c,9,14,p]+mtrace[c,9,15,p]
			
    #From 'healed 10' state	
      mtrace[c,10,1:10,p]<- 0
			mtrace[c,10,11,p]<- (1 - p_recur[p,10] - aux1)
      mtrace[c,10,12:13,p]<- 0
			mtrace[c,10,14,p]<- p_recur[p,10]
			mtrace[c,10,15,p]<- aux1

      			TOTAL[c,10,p]<-mtrace[c,10,1,p]+mtrace[c,10,2,p]+mtrace[c,10,3,p]+mtrace[c,10,4,p]+mtrace[c,10,5,p]+mtrace[c,10,6,p]+mtrace[c,10,7,p]+mtrace[c,10,8,p]+
                          mtrace[c,10,9,p]+mtrace[c,10,10,p]+mtrace[c,10,11,p]+mtrace[c,10,12,p]+mtrace[c,10,13,p]+mtrace[c,10,14,p]+mtrace[c,10,15,p]
      			
    #From 'healed 11' state	
      mtrace[c,11,1:11,p]<- 0
			mtrace[c,11,12,p]<- (1 - p_recur[p,11] - aux1)
      mtrace[c,11,13,p]<- 0
      mtrace[c,11,14,p]<- p_recur[p,11]
			mtrace[c,11,15,p]<- aux1

       			TOTAL[c,11,p]<-mtrace[c,11,1,p]+mtrace[c,11,2,p]+mtrace[c,11,3,p]+mtrace[c,11,4,p]+mtrace[c,11,5,p]+mtrace[c,11,6,p]+mtrace[c,11,7,p]+mtrace[c,11,8,p]+
                          mtrace[c,11,9,p]+mtrace[c,11,10,p]+mtrace[c,11,11,p]+mtrace[c,11,12,p]+mtrace[c,11,13,p]+mtrace[c,11,14,p]+mtrace[c,11,15,p]
       			
    #From 'healed 12' state	
      mtrace[c,12,1:12,p]<- 0
			mtrace[c,12,13,p]<- (1 - p_recur[p,12] - aux1)
			mtrace[c,12,14,p]<- p_recur[p,12]
			mtrace[c,12,15,p]<- aux1

            TOTAL[c,12,p]<-mtrace[c,12,1,p]+mtrace[c,12,2,p]+mtrace[c,12,3,p]+mtrace[c,12,4,p]+mtrace[c,12,5,p]+mtrace[c,12,6,p]+mtrace[c,12,7,p]+mtrace[c,12,8,p]+
                          mtrace[c,12,9,p]+mtrace[c,12,10,p]+mtrace[c,12,11,p]+mtrace[c,12,12,p]+mtrace[c,12,13,p]+mtrace[c,12,14,p]+mtrace[c,12,15,p]
            
    #From 'healed >12' state	
      mtrace[c,13,1:12,p]<- 0
			mtrace[c,13,13,p]<- (1 - p_recur[p,c + 12] - aux1)
			mtrace[c,13,14,p]<- p_recur[p,c + 12]
			mtrace[c,13,15,p]<- aux1
			
            TOTAL[c,13,p]<-mtrace[c,13,1,p]+mtrace[c,13,2,p]+mtrace[c,13,3,p]+mtrace[c,13,4,p]+mtrace[c,13,5,p]+mtrace[c,13,6,p]+mtrace[c,13,7,p]+mtrace[c,13,8,p]+
                          mtrace[c,13,9,p]+mtrace[c,13,10,p]+mtrace[c,13,11,p]+mtrace[c,13,12,p]+mtrace[c,13,13,p]+mtrace[c,13,14,p]+mtrace[c,13,15,p]
             
    #From 'unhealed' state	
      mtrace[c,14,1,p]<- p_heal[p,c]
      mtrace[c,14,2:13,p]<- 0
      mtrace[c,14,14,p]<- (1 - p_heal[p,c] - aux1)
			mtrace[c,14,15,p]<- aux1
			
            TOTAL[c,14,p]<-mtrace[c,14,1,p]+mtrace[c,14,2,p]+mtrace[c,14,3,p]+mtrace[c,14,4,p]+mtrace[c,14,5,p]+mtrace[c,14,6,p]+mtrace[c,14,7,p]+mtrace[c,14,8,p]+
                          mtrace[c,14,9,p]+mtrace[c,14,10,p]+mtrace[c,14,11,p]+mtrace[c,14,12,p]+mtrace[c,14,13,p]+mtrace[c,14,14,p]+mtrace[c,14,15,p]
             
    #From 'death' state	
      mtrace[c,15,1:14,p]<- 0
			mtrace[c,15,15,p]<- 1

            TOTAL[c,15,p]<-mtrace[c,15,1,p]+mtrace[c,15,2,p]+mtrace[c,15,3,p]+mtrace[c,15,4,p]+mtrace[c,15,5,p]+mtrace[c,15,6,p]+mtrace[c,15,7,p]+mtrace[c,15,8,p]+
                          mtrace[c,15,9,p]+mtrace[c,15,10,p]+mtrace[c,15,11,p]+mtrace[c,15,12,p]+mtrace[c,15,13,p]+mtrace[c,15,14,p]+mtrace[c,15,15,p]

	
  	}
	}

#Number of individuals in each state at time t

for (p in 1:P) {
	pi_[1,1:13,p]<- 0
	pi_[1,14,p]<- 1000
	pi_[1,15,p]<- 0

	CHECK[1,p]<- pi_[1,1,p] + pi_[1,2,p] + pi_[1,3,p] + pi_[1,4,p] + pi_[1,5,p] + pi_[1,6,p] + pi_[1,7,p] + pi_[1,8,p] + pi_[1,9,p] + pi_[1,10,p] + pi_[1,11,p] + 
               pi_[1,12,p] + pi_[1,13,p] + pi_[1,14,p] + pi_[1,15,p]
	
	}

for (p in 1:P) {
	for(c in 2:C) {
		for (s in 1:S) {
			pi_[c,s,p]<- pi_[(c - 1),,p] %*% mtrace[c,,s,p]
			}
	  CHECK[c,p]<-pi_[c,1,p] + pi_[c,2,p] + pi_[c,3,p] + pi_[c,4,p] + pi_[c,5,p] + pi_[c,6,p] + pi_[c,7,p] + pi_[c,8,p] + pi_[c,9,p] + pi_[c,10,p] + pi_[c,11,p] + 
                pi_[c,12,p] + pi_[c,13,p] + pi_[c,14,p] + pi_[c,15,p]
		}	
	}


#-------- COSTS IN EACH STATE ------------#

# Treatment costs per month
	
for (p in 1:P) {
  for (c in 1:C) {
  	cost[c,1:13,p]<- 0
	  cost[c,15,p]<- 0
    cost[c,14,p]<- input[paste("nursev",p,sep="")] *input[paste("nurse_dur",p,sep="")]*c_nurse[,p]/60 
    + input[paste("t_dur",p,sep="")]  + input["GP_home"] * c_GP_home + input["GP_clinic"] * c_GP_clinic + input["Outpts"] * c_Outpts + 
      input["hospv"] * c_hospv # +  input["hospstay"] * c_hospstay 
    
    }
  }


#Costs in each cycle of model
for (p in 1:P) {
	for(c in 1:C) {
		ct_[c,p] <- pi_[c,,p] %*% cost[c,,p] / ((1 + cDR/12)^(c - 1))
	
		}
	TotC[p] <- sum(ct_[,p])
	mean_C[p] <- TotC[p] / N	
	}

#-------- UTILITIES IN EACH STATE ------------#

for (p in 1:P) {
  for (c in 1:C) {
    u[c,1:13,p]<- (input[paste("u_pop",floor((c - 1)/12 + age), sep="")])/12
    u[c,14,p]<- (input[paste("u_pop",floor((c - 1)/12 + age), sep="")] - (input["u_decunh"]))/12
    #    max((input[paste("u_pop",floor(((c - 1)/12 + age)), sep="")] - (input["u_decunh"]))/12, -0.594)
    u[c,15,p]<- 0
  }	
}



#Utlities in each cycle of model
for (p in 1:P){
  for(c in 1:C) {
    ut_[c,p] <- pi_[c,,p] %*% u[c,,p] / ((1 + uDR/12)^(c - 1))
  }
  TotU[p] <- sum(ut_[,p])
  mean_U[p] <- TotU[p] / N
}



for (p in 1:6){
  inc_C[p] <- (mean_C[p]-mean_C[7])
  inc_U[p] <- (mean_U[p]-mean_U[7])## BC as reference
  INMB[p] <-inc_U[p]*threshold-inc_C[p]
}
inc_C[7] <- 0
inc_U[7] <- 0
INMB[7] <- 0

#icer[1] <- 0
#icer[2] <- (mean_C[2]-mean_C[7])/(mean_U[2] - mean_U[7])
#icer[3] <- 0
#icer[4] <- (mean_C[4]-mean_C[7])/(mean_U[4] - mean_U[7])
#icer[5] <- (mean_C[5]-mean_C[7])/(mean_U[5] - mean_U[7])
#icer[6] <- (mean_C[6]-mean_C[7])/(mean_U[6] - mean_U[7])
#icer[7] <- 0

for (p in 1:7){
  NMB[p] <- mean_U[p]*threshold-mean_C[p]
}


return(c(mean_C, mean_U, inc_C, inc_U, NMB, INMB))

}



Nsim1<-5000

system.time(CE_out <- sapply(1:Nsim1,CEmodelSA))
#CE_out
#save.image("C:/Users/tp994/Downloads/R/V6_test.RData")

x<-rowMeans(CE_out)

x_lCI<-c(quantile(CE_out[1,],0.025),quantile(CE_out[2,],0.025),quantile(CE_out[3,],0.025),0,quantile(CE_out[5,],0.025),
        quantile(CE_out[6,],0.025),quantile(CE_out[7,],0.025),quantile(CE_out[8,],0.025),quantile(CE_out[9,],0.025),quantile(CE_out[10,],0.025),0,quantile(CE_out[12,],0.025),
        quantile(CE_out[13,],0.025),quantile(CE_out[14,],0.025),quantile(CE_out[15,],0.025),quantile(CE_out[16,],0.025),quantile(CE_out[17,],0.025),0,quantile(CE_out[19,],0.025),
        quantile(CE_out[20,],0.025),quantile(CE_out[21,],0.025),quantile(CE_out[22,],0.025),quantile(CE_out[23,],0.025),quantile(CE_out[24,],0.025),0,quantile(CE_out[26,],0.025),
        quantile(CE_out[27,],0.025),quantile(CE_out[28,],0.025),quantile(CE_out[29,],0.025),quantile(CE_out[30,],0.025),quantile(CE_out[31,],0.025),0,quantile(CE_out[33,],0.025),
        quantile(CE_out[34,],0.025),quantile(CE_out[35,],0.025),quantile(CE_out[36,],0.025),quantile(CE_out[37,],0.025),quantile(CE_out[38,],0.025),0,quantile(CE_out[40,],0.025),
        quantile(CE_out[41,],0.025),quantile(CE_out[42,],0.025))
x_uCI<-c(quantile(CE_out[1,],0.975),quantile(CE_out[2,],0.975),quantile(CE_out[3,],0.975),0,quantile(CE_out[5,],0.975),
         quantile(CE_out[6,],0.975),quantile(CE_out[7,],0.975),quantile(CE_out[8,],0.975),quantile(CE_out[9,],0.975),quantile(CE_out[10,],0.975),0,quantile(CE_out[12,],0.975),
         quantile(CE_out[13,],0.975),quantile(CE_out[14,],0.975),quantile(CE_out[15,],0.975),quantile(CE_out[16,],0.975),quantile(CE_out[17,],0.975),0,quantile(CE_out[19,],0.975),
         quantile(CE_out[20,],0.975),quantile(CE_out[21,],0.975),quantile(CE_out[22,],0.975),quantile(CE_out[23,],0.975),quantile(CE_out[24,],0.975),0,quantile(CE_out[26,],0.975),
         quantile(CE_out[27,],0.975),quantile(CE_out[28,],0.975),quantile(CE_out[29,],0.975),quantile(CE_out[30,],0.975),quantile(CE_out[31,],0.975),0,quantile(CE_out[33,],0.975),
         quantile(CE_out[34,],0.975),quantile(CE_out[35,],0.975),quantile(CE_out[36,],0.975),quantile(CE_out[37,],0.975),quantile(CE_out[38,],0.975),0,quantile(CE_out[40,],0.975),
         quantile(CE_out[41,],0.975),quantile(CE_out[42,],0.975))
x_meanCE<- cbind(x,x_lCI,x_uCI)
write.csv(CE_out, "CE_out.csv")
write.csv(x_meanCE,"CE_meanCE.csv")

#Cost-effectiveness
NB = array(NA, dim=c(P,K, Nsim1), dimnames=list(seq(1,P,1),seq(1,K,1), seq(1,Nsim1,1)))

######## CALCULATING NET MONETARY BENEFITS (CEILING RATIOS FROM ?0 TO ?100,000 #############

for(i in 1:Nsim1) {
  for(k in 1:K) {
  	Rc[k]<- (k - 1) * 1000
  	for (p in 1:P) {
  		NB[p,k,i]<- Rc[k] * CE_out[5+p,i] - CE_out[p,i]
  		}
	}
}

aux = array(NA, dim=c(Nsim1,K), dimnames=list(seq(1,Nsim1,1),seq(1,K,1)))
pCE = array(NA, dim=c(P,K), dimnames=list(seq(1,P,1),seq(1,K,1)))
meanNB = array(NA, dim=c(K,P), dimnames=list(seq(1,K,1),seq(1,P,1)))
CI = array(NA, dim=c(K), dimnames=list(seq(1,K,1)))
PI = array(NA, dim=c(K), dimnames=list(seq(1,K,1)))
EVPI = array(NA, dim=c(K), dimnames=list(seq(1,K,1)))

for(k in 1:K) {
  aux[,k] <- apply(NB[,k,],2, max, na.rm=T)
  for (p in 1:P) {
  	pCE[p,k]<- sum(aux == NB[p,k,], na.rm=T)/Nsim1
    meanNB[k,p]<- mean(NB[p,k,])
    }
  CI[] <- apply(meanNB[,],1,max)
  PI[k] <- mean(aux[,k])
  EVPI[k] <- PI[k] - CI[k]
}  

########## CALCULATING POPULATION EVPI ##########

discEffpop<- matrix(data = NA, nrow = 5, ncol = 1)
popEVPI<- matrix(data = NA, nrow = 101, ncol = 1)
#median time to healing: 99 days
#duration in 1 year
dura <- 99/365
#UK population: 62.000.000
#VLU prevalence: 0.16%
Effpop<- 62000000 * (0.16/100) / dura


for(k in 1:5) {  
  discEffpop[k]<- Effpop /(1 + cDR)^(k-1)  
  }
sumdiscEffpop<-sum(discEffpop)  

for(k in 1:K) {  
  popEVPI[k]<- sumdiscEffpop * EVPI[k]
  }


#####----------------------------------------------
### PLOTTING CEACs #######
#####----------------------------------------------

namc_NB_mtx<- seq(0,100000,1000)
plot(namc_NB_mtx/1000,pCE[1,],xlim=c(0,100000/1000),ylim=c(0,1.0),xlab="Threshold ratio (x1000?)", ylab="Probability cost-effective",
     main="Acceptability curves",type="l",lty=1,lwd=3,col="black",axes=T,font.lab=2)
lines(namc_NB_mtx/1000,pCE[2,],lty=2,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE[3,],lty=3,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE[4,],lty=4,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE[5,],lty=5,lwd=3,col="black")
legend(50000/1000,1,legend=c("4LB","SSB","HH","Paste","2LB"),lty=c(1,2,3,4,5),lwd=c(3,3,3,3,3),col=c("black","black","black","black","black"),
      bty="n",cex=1)

#####----------------------------------------------
### PLOTTING PCE vs NB #######
#####----------------------------------------------

plot(meanNB[21,1]/1000,pCE[1,21],xlim=c(45000/1000,65000/1000),ylim=c(0,1.0),xlab="Net Monetary Benefits at ?20k threshold value (x1000?)", 
     ylab="Probability cost-effective",main="",type="p",pch=21,bg="black",col="black",axes=T,font.lab=2,cex=1.5)
points(meanNB[21,2]/1000,pCE[2,21],pch=22,bg="black",,col="black",cex=1.5)
points(meanNB[21,3]/1000,pCE[3,21],pch=23,bg="black",,col="black",cex=1.5)
points(meanNB[21,4]/1000,pCE[4,21],pch=24,bg="black",,col="black",cex=1.5)
points(meanNB[21,5]/1000,pCE[5,21],pch=25,bg="black",,col="black",cex=1.5)
legend(55000/1000,1,legend=c("4LB","SSB","HH","Paste","2LB"),pch=c(21,22,23,24,25),pt.cex=c(1.5,1.5,1.5,1.5,1.5),
      col=c("black","black","black","black","black"),pt.bg=c("black","black","black","black","black"), bty="n",cex=1.5)

#####----------------------------------------------
### PLOTTING AbsCosts vs AbsEffects #######
#####----------------------------------------------

plot(x[6],x[1],xlim=c(3.6,4),ylim=c(15000,25000),xlab="Absolute Effects (QALYs)", ylab="Absolute Costs (?)",main="",type="p",pch=21,bg="black",col="black",axes=T,
      font.lab=2,cex=1.5)
points(x[7],x[2],pch=22,bg="black",,col="black",cex=1.5)
points(x[8],x[3],pch=23,bg="black",,col="black",cex=1.5)
points(x[9],x[4],pch=24,bg="black",,col="black",cex=1.5)
points(x[10],x[5],pch=25,bg="black",,col="black",cex=1.5)
legend(3.9,25000,legend=c("4LB","SSB","HH","Paste","2LB"),pch=c(21,22,23,24,25),pt.cex=c(1.5,1.5,1.5,1.5,1.5),
      col=c("black","black","black","black","black"),pt.bg=c("black","black","black","black","black"), bty="n",cex=1.5)

########## CALCULATING THE PCE FOR FRONTIER ###########	

pCE_CEAF = array(NA, dim=c(K,P), dimnames=list(seq(1,K,1),seq(1,P,1)))

for(k in 1:K) {
   if (CI[k] == meanNB[k,1]) pCE_CEAF[k,1]<- pCE[1,k] else pCE_CEAF[k,1]<- NA
   if (CI[k] == meanNB[k,2]) pCE_CEAF[k,2]<- pCE[2,k] else pCE_CEAF[k,2]<- NA
   if (CI[k] == meanNB[k,3]) pCE_CEAF[k,3]<- pCE[3,k] else pCE_CEAF[k,3]<- NA
   if (CI[k] == meanNB[k,4]) pCE_CEAF[k,4]<- pCE[4,k] else pCE_CEAF[k,4]<- NA
   if (CI[k] == meanNB[k,5]) pCE_CEAF[k,5]<- pCE[5,k] else pCE_CEAF[k,5]<- NA
   }



#####----------------------------------------------
### PLOTTING CEAF AND EVPI #######
#####----------------------------------------------

plot(namc_NB_mtx/1000,pCE_CEAF[,1],xlim=c(0,100000/1000),ylim=c(0,1.0),xlab="Value of ceiling ratio (?)", ylab="Probability Cost-Effective",
     main="Acceptability Frontier",type="l",lty=1,lwd=3,col="black",axes=T)
lines(namc_NB_mtx/1000,pCE_CEAF[,2],lty=2,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE_CEAF[,3],lty=3,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE_CEAF[,4],lty=4,lwd=3,col="black")
lines(namc_NB_mtx/1000,pCE_CEAF[,5],lty=5,lwd=3,col="black")
legend(60000/1000,0.4,legend=c("4LB","SSB","HH","Paste","2LB"),lty=c(1,2,3,4,5),lwd=c(3,3,3,3,3),col=c("black","black","black","black","black"),
       bty="n",cex=1)


#####----------------------------------------------
### PLOTTING POPULATION EVPI #######
#####----------------------------------------------

plot(namc_NB_mtx/1000,popEVPI/100000,xlim=c(0,100000/1000),ylim=c(0,500000000/100000),xlab="Value of ceiling ratio (x1000?)", ylab="Population EVPI (x100000?)",
     main="Population EVPI",type="l",lty=1,lwd=3,col="black",axes=T)



###
ProbMatrix1 <- colMeans(ProbMatrix)
