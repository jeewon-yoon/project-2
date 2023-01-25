## set seed and path, load libraries
set.seed(1234567)
path<- ("c:/users/USER/jeewon")
setwd(path)
library(MASS)
library(lme4)

## number of replications
nrep= 1000

## conditions (adapted from Matuschek et al.(2017))
# number of persons: 30, 50
# number of items per level: 10, 20
# fixed effect (b0): 2000
# fixed effect (b1): 0, 25
# person random effect: 
# tau0(sd): 100
# tau1(sd): 20
# corr(tau0,tau1): 0.6
# item random effect(sd): 
# omega=20, 60, 100 
# sigma(=sd(e)): 300


## set parameters
nperson=30 nitem=10;
b0=2000; b1=25; 
tau0=100; tau1=20; r=0.6;
omega=20;
sigma=300

S = matrix(c(tau0^2, r*tau0*tau1,
            r*tau0*tau1, tau1^2),
          nrow=2)


## design matrix  
#id
j=rep(c(1:nperson),each=2*nitem)
i=rep(c(1:nitem),2*nperson)

# fixed x0=1
x1=rep(c(rep(0, nitem),rep(1,nitem)),nperson)


###### spaces to save
ll<- numeric(0)
fixed<- numeric(0)
sig<- numeric(0)


#### FOR LOOP
for (n in 1:nrep) 
  {
  
  # generate random effects
  s=mvrnorm(nperson, mu=c(0,0), Sigma=S)
  w1=rnorm(nitem, mean=0, sd=omega)
  e=rnorm(nperson*nitem*2, mean=0, sd=sigma)
  
  # person random
  ss=rep(s[,1],each=2*nitem)*x0+
    rep(s[,2],each=2*nitem)*x1
  
  # item random 
  w = rep(w1, 2*nperson)
  
  # y
  y = (b0*x0 + b1*x1)+ss+w+e
  
  ## data frame
  dat<-data.frame(j, i, x0, x1, ss, w,e,y)
  
  ###### estimation
  m0=lmer(y ~ 1 + (1+x1|j)+(1|i), data=dat, REML=F)
  m1=lmer(y ~ 1 + x1 +(1+x1|j)+(1|i), data=dat, REML=F)
  
  
  ## several things to save
  # b1
  fixed1<- summary(m1)$coefficients[c(2,4)]
  fixed<- rbind(fixed,fixed1) 
  z_value<- summary(m1)$coefficients[c(2)]/summary(m1)$coefficients[c(4)]
  p_value<- pnorm(z_value, mean=0, sd=1, lower.tail = FALSE, log.p = FALSE)
  if (p_value<0.05) {
    sig1<- 1
  } else {
    sig1<- 0
  }
  sig<- rbind(sig, sig1)
  
  # log-likelihood
  ll0<- summary(m0)$logLik
  ll1<- summary(m1)$logLik
  ll_n<- c(ll0,ll1)
  ll<- rbind(ll,ll_n)
  
} 
## END of FOR LOOP

## save files 
# fixed effect
colnames(fixed) <- c("m1est","m1se")
write.table(fixed, file="fixed.txt",
            sep="\t", row.names = F, col.names = T)

# significance of each repetition
colnames(sig) <- sig
write.table(sig, file="sig.txt", sep = "\t", row.names = F, col.names = T)

# proportion of significance
sig_level<-sum(sig[1:1000])/nrep*100

# log-l?kelihood
colnames(ll) <- c("m0ll","m1ll")
write.table(ll, file="ll.txt",
            sep="\t",row.names = F, col.names = T)

