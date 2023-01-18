## set seed and path, load libraries
set.seed(123456)
path <- ("C:/Users/USER/jeewon")
setwd(path)
library(MASS)
library(lme4)


## number of replications
nrep= 10

## varying conditions (adapted from Matuschek et al.(2017))

# number of persons: 30, 50
# ?umber of items per level: 10, 20
# fixed effect (b0): 2000
# fixed effect (b1): 0, 25, 50
# person random effect: 
# tau0(sd): 100
# tau1(sd): 20
# corr(tau0,tau1): 0.6
# item random effect(sd): 
#[omega1,omega2]=[20,20], [20,60], [20,100] 
# sigma(=sd(e))? 300


## set parameters
nperson=30; nitem=10;
b0=2000; b1=25; 
tau0=100; tau1=20; r=0.6;
omega1=20; omega2=100;
sigma=300
S= matrix(c(tau0^2, r*tau0*tau1,
            r*tau0*tau1, tau1^2),
          nrow=2)


## design matrix  

#id
j=rep(c(1:nperson), ea?h=2*nitem)
i=rep(c(1:(2*nitem)), nperson)
k1 <- rep(c(rep(1,nitem), rep(0,nitem)), nperson)
k2 <- rep(c(rep(0,nitem), rep(1,nitem)), nperson)

# fixed 
x0=1
x1= rep(c(rep(0, nitem), rep(1,nitem)), nperson)


###### spaces to save

ll<- numeric(0)
fixed<- n?meric(0)


#### FOR LOOP

for (n in 1:nrep) {
  
  # generate random effects
  
  s=mvrnorm(nperson, mu=c(0,0), Sigma=S)
  w1=rnorm(nitem, mean=0, sd=omega1)
  w2=rnorm(nitem, mean=0, sd=omega2)
  e=rnorm(nperson*nitem*2, mean=0, sd=sigma)
  
  # person ra?dom
  
  ss=rep(s[,1],each=2*nitem)*x0+
    rep(s[,2],each=2*nitem)*x1
  
  # item random 
  w= rep(c(w1,w2), nperson)
  
  # y
  y = (b0*x0+b1*x1)+ ss + w + e
  
  ## data frame
  dat<- data.frame(j, i, k1,k2,x0,x1, ss,w,e,y)
  
  
  ###### estimation
  m?=lmer(y~1+x1+(1+x1|j)+(1|i), data=dat, REML=F)
  m2=lmer(y~1+x1+(1+x1|j)+(-1+k1|i)+(-1+k2|i), data=dat, REML=F)
  
  ## several things to save
  
  # b1
  fixed1<- summary(m1)$coefficients[c(2,4)]
  fixed2<- summary(m2)$coefficients[c(2,4)]
  fixed_n<-c(fi?ed1,fixed2)
  fixed<- rbind(fixed,fixed_n) 
  
  # log-likelihood
  ll1<- summary(m1)$logLik
  ll2<- summary(m2)$logLik
  ll_n<- c(ll1,ll2)
  ll<- rbind(ll,ll_n)
  
}  
## END of FOR LOOP


## save files 
# fixed effect
colnames(fixed) <- c("m1est","m1se",?m2est","m2se")
write.table(fixed,file="fixed_nri.txt",
            sep="\t", row.names = F, col.names = T)

# log-likelihood
colnames(ll) <- c("m1ll","m2ll")
write.table(ll, file="ll_nri.txt",
            sep="\t", row.names = F, col.names = T)
