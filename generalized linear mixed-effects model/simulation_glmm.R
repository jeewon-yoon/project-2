## set seed and path, load libraries
set.seed(1234567)
path<- (" ")
setwd(path)
library(MASS)
library(lme4)


## number of replications
nrep= 1000

## conditions 

# number of persons: 30, 50, 100
# number of items per level: 10, 20, 50

# fixed effect (b0): 0, 1, 2
# fixed effect (b1): 0, 0.2, 0.8

# person random effect: 
# tau0(sd): 0.5
# tau1(sd): 0.2
# corr(tau0,tau1): 0.6
# item random effect(sd): 
#omega=0.2 

## set parameters
nperson=30;  
nitem=10;
b0 = 0;  
b1 = 0; 
tau0=0.5;  tau1=0.2;  r=0.6;
omega=0.2;
S= matrix(c(tau0^2, r*tau0*tau1,
            r*tau0*tau1, tau1^2),
          nrow=2)


## design matrix  

#id
j=rep(c(1:nperson),each=2*nitem)
i=rep(c(1:nitem),2*nperson)

# fixed 
x0=1
x1= rep(c(rep(0, nitem),rep(1,nitem)),nperson)


###### spaces to save
ll<- numeric(0)
fixed<- numeric(0)
#### FOR LOOP

for (n in 1:nrep) {
  
  # generate random effects
  s = mvrnorm(nperson, mu=c(0,0), Sigma=S)
  w1 = rnorm(nitem,mean=0, sd=omega)
  
  # person random
  ss = rep(s[,1],each=2*nitem)*x0 + rep(s[,2],each=2*nitem)*x1
  
  # item random 
  w= rep(w1, 2*nperson)
  
  # logit 
  logit = (b0*x0 + b1*x1)+ ss+ w  
    
  ## probability 
  p <- 1/(1 + exp(-logit))  
  
  ## from unif(0,1) 
  probunif<- runif(nperson*nitem*2) 
  
  # y 	
  y<- ifelse(probunif<p,1,0) 
  
  ## data frame
  dat<- data.frame(j, i, x0, x1, ss,w,y,logit)
  
  ###### estimation
  m1=glmer(y ~ 1+x1+(1+x1|j)+(1|i), family=binomial ,data=dat) # maximal model(i.e., random slope) 
  m2=glmer(y ~ 1+x1+(1|j)+(1|i), family=binomial, data=dat)    # random intercept-only
  m1.null = glmer(y ~ 1+(1+x1|j)+(1|i), family=binomial, data=dat)  
  m2.null = glmer(y ~ 1+(1|j)+(1|i), family=binomial, data=dat) 
  
  
  ## several things to save
  
  # b1
  fixed1<- summary(m1)$coefficients[c(2,4)]
  fixed2<- summary(m2)$coefficients[c(2,4)]
  fixed<- rbind(fixed, c(fixed1,fixed2)) 
  
  # log-likelihood
  ll1<- summary(m1)$logLik
  ll2<- summary(m2)$logLik
  ll1.null<- summary(m1.null)$logLik
  ll2.null<- summary(m2.null)$logLik
  ll_n<- c(ll1, ll2, ll1.null, ll2.null)
  ll<- rbind(ll,ll_n)
  
} ## END of FOR LOOP

## save files 
# fixed effect
colnames(fixed) <- c("m1est","m1se","m2est","m2se")
write.table(fixed,file="fixed.txt",
            sep="\t",row.names = F,col.names = T)

# log-likelihood
colnames(ll) <- c("ll.m1","ll.m2","ll.m1.null", "ll.m2.null")
write.table(ll, file="ll.txt",
            sep="\t",row.names = F, col.names = T)

