## set seed and path, load libraries
path<- (" ")
setwd(path)
library(MASS)
library(lme4)

## load files
dat<- read.table("fixed.txt", header=T) 
dat2<- read.table("ll.txt", header=T)


#### 1. Maximal model (M1) --- (Wald & LRT)

## 1.1. bias
b0 = 0  ## fixed effect magnitude (0, 1, 2)
b1 = 0  ## fixed effect magnitude (0, 0.2, 0.8) 

bias.max = mean(dat$m1est)-b1


## 1.2. RMSE
rmse.max = sd(dat$m1est-b1)

## 1.3. Type 1 error rate or Power
### 1.3.1. Wald test
t.max = dat$m1est / dat$m1se
wald.sig.max = ifelse(t.max>qnorm(.975),1,0) 
wald.rate.max = mean(wald.sig.max)  

### 1.3.2. LRT
deviance.max <- -2*(dat2$ll.m1.null-dat2$ll.m1) 
LRT.sig.max<- ifelse(deviance.max>qchisq(.95,1),1,0) 
LRT.rate.max = mean(LRT.sig.max) 


#### 2. Model comparison --- (Wald & LRT)

### 2.0. LRT for model selection
deviance<- -2*(dat2$ll.m1-dat2$ll.m2) 
select.m1<- ifelse(deviance>qchisq(.95 ,1),1,0) 
b1est.final<- ifelse(deviance>qchisq(.95 ,1), dat$m1est,dat$m2est)
b1se.final<- ifelse(deviance>qchisq(.95 ,1), dat$m1se,dat$m2se)

## 2.1. bias
bias.MC= mean(b1est.final) - b1

## 2.2. RMSE
rmse.MC= sd(b1est.final-b1)

## 2.3. Type 1 error rate or Power
### 2.3.1. Wald test
t.MC=b1est.final/b1se.final
wald.sig.MC= ifelse(t.MC>qnorm(.975),1,0)
wald.rate.MC = mean(wald.sig.MC)

### 2.3.2. LRT
deviance.MC<- ifelse(deviance>qchisq(.95,1),
                     -2*(dat2$ll.m1.null-dat2$ll.m1),
                     -2*(dat2$ll.m2.null-dat2$ll.m2))
LRT.sig.MC<- ifelse(deviance.max>qchisq(.95,1),1,0)
LRT.rate.MC =mean(LRT.sig.MC)

## summary 
table <- numeric(0)
table<- c(bias.max, bias.MC,  rmse.max, rmse.MC,  wald.rate.max, wald.rate.MC, 
          LRT.rate.max,LRT.rate.MC)
table
