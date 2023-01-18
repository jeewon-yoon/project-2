setwd("c:/users/USER/jeewon")
library(readxl)
res <- read_excel("results_hss.xlsx")
colnames(res)[9]<- "rmse.m2"

## DV: rmse
dv<- c(res$rmse.m1,res$rmse.m2)

# simulation conditions
model<- c(rep("m1",36),rep("m2",36))
nperson <- factor(rep(c(rep(30,18),rep(50,18)),2))
nitem <- factor(rep(c(rep(10,9),rep(20,9)),4))
fixed <- factor(rep(c(0,0,0,25,25,25,50,50,50),8))  
random <- factor(rep(c("equal","small","large"),24), levels=c("equal","small","large"))

## data.frame
dat<- data.frame(dv,nperson,nitem,fixed,random, model)

#
library(ggplot2)
ggplot(data=dat, 
       aes(x=random, y=dv)) +
  geom_point(aes(color=model, shape=nitem)) +
  facet_wrap(nperson~fixed)



##### DV: Type 1 error rate
rate<- c(res$rate.m1,res$rate.m2)

dv<- rate[fixed==0]
model<- c(rep("m1",36),rep("m2",36))[fixed==0]
nperson <- factor(rep(c(rep(30,18),rep(50,18)),2))[fixed==0]
nitem <- factor(rep(c(rep(10,9),rep(20,9)),4))[fixed==0]
random <- factor(rep(c("equal","small","large"),24), 
                 levels=c("equal","small","large"))[fixed==0]

## data.frame
dat<- data.frame(dv,nperson,nitem,random, model)

ggplot(data=dat, 
       aes(x=random, y=dv)) +
  geom_point(aes(color=model)) +
  facet_wrap(nperson~nitem)


#### DV: Power 

rate<- c(res$rate.m1,res$rate.m2)

dvvvv<- rate[fixed!=0]
model<- c(rep("m1",36),rep("m2",36))[fixed!=0]
nperson <- factor(rep(c(rep(30,18),rep(50,18)),2))[fixed!=0]
nitem <- factor(rep(c(rep(10,9),rep(20,9)),4))[fixed!=0]
random <- factor(rep(c("equal","small","large"),24), 
                 levels=c("equal","small","large"))[fixed!=0]
fixed <- factor(rep(c(25,25,25,50,50,50),8)) 

## data.frame
dat<- data.frame(dv,nperson,nitem,random, model)

ggplot(data=dat, 
       aes(x=random, y=dv)) +
  geom_point(aes(color=model, shape=fixed)) +
  facet_wrap(nperson~nitem)
