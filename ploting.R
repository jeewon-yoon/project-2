setwd(" ")
library(ggplot2)

## DV: rmse
dat.rmse <- read.delim(" ~/glmm_bias.txt")

ggplot(data=dat.rmse, 
       aes(x=factor(b0), y=RMSE)) +
  geom_point(size=3,aes(shape=factor(b1), color=approach)) +
  facet_wrap(factor(nperson,
                    labels=c("30persons","50persons"))~factor(nitem, labels=c("10items","20items"))) + ylim(0,0.5)+ labs(x="Beta0", shape="Beta1") 


##### DV: Type 1 error rate
dat.type1error <- read.delim(" ~/glmm_type1error.txt")

ggplot(data=dat.type1error, 
       aes(x=factor(b0), y=rate)) +
  geom_point(size=3,aes(shape=test, color=approach)) +
  facet_wrap(factor(nperson,
                    labels=c("30persons","50persons"))~factor(nitem,
                                                              labels=c("10items","20items"))) +
  ylim(0,0.1)+ 
  geom_abline(intercept = 0.05, slope=0, colour = "red")+
  labs(x="Beta0",y="Type I error rate") 


#### DV: Power 

dat.power <- read.delim(" ~/glmm_power.txt")

ggplot(data=dat.power, 
       aes(x=factor(b0), y=rate)) +
  geom_point(size=3,aes(shape=test, color=approach)) +
  facet_wrap(factor(b1, labels=c("beta1=0.2","beta1=0.8"))~factor(nperson,
                                                                  labels=c("30persons/10items","50persons/20items"))) +
  ylim(0,1)+ 
  geom_abline(intercept = 0.8, slope=0, colour = "red")+
  labs(x="Beta0", y="Power") 
