# project 2: Linear mixed-effects model & Generalized linear mixed-effects model for behavior experimental analysis
## Created 1000 datasets using Monte-Carlo simulation in each 12 conditions and fitted these data sets into 4 types of model as shown below
## 1) maximal model: y ~ 1+x1 + (1+x1|j) + (1|i) 
## 2) null hypothesis of maximal model: y ~ 1 + (1+x1|j) + (1|i)
## 3) model that only has a random intercept: y ~ 1+x1 + (1|j) + (1|i), 
## 4) the null hypothesis of the third model: y ~ 1 + (1|j) + (1|i). 
