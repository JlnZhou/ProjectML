
rm(list=objects())
###############packages
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
library(tibble)
library(tidyverse)
###############Import des donnees
#setwd("/Users/yannig/Documents/Enseignement/2017-2018/M2statML/TP/")
#C:\Enseignement\2015-2016\Projet Data Mining\TP\Regression
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data0<-read_delim("../Data/meteo/H1700010_bind.csv", delim="," )


###############evaluation criteria
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}


####################################################
##############qq graphiques pour l'analyse descriptive
####################################################
attach(data0)
data0 <- na.omit(data0) 


plot(Q,type='l')
plot(Date, Q,pch=16,cex=0.5)

plot(Prec4, Q,pch=16,cex=0.5)
formule <- paste0("s(",colnames(data0)[c(-1,-2,-3)], ', k=3, bs="cr")')
formule <- paste(formule, collapse = "+")
formule <- as.formula(paste0("Q~",formule))
g0 <- gam(formule, data=data0)

summary(g0)
plot(g0)
plot(data0$Temp1, g0$residuals, pch=16)

formule <- paste0("s(",colnames(data0)[c(-1,-2,-3)], ', k=3, bs="cr")')
formule <- paste(formule, collapse = "+")
formule <- as.formula(paste0("g0$residuals~",formule))
g1 <- gam(formule,data=data0)
summary(g1)

# 
# fit0 <- getViz(g0, nsim = 50)
# plot(sm(fit0, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()
# ( check1D(fit0, "Temp") + l_gridCheck1D(gridFun = mean, stand = "sc", n=100) )$ggObj

formule <- paste0("s(",colnames(data0)[c(-1,-2,-3)], ', k=10, bs="cr")')
formule <- paste(formule, collapse = "+")
formule <- as.formula(paste0("Q~",formule))
g1 <- gam(formule, data=data0)
summary(g1)

sqrt(g0$gcv.ubre)
sqrt(g1$gcv.ubre)

fit1 <- getViz(g1, nsim = 50)
summary(g1)
#plot(sm(fit1, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()
#( check1D(fit1, "Temp") + l_gridCheck1D(gridFun = mean, stand = "sc", n=100) )$ggObj


############################################################
############block Cross Validation for choosing k
############################################################
univ<-function(k, block)
{
  g<- gam(Q~s(Temp, k=k, bs="cr"), data=data0[-block,])
  #g<- gam(Q~s(Temp, k=k, bs="cr") + s(Time, k=3) + s(NumWeek, k=20), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
}

Nblock<-10
borne_block<-seq(1, nrow(data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))

K<-c(3:20)
rmseK<-lapply(K, function(k){lapply(block_list, univ,k=k)%>%unlist%>%rmse} )
plot(K, rmseK, type='b', pch=20)

################################################################################
############block Cross Validation for choosing k with different spline basis
################################################################################
univ<-function(k, block, bs)
{
  g<- gam(Q~s(Temp, k=k, bs=bs), data=data0[-block,])
  #g<- gam(Q~s(Temp, k=k, bs=bs) + s(Time, k=3), data=data0[-block,])
  #g<- gam(Q~s(Temp, k=k, bs=bs) + s(Time, k=3) + s(NumWeek, k=20), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
}

K<-c(4:20)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%rmse} )

col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "ps", "cs"), pch=20, ncol=2, bty='n')


gcr <- gam(Q~s(Temp, k=5, bs="cr"), data=data0)
gcr.fit <- predict(gcr, newdata=data0, type="terms")

gtp <- gam(Q~s(Temp, k=5, bs="tp"), data=data0)
gtp.fit <- predict(gtp, newdata=data0, type="terms")

gps <- gam(Q~s(Temp, k=5, bs="ps"), data=data0)
gps.fit <- predict(gps, newdata=data0, type="terms")

gad <- gam(Q~s(Temp, k=10, bs="ad", m=5, xt=list(bs="cr")), data=data0)
gad.fit <- predict(gad, newdata=data0, type="terms")


par(mfrow=c(1,1))
o<-order(data0$Temp)
plot(data0$Temp, data0$Q-gcr$coef[1], pch=16, cex=0.5)
lines(data0$Temp[o], gcr.fit[o], col=col[1])
lines(data0$Temp[o], gtp.fit[o], col=col[2])
lines(data0$Temp[o], gps.fit[o], col=col[3])
lines(data0$Temp[o], gad.fit[o], col=col[4])
legend("top", col=col[1:4], c("cr", "tp", "ps", "ad"), pch=20, ncol=2, bty='n')



##########################################################################################################################
#####plot of each splines scaled by their coef (colors) and the estimated effect (black) for cubic regression basis
##########################################################################################################################
gcr.mat <- predict(gcr, newdata=data0, type="lpmatrix")
gcr.mat%*%gcr$coefficients-gcr$fitted
o<-order(data0$Temp)
col<-piratepal("basel", length.out = 5)
plot(data0$Temp, data0$Q-gcr$coef[1], pch=16, cex=0.5)
for(i in c(2:ncol(gcr.mat)))
{
  lines(data0$Temp[o], gcr.mat[o,i]*gcr$coefficients[i], col=col[i-1])
}
lines(data0$Temp[o], gcr.fit[o], col="black", lwd=2)



##########################################################################################
#####plot of the different splines basis
##########################################################################################
par(mfrow=c(2,2))
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  print(dim(g.mat))
  plot(data0$Temp[o],g.mat[o,2] , pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col=col[1], type='l', ylim=range(g.mat[,-1]))
  for(j in c(3:ncol(g.mat)))
  {
    lines(data0$Temp[o], g.mat[o,j], col=col[j-1])
  }
}


#############en choisissant les noeuds
# gcr <- gam(Q~s(Temp, k=3, bs="cr"), data=data0, knots=list(Temp=c(5,10,20)))
# g.mat <- predict(gcr, newdata=data0, type="lpmatrix")
# 
# plot(data0$Temp[o],g.mat[o,2] , pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col=col[1], type='l', ylim=range(g.mat[,-1]))
# for(j in c(3:ncol(g.mat)))
# {
#   lines(data0$Temp[o], g.mat[o,j], col=col[j-1])
# }
# abline(v=c(5,10,20))

##########################################################################################
#####plot of each splines scaled by their coef (colors) and the estimated effect (black)
##########################################################################################

par(mfrow=c(2,2))
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  plot(data0$Temp, data0$Q-listMod[[i]]$coef[1], pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col='grey')
  for(j in c(2:ncol(g.mat)))
  {
    lines(data0$Temp[o], g.mat[o,j]*listMod[[i]]$coefficients[j], col=col[j-1])
  }
  g.fit <- predict(listMod[[i]], newdata=data0, type="terms")
  lines(data0$Temp[o], g.fit[o], col="black", lwd=2)
}


#################################################################################
############NumWeek effect
#################################################################################
univ<-function(k, block, bs)
{
  g<- gam(Q~s(NumWeek, k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
}

K<-c(4:40)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%rmse} )

par(mfrow=c(1,1))
col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "ps", "cs"), pch=20, ncol=2, bty='n')

g<- gam(Q~s(NumWeek, k=30, bs='tp'), data=data0)
summary(g)


Kopt<-10
gcr <- gam(Q~s(NumWeek, k=Kopt, bs="cr"), data=data0)
gcr.fit <- predict(gcr, newdata=data0, type="terms")

gtp <- gam(Q~s(NumWeek, k=Kopt, bs="tp"), data=data0)
gtp.fit <- predict(gtp, newdata=data0, type="terms")

gps <- gam(Q~s(NumWeek, k=Kopt, bs="ps"), data=data0)
gps.fit <- predict(gps, newdata=data0, type="terms")

gad <- gam(Q~s(NumWeek, k=Kopt, bs="cr", m=10, xt=list(bs="cr")), data=data0)
gad.fit <- predict(gad, newdata=data0, type="terms")

par(mfrow=c(2,2))
o<-order(data0$NumWeek)
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  plot(data0$NumWeek, data0$Q-listMod[[i]]$coef[1], pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col='grey')
  for(j in c(2:ncol(g.mat)))
  {
    lines(data0$NumWeek[o], g.mat[o,j]*listMod[[i]]$coefficients[j], col=col[j-1])
  }
  g.fit <- predict(listMod[[i]], newdata=data0, type="terms")
  lines(data0$NumWeek[o], g.fit[o], col="black", lwd=2)
}


#################################################################################
############Trend effect
#################################################################################
univ<-function(k, block, bs)
{
  g<- gam(Q~s(Time, k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
}

K<-c(3:20)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )

par(mfrow=c(1,1))
col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
legend("top", col=col, c("cr", "tp"), pch=20, ncol=2, bty='n')

g<- gam(Q~s(Time, k=4, bs='cr'), data=data0)

summary(g)
plot(g)

#################################################################################
############3 variables model
#################################################################################
gam1<-gam(Q~s(Time, k=3)+s(NumWeek, k=30,bs='tp')+s(Temp,k=5))
summary(gam1)


#####temp
univ<-function(k, block, bs)
{
  g<- gam(Q~s(Time,k=5)+s(NumWeek,k=30,bs='tp')+s(Temp,k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
} 
K<-c(3:15)
rmseK<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
col<-piratepal("basel", length.out = 9)
plot(K, rmseK, type='b', pch=20, ylim= range(rmseK), col=col[1])

#####NumWeek
univ<-function(k, block, bs)
{
  g<- gam(Q~s(Time,k=5)+s(NumWeek,k=k,bs=bs)+s(Temp,k=5, bs='tp'), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$Q-forecast)
} 
K<-c(10:40)
rmseK<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
col<-piratepal("basel", length.out = 9)
plot(K, rmseK, type='b', pch=20, ylim= range(rmseK), col=col[1])

