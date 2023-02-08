

Length<-subset(Length,Length$STRATUM<700)
Length$LENGTH<-Length$LENGTH/10
L1<-data.frame(aggregate(list(FREQ=Length$FREQUENCY),by=list(YEAR=Length$YEAR,LENGTH=Length$LENGTH),FUN=sum))
L2<-expand.dft(L1,freq="FREQ")


Years=c(2002,2004,2006,2010,2012,2014)

par(mfrow=c(2,3))
for(i in Years){
x<-subset(L2,L2$YEAR==i)
hist(x$LENGTH,breaks=seq(0,90,1),col="BROWN",ylab="Proportion",ylim=c(0,0.06),freq=F,xlab="Fork length (cm)",main=i)
}
 



  x1<-subset(data,data$NMFS_AREA==541&data$LATDD_END<5300)
  x2<-subset(data,data$NMFS_AREA==542&data$LATDD_END<5230)
  x3<-subset(data,data$NMFS_AREA==543&data$LATDD_END<5330)
  x<-rbind(x1,x2,x3)




  lm2<-function(data,year){
  data2<-subset(data,data$YEAR==year)
  mod<-lm(log(data2$WEIGHT)~log(data2$LENGTH))
  x<-summary(mod)$coef
  x
  }

( vb1 <- vbFuns() )              # typical parameterization
ages <- 0:15
plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)
data$LENGTH<-data$LENGTH/10


 fit10 <- nls(LENGTH~vb1(AGE,Linf,K,t0),data=subset(data,!is.na(data$AGE)&data$YEAR==2010),start=vbStarts(LENGTH~AGE,data=subset(data,!is.na(data$AGE)&data$YEAR==2010)))
summary(fit1,correlation=TRUE)


 plot(LENGTH~AGE,data=data,pch=19,col="gray80")
curve(vb1(x,Linf=coef(fit10)[1],K=coef(fit10)[2],t0=coef(fit10)[3]),from=0,to=20,
 col="red",lwd=2,add=TRUE)
 curve(vb1(x,Linf=coef(fit12)[1],K=coef(fit12)[2],t0=coef(fit12)[3]),from=0,to=20,
 col="purple",lwd=2,add=TRUE)
 curve(vb1(x,Linf=coef(fit06)[1],K=coef(fit06)[2],t0=coef(fit06)[3]),from=0,to=20,
 col="green",lwd=2,add=TRUE)
 curve(vb1(x,Linf=coef(fit04)[1],K=coef(fit04)[2],t0=coef(fit04)[3]),from=0,to=20,
 col="blue",lwd=2,add=TRUE)





find_EFFN<-function(data=test){
  
  x<-ncol(data$phat_ind_1)
  Ind_N<-array(dim=nrow(data$phat_ind_1))
  
  for(i in 1:nrow(data$phat_ind_1)){
    Ind_N[i]<-sum((1-data$phat_ind_1[i,2:x])*data$phat_ind_1[i,2:x])/(sum((data$phat_ind_1[i,2:x]-data$pobs_ind_1[i,2:x])^2))
  }

  Fsh_N<-array(dim=nrow(data$phat_fsh_1))
  for(i in 1: nrow(data$phat_fsh_1)){
    Fsh_N[i]<-sum((1-data$phat_fsh_1[i,2:x])*data$phat_fsh_1[i,2:x])/(sum((data$phat_fsh_1[i,2:x]-data$pobs_fsh_1[i,2:x])^2))
  }
  
  require(psych)
  
  Ot<-vector("list")
  Ot$FN<-Fsh_N
  Ot$EFFN_F<-mean(Fsh_N)
  Ot$EFFN_HF<-harmonic.mean(as.numeric(Fsh_N))
  Ot$IN<-Ind_N
  Ot$EffN_I<-mean(Ind_N)
  Ot$EffN_HI<-harmonic.mean(as.numeric(Ind_N))
  
  Ot$RMSE<-sqrt(mean(log(data$Obs_Survey_1[,2]/data$Obs_Survey_1[,3])^2))
  Ot$Ind_RMSE<-sqrt(mean((data$phat_ind_1[,3:16]-data$pobs_ind_1[,3:16])^2))
  Ot$Fsh_RMSE<-sqrt(mean((data$phat_fsh_1[,3:16]-data$pobs_fsh_1[,3:16])^2))


  Ot
}












  Age_AI=get_Sage(region=52,species=21740,sy=1978)
  Age_EBS=get_Sage(region=98,species=21740,sy=1978)
  Age_GOA=get_Sage(region=47,species=21740,sy=1978)
  
  Age_AI<-subset(Age_AI,Age_AI$YEAR==2012)
  Age_GOA<-subset(Age_GOA,Age_GOA$YEAR==2013)
  Age_EBS<-subset(Age_EBS,Age_EBS$YEAR==2013)

  Age_AI<-subset(Age_AI,!is.na(Age_AI$AGE))
  Age_GOA<-subset(Age_GOA,!is.na(Age_GOA$AGE))
  Age_EBS<-subset(Age_EBS,!is.na(Age_EBS$AGE))

  Age_AI<-subset(Age_AI,Age_AI$END_LONGITUDE > 0 | Age_AI$END_LONGITUDE < -170)
  

 x_AI<-max(Age_AI$AGE)
  x_EBS<-max(Age_EBS$AGE)-1+0.3
   x_GOA<-max(Age_GOA$AGE)-3.3

boxplot(LENGTH/10~AGE,data=Age_AI,col="red",boxwex=0.25,ylim=c(0,100),ylab="Fork length (cm)",xlab="Age")
boxplot(LENGTH/10~AGE,data=Age_EBS,add=T,boxwex=0.25,at=seq(1.3,x_EBS,1),col="gray80",xaxt="n")
boxplot(LENGTH/10~AGE,data=Age_GOA,add=T,boxwex=0.25,at=seq(0.7,x_GOA,1),col="blue",xaxt="n")
 



Age_AI=get_Sage(region=52,species=21740,sy=1978)
  Age_EBS=get_Sage(region=98,species=21740,sy=1978)
  Age_GOA=get_Sage(region=47,species=21740,sy=1978)
  
  Age_AI<-subset(Age_AI,!is.na(Age_AI$AGE))
  Age_GOA<-subset(Age_GOA,!is.na(Age_GOA$AGE))
  Age_EBS<-subset(Age_EBS,!is.na(Age_EBS$AGE))

  Age_AI<-subset(Age_AI,Age_AI$END_LONGITUDE > 0 | Age_AI$END_LONGITUDE < -170)
  

  
plot_means<-function(data=Age_AI,mn="AI Pollock"){
  rgb.palette <- colorRampPalette(c("yellow","gold","goldenrod2","brown"),space = "rgb")
  x<-aggregate(list(LENGTH=data$LENGTH),by=list(AGE=data$AGE,YEAR=data$YEAR),FUN=mean)
  years<-sort(unique(x$YEAR))
  plot(LENGTH/10~AGE,data=subset(x,x$YEAR==min(years)),type="l",ylim=c(0,90),xlim=c(0,25),main=mn,ylab="Fork length (cm)",xlab="Age")
 
  for(j in 1:length(years)){
    points(LENGTH/10~AGE,data=subset(x,x$YEAR==years[j]),type="l",col=rgb.palette(length(years))[j])
  }
}
