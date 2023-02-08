

p.sur.stk(test2,yf=1978,yl=2013)
cont.f.mort(test2)

p.bub.age(test2,siz=0.1)
p.bub.age(test2,"CB",siz=0.01)
p.bub.age(test2,"B",siz=100)


cont.f.mort2(test2,siz=0.1)
cont.f.mort2(test2,"CB",siz=0.01)
cont.f.mort2(test2,"B",siz=100)

cont.f.age.res(test2,nl=40)
cont.f.age.res(test2,typ="F",nl=45)

p.biom.stk(test2)
p.biom.stk(test2,"TB")
p.biom.stk(test2,"R")

p.eff.n(test2,"S")
p.eff.n(test2,"F")

p.age.fit(test2,typ="F",f=1,lage=2,hage=15,fy=1978,ly=1985)
p.age.fit(test2,typ="F",f=1,lage=2,hage=15,fy=1986,ly=1993)
p.age.fit(test2,typ="F",f=1,lage=2,hage=15,fy=1994,ly=1995)

p.age.fit(test2,typ="S",f=1,lage=2,hage=15,fy=1978,ly=1987)
p.age.fit(test2,typ="S",f=1,lage=2,hage=15,fy=1988,ly=1990)

p.catch.fit(test2)

c.select.b(test2)
c.select.b(test2,typ="S")
c.select.b(test2,typ2="CB",siz=0.01)
c.select.b(test2,typ="S",typ2="CB",siz=0.01)
c.select.b(test2,typ2="B",siz=100)
c.select.b(test2,typ="S",typ2="B",siz=100)

p.stock.rec(test2)
p.rec.hist(test2)
p.full.f(test2)
p.survey.curve(test2)

p.biom.pol(test2,"TB")
p.biom.pol(test2,typ="TB")
p.biom.pol(test2,typ="R")



sel.age.mountain(test2,typ="F")
sel.age.mountain(test2,typ="S")

AgeFits(test2)
AgeFitsSrv(test2)

spwn_ratio(test2)

plot_comp_old()

for(i in 1:7)
  plot_proj(i)
  }
  
## Phase plane plots  
  
x<-(test2$SSB[1:49,2][test2$SSB[1:49,1]>1977]/biom$SB35)
##plot(test$F_fsh_1[,2]/Fofl~x,type="l",ylim=c(0,1.5),xlim=c(0,2),)
plot.phase.plane(x,test$F_fsh_1[,3]/Fofl,xlim=c(0,5),ylim=c(0,1.5))

plot.phase.plane(SSB0=biom$SB0,Fabc=biom$Fabc,BoverBmsy=test2$SSB[,2][test2$SSB[,1]>1977&test2$SSB[,1]<2014]/biom$SB35,Fmsy=biom$Fmsy,FoverFmsy=test2$F_fsh_1[,3]/biom$Fmsy,xlim=c(0,5),ylim=c(0,1.5))


 
## without fishing analys
library(Hmisc)
errbar(test1$TotBiom_NoFish[,1],test1$TotBiom_NoFish[,2],yminus=test1$TotBiom_NoFish[,4],yplus=test1$TotBiom_NoFish[,5],ylim=c(0,max(test1$TotBiom_NoFish[,5])),ylab="Total age 2+ biomass (kt)", xlab="Year",cex.lab=1.5,cex.axis=1.5)
#errbar(test1$TotBiom[,1],test1$TotBiom[,2],yminus=test1$TotBiom[,4],yplus=test1$TotBiom[,5],col="red",ylim=c(0,max(test1$TotBiom[,5])),add=T)
text(2000,max(test1$TotBiom_NoFish[,5])-50,"Total biomass w/o fishing",pos=4) 
text(2000,max(test1$TotBiom_NoFish[,5])-150,"Total biomass",pos=4)
points(2000,max(test1$TotBiom_NoFish[,5])-50,pch=16)
points(2000,max(test1$TotBiom_NoFish[,5])-150,pch=16,col="red")


library(Hmisc)
errbar(test1$SSB_NoFish[,1],test1$SSB_NoFish[,2],yminus=test1$SSB_NoFish[,4],yplus=test1$SSB_NoFish[,5],ylim=c(0,max(test1$SSB_NoFish[,5])),ylab="Spawning Biomass/Spawning biomass w/o fishing", xlab="Year",cex.lab=1.5,cex.axis=1.5)
errbar(test1$SSB[,1],test1$SSB[,2],yminus=test1$SSB[,4],yplus=test1$SSB[,5],col="red",ylim=c(0,max(test1$SSB[,5])),add=T)
text(2000,max(test1$SSB_NoFish[,5])-50,"Female spawning biomass w/o fishing",pos=4) 
text(2000,max(test1$SSB_NoFish[,5])-150,"Female spawning biomass",pos=4)
points(2000,max(test1$SSB_NoFish[,5])-50,pch=16)
points(2000,max(test1$SSB_NoFish[,5])-150,pch=16,col="red")








cont.CB_TB(test2,mod="1",f=1,f1=1,lage=2,hage=15,fy=1978,ly=2011,siz=100,cl="BW")
cont.f.mort2(test2,"CB",siz=0.01)
 
