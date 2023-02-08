##updated August 13, 2015

require(PBSadmb)

#C:\Working Folder for DOCS\2018_Assessments\AI_pollock_18\AI_Pollock\R_functions\Results
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/R_functions/Results")
#setwd("D://_mymods/AMAK/examples/AI_Pollock/R_functions/Results")
source("ADfunctions.r")


#setwd("D://_mymods/AMAK/examples/AI_Pollock")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model1.0")
test1<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model2.0")
test2<-readList("For_R.rep")
#setwd("C:/Working Folder for DOCS/2018_Assessments/AI_pollock_18/AI_Pollock/Model3")
#test3<-readList("For_R.rep")

## catch at age for fishery and survey with prominent year classes
pdf("AI22_M1.pdf",height=8,width=8)
p.bub.catch(test1)
 points(c(1989,2005),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2000,2016),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(1978,1994),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2006,2022),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2012,2028),c(0,16),type="l",lty=3,col="red",lwd=2)


p.bub.catch(test1,typ="s")
 points(c(1989,2005),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2000,2016),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(1978,1994),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2006,2022),c(0,16),type="l",lty=3,col="red",lwd=2)
 points(c(2012,2028),c(0,16),type="l",lty=3,col="red",lwd=2)


## plot index fit.
 p.sur.stk.comp(dat1=test1,dat2=test2,yl=2022)

# show projections
plt_proj(test1)

# show fit to catch biomass
CatchFit(test1) 

# show spawning biomass relative to population with no fishing
spwn_ratio(test1, ly=2022) 

# example of writing multiple plots to pdf file:

AgeFits(test1,case_label="2022 Assessment - All")
AgeFitsSrv(test1, case_label="2022 Assessment - All")
# sample sizes
 p.eff.n.com(test1,test2)
 p.eff.n.com(test1,test2,typ="F")

# p.eff.n(test1,typ="F")
# p.eff.n(test1,typ="S")
 # Residuals
 cont.f.age.res(test2)
 cont.f.age.res(test1,typ="F",nl=35)
 
 bp.f.age.res(test1,siz=3)
 bp.f.age.res(test1,typ="F",siz=3)

p.biom.pol(test1, n.mod=1)
p.biom.pol(test1,typ="TB",n.mod=1)
 
p.catch.fit(test1) 
 
 cont.f.mort2(test1,siz=0.005,typ="CB",ly=2022)
 cont.n.Cbiom(test1,siz=100,typ="B",ly=2022)

c.select.b(test1,typ2="CB",siz=0.005,ly=2022) 
c.select.b2(test1,siz=100,typ2="B",ly=2022) 
 

## need to define biom$Fmsy and biom$Fabc

biom=data.frame(Model=1)
biom$SB0=174.218
biom$Fabc=0.305473
biom$Fmsy=0.37995
biom$SB35=biom$SB0*0.35
biom$SB40=biom$SB0*0.40
biom$SB23 = 78.628
biom$SB24= 77.3505
biom$F23=0.0104
biom$F24=0.0263


nr=length(test1$F_fsh_1[,3])

BoverBmsy=c(test1$SSB[,2][test1$SSB[,1]>1977&test1$SSB[,1]<2023],biom$SB23,biom$SB24)/biom$SB35
FoverFmsy=c(test1$F_fsh_1[1:(nr),3],biom$F23,biom$F24)/biom$Fabc
 
plot.phase.plane(SSB0=biom$SB0,Fabc=biom$Fabc,Fmsy=biom$Fmsy,BoverBmsy=BoverBmsy,FoverFmsy=FoverFmsy,xlim=c(0,5),ylim=c(0,1.5),header="AI pollock",bw.mult=1,jitter.fac=0,eyr=2022)
 
 
## selectivity for fishery and survey for selected years
select_years(test1,Syrs=c(2014),Fyrs=c(1978,1990,1999,2022)) 
## historic biomass estimates
old_biom(test1,lyr=2022)
old_biom(test1,typ="sd",lyr=2022) 
 
 
 # another example of writing multiple plots to pdf file:

#pdf("selectivity.pdf",width=9, height=7)
#Mntns(mod_All,"Model All")
#Mntns(mod_1991,"Model 1991")
#Mntns(mod_Cheat,"Model Cheat")
#dev.off()

#Mntns_srv(mod_All,"Model 1c")

# Stock recruitment curve
 p.stock.rec(test1)
# recruitment hist w/ errors
 p.rec.hist(test1,ylab="Age 1 recruitment",fy=1978,ly=2022)

# Fishing mortality 
 p.full.f(test1)


 
# spawning biomass and last year's estimates 
 p.biom.pol(test2,test1,2,typ="TB")
points(c(1200,1200)~c(2008,2010),type="l",lty=2,col="dark green",lwd=2)
points(c(1300,1300)~c(2008,2010),type="l",lty=1,col="dark blue",lwd=2)
text(2010,1300,"Model 15.1",pos=4,cex=1.5)
text(2010,1200,"Model 15.2",pos=4,cex=1.5)


p.biom.pol(test2,test1,2,typ="SSB")
points(c(400,400)~c(2008,2010),type="l",lty=2,col="dark green",lwd=2)
points(c(450,450)~c(2008,2010),type="l",lty=1,col="dark blue",lwd=2)
text(2010,450,"Model 15.1",pos=4,cex=1.5)
text(2010,400,"Model 15.2",pos=4,cex=1.5)





 #lines(test3$TotBiom[,1],test3$TotBiom[,2],lty=3,lw=3,col="red")
 #lines(test3$TotBiom[,1],test3$TotBiom[,4],lty=3,lw=1,col="salmon")
 #lines(test3$TotBiom[,1],test3$TotBiom[,5],lty=3,lw=1,col="salmon")
 
text(2010,1300,"Model 1.0",pos=4,cex=2)
text(2010,1250,"Model 2.0",pos=4,cex=2)
#text(2010,1200,"Model 3",pos=4,cex=2)
points(c(2008,2010),c(1300,1300),type="l",lwd=2,col="dark green")
points(c(2008,2010),c(1250,1250),type="l",lty=2,lwd=2,col="dark blue")
#points(c(2008,2010),c(1200,1200),type="l",lwd=2,lty=3,col="red")



p.biom.pol(test2,test1,2)
 #lines(test3$SSB[,1],test3$SSB[,2],lty=3,lw=3,col="red")
 #lines(test3$SSB[,1],test3$SSB[,4],lty=3,lw=1,col="salmon")
 #lines(test3$SSB[,1],test3$SSB[,5],lty=3,lw=1,col="salmon")
 
text(2010,450,"Model 1",pos=4,cex=2)
text(2010,425,"Model 2",pos=4,cex=2)
#text(2010,400,"Model 3",pos=4,cex=2)
points(c(2008,2010),c(450,450),type="l",lwd=2,col="dark green")
points(c(2008,2010),c(425,425),type="l",lty=2,lwd=2,col="dark blue ")
#points(c(2008,2010),c(400,400),type="l",lwd=2,lty=3,col="red")






# Survey fit
 p.sur.stk(test1,f=1)

# Rec
 p.biom.stk(test1,typ="R")

# Numbers at age
 p.bub.age(test1,siz=0.1)

