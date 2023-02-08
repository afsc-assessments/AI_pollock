
require(PBSadmb)

setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/R_functions/Results")
source("ADfunctions.r")

setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model1.0")
test2<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_1")
testM_1<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_2")
testM_2<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_3")
testM_3<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_4")
testM_4<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_5")
testM_5<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_6")
testM_6<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_7")
testM_7<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_8")
testM_8<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_9")
testM_9<-readList("For_R.rep")
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/Model_1.0_retro/M_10")
testM_10<-readList("For_R.rep")

#============================================================================================
#============= Get Working Directories
#============================================================================================

pathD<-getwd()

#============================================================================================
#============= Read in plot data
#============================================================================================

N_SSB<-nrow(test2$SSB)
sab<-matrix(ncol=13,nrow=N_SSB)
sab[,12]<-test2$SSB[,2]
sab[,13]<-test2$SSB[,3]
sab[,1]<-test2$SSB[,1]
j<-c(11:2)
for(i in 1:10){
	y<-paste("testM_",i,sep="")
	sab[1:(N_SSB-i),(j[i])]<-get(y)$SSB[1:(N_SSB-i),2]
}


SAB=data.frame(sab)
names(SAB)<-c("Year",paste("M_",seq(10,0,-1)),"M_0SD")

#============================================================================================
#============= Plot it up
#============================================================================================

#colors<-rainbow(10,start=0.4)
#ramp=colorRampPalette(c("indianred4","ivory2") )
ramp=colorRampPalette(c("dark blue","light blue") )
colors= ramp(10)
layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.3,1,1,0.3))

par(mar=c(0.1,8,0.1,0.5))
l1<- length(SAB[1,])-1
LCI<- SAB[,l1]-(1.96*SAB[,length(SAB[1,])])
UCI<- SAB[,l1]+(1.96*SAB[,length(SAB[1,])])
plot(SAB$Year,SAB[,l1],type="l",lwd=3,xaxt="n",las=2,xlab="",ylab="",cex.axis=1.5,ylim=c(0,450),lty=3)
points(SAB$Year,LCI,type="l",lty=3,col="red",lwd=2)
points(SAB$Year,UCI,type="l",lty=3,col="red",lwd=2)

text(2005,450,"2022",pos=4,col="black")
for(i in 1:10) {
lines(SAB$Year,SAB[,(l1-i)],lwd=1.75,col=colors[i])
}
k=seq(2021,2012,-1)
for(j in 1:10){
text(2005,(450-j*25),paste(k[j]),pos=4,col=colors[j])
}


mtext("Spawning biomass (kt)",side=2,line=6,cex=1.)

plot(SAB$Year,100*(SAB[,(l1)]-SAB[,(l1)])/SAB[,(l1)],type="l",lwd=3,xaxt="n",las=2,xlab="",ylab="",cex.axis=1.4,ylim=c(-50,50),lty=2)
points(SAB$Year,100*(UCI-SAB[,(l1)])/SAB[,(l1)],type="l",lty=3,col="red",lwd=2 )
points(SAB$Year,100*(LCI-SAB[,(l1)])/SAB[,(l1)],type="l",lty=3,col="red",lwd=2 )
for(i in 1:10) {
lines(SAB$Year,100*(SAB[,(l1-i)]-SAB[,l1])/SAB[,l1],lwd=1.75,col=colors[i]) }

mtext("Percent differences",side=2,line=6,cex=1.)
mtext("from terminal year",side=2,line=4.5,cex=1.)

for(y in seq(1,length(SAB$Year),2)){
axis(side=1,at=SAB$Year[y],SAB$Year[y],cex.axis=1.2,las=2)}

mtext("Year",side=1,line=4.6,cex=1.)


#============================================================================================
#=====Mohn's Rho
#============================================================================================

x<-array(dim=10)
end<-nrow(SSB)
for(i in 1:10){
	x[i]<-(SAB[(end-i),(12-i)]-SAB[(end-i),12])/SAB[(end-i),12]
}
rho=sum(x)/10

#============================================================================================
#=====Wood's Hole Rho
#============================================================================================
x<-matrix(ncol=10,nrow=(end-1))
for(i in 1:10){
	x[1:(end-i),i]<-(SAB[1:(end-i),(12-i)]-SAB[1:(end-i),12])/SAB[1:(end-i),12]
}
y<-array(dim=10)

for( i in 1:10){
	y[i]<-sum(x[,i][!is.na(x[,i])]/length(x[,i][!is.na(x[,i])]))
}

WH_rho<-sum(y)/10

#============================================================================================
#=====REtrospectic RMSE
#============================================================================================

x<-matrix(ncol=10,nrow=(end-1))
for(i in 1:10){
	x[1:(end-i),i]<-(log(SAB[1:(end-i),(12-i)])-log(SAB[1:(end-i),12]))^2
}

RMSE=sqrt(sum(x[!is.na(x)])/length(x[!is.na(x)]))


 