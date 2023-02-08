
haul=get_SHAUL(sy=1980)
haul<-subset(haul, is.na(haul$BOTTOM_DEPTH)==F)
haul<-subset(haul, is.na(haul$GEAR_TEMPERATURE)==F)
haul<-subset(haul, is.na(haul$END_LONGITUDE)==F)


temp1<-aggregate(list(TEMP=haul$GEAR_TEMPERATURE),by=list(DEPTH=round(haul$BOTTOM_DEPTH,-1),YEAR=haul$YEAR),FUN=mean)


plot(haul$GEAR_TEMPERATURE[haul$YEAR==2012]~haul$BOTTOM_DEPTH[haul$YEAR==2012],col="red",pch=1,ylim=c(2,6), xlab="Bottom depth (m)", ylab="Bottom temp. (C)")
points(haul$GEAR_TEMPERATURE[haul$YEAR==2000]~haul$BOTTOM_DEPTH[haul$YEAR==2000],col="black",pch=5,cex=0.8,ylim=c(2,6))
points(temp1$TEMP[temp1$YEAR==2012]~temp1$DEPTH[temp1$YEAR==2012],type="l",col="red",lwd=2)
points(temp1$TEMP[temp1$YEAR==2000]~temp1$DEPTH[temp1$YEAR==2000],type="l",col="black",lwd=2)

text(400,5.8,"2012",pos=4)
text(400,6,"2000",pos=4)
points(400,6,pch=5,col="black",cex=0.8)
points(400,5.8,pch=1,col="red")




haul_80<-subset(haul,haul$BOTTOM_DEPTH<201&&haul$BOTTOM_DEPTH>79)

mtemp<-aggregate(list(M_TEMP=haul_80$GEAR_TEMPERATURE),by=list(YEAR=haul_80$YEAR),FUN=mean)






plot(mtemp$pol~mtemp$M_TEMP,pch=16,col="red",ylim=c(-3,2), ylab="Standardized Biomass", xlab="Mean Temp. for 80-200m")
points(mtemp$atka1~mtemp$M_TEMP,pch=17,col="blue")
text(mtemp$M_TEMP,mtemp$atka1,paste(mtemp$YEAR),cex=0.5,col="blue",pos=3)
text(mtemp$M_TEMP,mtemp$pol,paste(mtemp$YEAR),cex=0.5,col="red",pos=3)
abline(testp,lty=2,col="red")
abline(test,lty=2,col="blue")

text (4.3,-2, "Atka Mackerel",pos=4)
text (4.3,-2.2, "Pollock",pos=4)
points(4.3,-2,pch=17,col="blue")
points(4.3,-2.2,pch=16, col="red")





plot(haul$GEAR_TEMPERATURE[haul$YEAR==2012]~haul$BOTTOM_DEPTH[haul$YEAR==2012],col="red",pch=1,ylim=c(2,6), xlab="Bottom depth (m)", ylab="Bottom temp. (C)",type="n")
points(temp1$TEMP[temp1$YEAR==2012]~temp1$DEPTH[temp1$YEAR==2012],type="l",col="red",lwd=2)
points(temp1$TEMP[temp1$YEAR==2010]~temp1$DEPTH[temp1$YEAR==2010],type="l",col="blue",lwd=1)
points(temp1$TEMP[temp1$YEAR==2006]~temp1$DEPTH[temp1$YEAR==2006],type="l",col="green",lwd=1)
points(temp1$TEMP[temp1$YEAR==2004]~temp1$DEPTH[temp1$YEAR==2004],type="l",col="orange",lwd=1)
points(temp1$TEMP[temp1$YEAR==2002]~temp1$DEPTH[temp1$YEAR==2002],type="l",col="purple",lwd=1)
points(temp1$TEMP[temp1$YEAR==2000]~temp1$DEPTH[temp1$YEAR==2000],type="l",col="black",lwd=1)
points(temp1$TEMP[temp1$YEAR==1997]~temp1$DEPTH[temp1$YEAR==1997],type="l",col="turquoise",lwd=1)
points(temp1$TEMP[temp1$YEAR==1994]~temp1$DEPTH[temp1$YEAR==1994],type="l",col="brown",lwd=1)
points(temp1$TEMP[temp1$YEAR==1991]~temp1$DEPTH[temp1$YEAR==1991],type="l",col="dark green",lwd=1)





text(50,3.8,"2012",pos=4)
text(50,3.6,"2010",pos=4)
text(50,3.4,"2006",pos=4)
text(50,3.2,"2004",pos=4)
text(50,3,"2002",pos=4)
text(50,2.8,"2000",pos=4)
text(50,2.6,"1997",pos=4)
text(50,2.4,"1994",pos=4)
text(50,2.2,"1991",pos=4)



points(50,3.8,pch="-",col="red")
points(50,3.6,pch="-",col="blue")
points(50,3.4,pch="-",col="green")
points(50,3.2,pch="-",col="orange")
points(50,3,pch="-",col="purple")
points(50,2.8,pch="-",col="black")
points(50,2.6,pch="-",col="turquoise")
points(50,2.4,pch="-",col="brown")
points(50,2.2,pch="-",col="dark green")






 quantiles1<-function(x){quantile(x,probs=c(0.025,0.5,0.975))}
  t_quant=aggregate(list(temp=haul$GEAR_TEMPERATURE),by=list(haul$DEPTH),FUN=quantiles1)
  t_quant12=aggregate(list(temp=haul$GEAR_TEMPERATURE[haul$YEAR==2012]),by=list(haul$DEPTH[haul$YEAR==2012]),FUN=quantiles1)
  t_quant10=aggregate(list(temp=haul$GEAR_TEMPERATURE[haul$YEAR==2010]),by=list(haul$DEPTH[haul$YEAR==2010]),FUN=quantiles1)

  
  plot(t_quant10[,2][,2]~t_quant10[,1],type="l",col="black",ylim=c(3,7),xlim=c(0,480),xlab="Bottom depth (m)",ylab="Bottom Temp. (C)", cex.lab=1.2,cex.axis=1.2)
  points(t_quant10[,2][,1]~t_quant10[,1],type="l",lty=2)
  points(t_quant10[,2][,3]~t_quant10[,1],type="l",lty=2)
  
  points(t_quant12[,2][,1]~t_quant12[,1],type="l",lty=3,col="red")
  points(t_quant12[,2][,3]~t_quant12[,1],type="l",lty=3,col="red")
  points(t_quant12[,2][,2]~t_quant12[,1],type="l",lty=1,lwd=2,col="red")
  
  
  
  
  
  
  
  length2<-subset(length, length$stratum<700)
  
  
  
  
