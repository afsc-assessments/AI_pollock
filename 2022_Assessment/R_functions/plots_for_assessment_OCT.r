
AFSC=odbcConnect("AFSC","sbarb","BluFsh!7890",believeNRows=FALSE)
AKFIN=odbcConnect("AKFIN","sbarbeaux","$tockmen12",believeNRows=FALSE)



library(data.table)

haul=data.table(get_SHAUL(sy=1990))
haul<-haul[!is.na(haul$BOTTOM_DEPTH)&!is.na(haul$GEAR_TEMPERATURE)&!is.na(haul$END_LONGITUDE)]

library(data.table)


#[1] "REGION"           "YEAR"             "TIME"             "CRUISE"          
# [5] "VESSEL"           "HAUL"             "END_LONGITUDE"    "END_LATITUDE"    
# [9] "GEAR"             "STRATUM"          "BOTTOM_DEPTH"     "GEAR_TEMPERATURE"
 
haul$RDEPTH<- -round(haul$BOTTOM_DEPTH,-1)

akima.li <- interp(haul$YEAR, haul$RDEPTH, haul$GEAR_TEMPERATURE, duplicate="mean",
                   yo=seq(min(haul$RDEPTH), max(haul$RDEPTH), length = 40),
                   xo=seq(min(haul$YEAR), max(haul$YEAR), length = 30))

filled.contour(akima.li,xlim=c(1990,2023),
                   panel.last=(list(abline(v=2022,lty=2,col="red"),
                   abline(v=2018,lty=2,col="red"),
                   abline(v=2016,lty=2,col="red"),
                   abline(v=2014,lty=2,col="red"),
                   abline(v=2012,lty=2,col="red"),
                   abline(v=2010,lty=2,col="red"),
                   abline(v=2006,lty=2,col="red"),
                   abline(v=2004,lty=2,col="red"),
                   abline(v=2002,lty=2,col="red"),
                   abline(v=2000,lty=2,col="red"),
                   abline(v=1997,lty=2,col="red"),
                   abline(v=1994,lty=2,col="red"),
                   abline(v=1991,lty=2,col="red"),
                   abline(h=-120,lty=3,col="blue"),
                   abline(h=-300,lty=3,col="blue"))),
                   xlab="Year",ylab="Bottom Depth (m)",key.title="Bottom Temp."
                   )

haul=get_SHAUL(sy=1990)
haul<-subset(haul, !is.na(haul$GEAR_TEMPERATURE))
haul<-subset(haul, !is.na(haul$END_LONGITUDE))


haul$long<- haul$END_LONGITUDE
haul[haul$long<0]$long<-360+haul[haul$long<0]$long
haul$RLONG<- round(haul$long)                  
akima.li <- interp(haul$YEAR, haul$RLONG, haul$GEAR_TEMPERATURE, duplicate="mean",
                   yo=seq(min(haul$RLONG), max(haul$RLONG), length = 100),
                   xo=seq(min(haul$YEAR), max(haul$YEAR), length = 30))

filled.contour(akima.li,xlim=c(1990,2023),
                   panel.last=(list(
		   abline(v=2022,lty=2,col="red"),
		   abline(v=2018,lty=2,col="red"),
                   abline(v=2016,lty=2,col="red"),
                   abline(v=2014,lty=2,col="red"),
                   abline(v=2012,lty=2,col="red"),
                   abline(v=2010,lty=2,col="red"),
                   abline(v=2006,lty=2,col="red"),
                   abline(v=2004,lty=2,col="red"),
                   abline(v=2002,lty=2,col="red"),
                   abline(v=2000,lty=2,col="red"),
                   abline(v=1997,lty=2,col="red"),
                   abline(v=1994,lty=2,col="red"),
                   abline(v=1991,lty=2,col="red"),
                   abline(v=1987,lty=2,col="red"),
                   abline(v=1983,lty=2,col="red"),
                   abline(v=1980,lty=2,col="red"))),
                   xlab="Year",ylab="E.    Longitude    W.",key.title="Bottom Temp."
                   )




haul=data.table(get_SHAUL(sy=1990))
haul<-haul[!is.na(haul$BOTTOM_DEPTH)&!is.na(haul$GEAR_TEMPERATURE)&!is.na(haul$END_LONGITUDE)]

haul$RDEPTH<- -round(haul$BOTTOM_DEPTH,-1)

akima.li <- interp(haul$YEAR, haul$RDEPTH, haul$CPUE, duplicate="mean",
                   yo=seq(min(haul$RDEPTH), max(haul$RDEPTH), length = 40),
                   xo=seq(min(haul$YEAR), max(haul$YEAR), length = 30))

                   filled.contour(akima.li,xlim=c(1990,2023),
                   panel.last=(list(
                   abline(v=2022,lty=2,col="red"),
                   abline(v=2018,lty=2,col="red"),
                   abline(v=2016,lty=2,col="red"),
                   abline(v=2014,lty=2,col="red"),
                   abline(v=2012,lty=2,col="red"),
                   abline(v=2010,lty=2,col="red"),
                   abline(v=2006,lty=2,col="red"),
                   abline(v=2004,lty=2,col="red"),
                   abline(v=2002,lty=2,col="red"),
                   abline(v=2000,lty=2,col="red"),
                   abline(v=1997,lty=2,col="red"),
                   abline(v=1994,lty=2,col="red"),
                   abline(v=1991,lty=2,col="red"),
                   abline(v=1987,lty=2,col="red"),
                   abline(v=1983,lty=2,col="red"),
                   abline(v=1980,lty=2,col="red"),
                   abline(h=-120,lty=3,col="blue"),
                   abline(h=-300,lty=3,col="blue"))),xlab="Year",ylab="Bottom Depth (m)",key.title="Bottom Temp.",
                   )

haul$RTEMP<- round(haul$GEAR_TEMPERATURE,1)

akima.li <- interp(haul$YEAR, haul$RTEMP, (haul$CPUE), duplicate="mean",
                   yo=seq(min(haul$RTEMP), max(haul$RTEMP), length = 100),
                   xo=seq(min(haul$YEAR), max(haul$YEAR), length = 20))

                   filled.contour(akima.li,xlim=c(1990,2023),
                   panel.last=(list(
                   abline(v=2022,lty=2,col="red"),
                   abline(v=2018,lty=2,col="red"),
                   abline(v=2016,lty=2,col="red"),
                   abline(v=2014,lty=2,col="red"),
                   abline(v=2012,lty=2,col="red"),
                   abline(v=2010,lty=2,col="red"),
                   abline(v=2006,lty=2,col="red"),
                   abline(v=2004,lty=2,col="red"),
                   abline(v=2002,lty=2,col="red"),
                   abline(v=2000,lty=2,col="red"),
                   abline(v=1997,lty=2,col="red"),
                   abline(v=1994,lty=2,col="red"),
                   abline(v=1991,lty=2,col="red"),
                   abline(v=1987,lty=2,col="red"),
                   abline(v=1983,lty=2,col="red"),
                   abline(v=1980,lty=2,col="red"),
                   abline(h=3,lty=3,col="blue"),
                   abline(h=5.5,lty=3,col="blue"))),xlab="Year",ylab="Bottom Temp."
                   )

haul=data.table(get_SHAUL(sy=1990))
haul<-haul[!is.na(haul$BOTTOM_DEPTH)&!is.na(haul$GEAR_TEMPERATURE)&!is.na(haul$END_LONGITUDE)&!is.na(haul$CPUE)]
                
haul$long<- haul$END_LONGITUDE
haul[haul$long<0]$long<-360+haul[haul$long<0]$long
haul$RLONG<- round(haul$long,-1) 
                 
akima.li <- interp(haul$YEAR, haul$RLONG, haul$CPUE, duplicate="mean",
                   yo=seq(min(haul$RLONG), max(haul$RLONG), length = 40),
                   xo=seq(min(haul$YEAR), max(haul$YEAR), length = 40))

filled.contour(akima.li,xlim=c(1990,2023),
                   panel.last=(list(
                   abline(v=2022,lty=2,col="red"),
                   abline(v=2018,lty=2,col="red"),
                   abline(v=2016,lty=2,col="red"),
                   abline(v=2014,lty=2,col="red"),
                   abline(v=2012,lty=2,col="red"),
                   abline(v=2010,lty=2,col="red"),
                   abline(v=2006,lty=2,col="red"),
                   abline(v=2004,lty=2,col="red"),
                   abline(v=2002,lty=2,col="red"),
                   abline(v=2000,lty=2,col="red"),
                   abline(v=1997,lty=2,col="red"),
                   abline(v=1994,lty=2,col="red"),
                   abline(v=1991,lty=2,col="red"),
                   abline(v=1987,lty=2,col="red"),
                   abline(v=1983,lty=2,col="red"),
                   abline(v=1980,lty=2,col="red"))),
                   xlab="Year",ylab="E.    Longitude    W."
                   )



data1<-Get_DATA(username="sbarb",password="BluFsh!7890",survey = c("52"),yr=c(1990:2022))
Get_TEMP(data=data1,plotT=T)




sizecomp<-paste0("SELECT AI.SIZECOMP_AREA.YEAR,
  AI.SIZECOMP_AREA.SPECIES_CODE,
  AI.SIZECOMP_AREA.REGULATORY_AREA_NAME,
  AI.SIZECOMP_AREA.LENGTH,
  AI.SIZECOMP_AREA.TOTAL
  FROM AI.SIZECOMP_AREA
  WHERE AI.SIZECOMP_AREA.SPECIES_CODE       = 21740
  AND AI.SIZECOMP_AREA.REGULATORY_AREA_NAME = 'ALEUTIANS'
  ORDER BY AI.SIZECOMP_AREA.YEAR,
  AI.SIZECOMP_AREA.LENGTH")

  Ssize=data.table(sqlQuery(AFSC,sizecomp))

x<-Ssize[,list(totals=sum(TOTAL)),by="YEAR"]
Ssize<-merge(Ssize,x,by="YEAR")
Ssize$PROP=Ssize$TOTAL/Ssize$totals

 d<-ggplot(data=Ssize,aes(x=LENGTH,y=PROP,fill="red"))
 d<-ggplot(data=Ssize,aes(x=LENGTH,y=PROP))
 d<-d+geom_bar(stat="identity",fill="blue")
 d<-d+facet_wrap(~YEAR)
 d<-d+ylab("Proportion")+xlab("Length (mm)")
 d

 agecomp<-paste0("SELECT AI.AGECOMP_TOTAL.SURVEY_YEAR,
  AI.AGECOMP_TOTAL.SPECIES_CODE,
  AI.AGECOMP_TOTAL.AGE,
  SUM(AI.AGECOMP_TOTAL.AGEPOP) AS Sum_AGEPOP
  FROM AI.AGECOMP_TOTAL
  WHERE AI.AGECOMP_TOTAL.SPECIES_CODE = 21740
  AND AI.AGECOMP_TOTAL.AGE           >= 0
  GROUP BY AI.AGECOMP_TOTAL.SURVEY_YEAR,
  AI.AGECOMP_TOTAL.SPECIES_CODE,
  AI.AGECOMP_TOTAL.AGE
  ORDER BY AI.AGECOMP_TOTAL.SURVEY_YEAR,
  AI.AGECOMP_TOTAL.AGE")

  Sage=data.table(sqlQuery(AFSC,agecomp))

  x<-Sage[,list(totals=sum(SUM_AGEPOP)),by="SURVEY_YEAR"]
Sage<-merge(Sage,x,by="SURVEY_YEAR")
Sage$PROP=Sage$SUM_AGEPOP/Sage$totals

 d<-ggplot(data=Sage,aes(x=AGE,y=PROP))
 d<-d+geom_bar(stat="identity",fill="blue")
 d<-d+facet_wrap(~SURVEY_YEAR)
 d<-d+ylab("Proportion")+xlab("Age")
 d


get_SlengthAI<-function(species=21740){
 		require(data.table)
 		sizecomp<-paste0("SELECT AI.SIZECOMP_AREA.YEAR,
  				AI.SIZECOMP_AREA.LENGTH,
  				AI.SIZECOMP_AREA.MALES,
  				AI.SIZECOMP_AREA.FEMALES,
  				AI.SIZECOMP_AREA.UNSEXED,
  				AI.SIZECOMP_AREA.TOTAL
				FROM AI.SIZECOMP_AREA
				WHERE AI.SIZECOMP_AREA.SPECIES_CODE       =", species,
				"AND AI.SIZECOMP_AREA.REGULATORY_AREA_NAME = 'ALEUTIANS'")

		Ssize=data.table(sqlQuery(AFSC,sizecomp))
		SsizeF<-Ssize[, c("MALES","UNSEXED","TOTAL"):=NULL]
		names(SsizeF)[3]<-"FREQ"
		SsizeF$SEX<-1
		Ssize=data.table(sqlQuery(AFSC,sizecomp))
		SsizeM<-Ssize[, c("FEMALES","UNSEXED","TOTAL"):=NULL]
		names(SsizeM)[3]<-"FREQ"
		SsizeM$SEX<-2
		Ssize=data.table(sqlQuery(AFSC,sizecomp))
		SsizeU<-Ssize[, c("MALES","FEMALES","TOTAL"):=NULL]
		names(SsizeU)[3]<-"FREQ"
		SsizeU$SEX<-3
		SsizeU$FREQ<-round(SsizeU$FREQ/2)
		SsizeF<-rbind(SsizeF,SsizeU)
		SsizeF$SEX<-1
		SsizeM<-rbind(SsizeM,SsizeU)
		SsizeM$SEX<-2

		Ssize<-rbind(SsizeM,SsizeF)
		Ssize<-Ssize[,list(FREQ=sum(FREQ)),by=c("YEAR","LENGTH","SEX")]
		Ssize
}



AI_DAT<-get_all_data(fyear=2018, species=21740,region=52,area="AI",MINAGE=100,filename="ai18b.dat",WRITE=FALSE) 
snage<-data.table(AI_DAT$S_NAGE)
x<-melt(data=snage,"YEAR")
x$variable<-substring(x$variable,2)
x$AGE<-as.numeric(x$variable)

d<-ggplot(data=x,aes(x=AGE,y=value))
 d<-d+geom_bar(stat="identity",fill="blue")
 d<-d+facet_wrap(~YEAR)
 d<-d+ylab("Proportion")+xlab("Age")
 d




data<-data.table(read.csv())
data$Length<-data$LENGTH/10

dataBSM<-data1[CRUISE==201601]
 svTypical <- vbStarts(Length~AGE,data=dataBSM)
 #growthModelSim("vbTypical",Length~Final_Age,data=dataBSM)
vbTypical <- Length~Linf*(1-exp(-K*(AGE-t0)))
fitTypical <- nls(vbTypical,data=dataBSM,start=svTypical)
fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",main="")
bootTypical <- nlsBoot(fitTypical,niter=200) 
ages2plot<-data.frame(AGE=c(0:16))
ages2plot$pred<-predict(fitTypical,newdata=ages2plot)

 #fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",xlim=range(ages2plot),main="",ylim=c(0,1200))
plot(dataBSM$Length~dataBSM$AGE,xlab="Age",ylab="Total Length (mm)",xlim=range(ages2plot$AGE),main="",ylim=c(0,120))
points(ages2plot$pred~ages2plot$AGE,type="l",col="red")

ests <- bootTypical$coefboot
LCI <- UCI <- numeric(nrow(ages2plot))
 
for (i in 1:nrow(ages2plot)) {
	pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(ages2plot$AGE[i]-ests[,"t0"])))
	LCI[i] <- quantile(pv,0.025)
	UCI[i] <- quantile(pv,0.975)
	}
 
 lines(UCI~ages2plot$AGE,type="l",col="blue",lwd=2,lty=2)
 lines(LCI~ages2plot$AGE,type="l",col="blue",lwd=2,lty=2)


