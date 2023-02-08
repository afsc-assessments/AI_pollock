## updated November 2, 2016

library(KernSmooth)


source("mntns.r")
source("indexfit.r")
source("spwn_rat.r")
source("proj.r")
source("agefits.r")
##source("common.r")


# NOTE: the functions below are a hodge podge of various routines, they may not have been tested
  ##plot survey fit with all suveys
  p.sur.stk<-function(dat,f=1,ylab="Biomass index (1000 t)", yf=1978,yl=2018,YLIM=c(0,650))
  {
  attach(dat)
  require(plotrix)
  x<-paste("Obs_Survey_",f,sep="")
  Data<-get(x)
  #d1<-subset(Data,Data[,2]>0)
  d1<-Data ##Surv_Biom
  
  windows(8,8)
  plotCI(d1[4:13,1],d1[4:13,2],2*d1[4:13,4],
  main=Index_names[f],
  ylim=YLIM,xlim=c(yf,yl),pch=19,
  ylab=ylab,
  xlab="Year",col="green",scol="black",lab=c(10,10,7))
  points(Data[4:13,2]~Data[4:13,1],pch=16,col="blue")
  points(Data[1:3,2]~Data[1:3,1],pch=16,col="green")

  points(Data[,3]~Data[,1],type="l",col="red",lwd=2)
  text(min(Data[,1])+2,(max(d1[,2])*1.6),"Survey Point Estimates Not Fit",pos=4,cex=0.8)
   text(min(Data[,1])+2,(max(d1[,2])*1.5),"Survey Point Estimates Fit",pos=4,cex=0.8)
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"Model Fit",pos=4,cex=0.8)
  points(min(Data[,1])+2,(max(d1[,2])*1.6),pch=16,col="green")
 
  points(min(Data[,1])+2,(max(d1[,2])*1.5),pch=19,col="blue")
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"--",cex=1.5,col="red")
  detach(dat)

  }
##plot survey fit
#p.sur.stk<-function(dat,f=1,ylab="Index", yf=1978,yl=2012)
##  {
# attach(dat)
 # require(plotrix)
 # x<-paste("Obs_Survey_",f,sep="")
 # Data<-get(x)
 # d1<-subset(Data,Data[,2]>0)
 # windows(8,8)
 # plotCI(d1[,1],d1[,2],2*d1[,4],
 # main=Index_names[f],
 # ylim=c(0,(max(max(Data[,3],max(d1[,2])*1.9)))),xlim=c(yf,yl),pch=19,
 # ylab=ylab,
 # xlab="Year",col="blue",scol="black",lab=c(10,10,7))

 # points(Data[,3]~Data[,1],type="l",col="red",lwd=2)

  #text(min(Data[,1])+2,(max(d1[,2])*1.5),"Survey Point Estimate",pos=4,cex=0.8)
  #text(min(Data[,1])+2,(max(d1[,2])*1.4),"Model Fit",pos=4,cex=0.8)
  #points(min(Data[,1])+2,(max(d1[,2])*1.5),pch=19,col="blue")
  #text(min(Data[,1])+2,(max(d1[,2])*1.4),"--",cex=1.5,col="red")
  #detach(dat)

  #}


p.sur.stk.comp<-function(dat1,dat2,f=1,ylab="Biomass index (1000 t)", yf=1991,yl=2022,YLIM=c(0,650))
  {
  
  require(plotrix)
  require(data.table)
  x<-paste("Obs_Survey_",f,sep="")
  attach(dat1)
  Data<-data.frame(get(x))
  detach(dat1)
  attach(dat2)
  Data2<-data.frame(get(x))

for(i in 1:6){
  Data[,i]<-as.numeric(as.character(Data[,i]))
  Data2[,i]<-as.numeric(as.character(Data2[,i]))
  }

  #d1<-subset(Data,Data[,2]>0)
  #Data<-data.table(Data)
  d1<-subset(Data,!is.na(Data[,2])) ##Surv_Biom
  #Data2<-data.table(Data2)
  
  windows(8,8)
  plotCI(d1[,1],d1[,2],2*d1[,4],
  main=Index_names[f],
  ylim=YLIM,xlim=c(yf,yl),pch=19,
  ylab=ylab,
  xlab="Year",col="blue",scol="black",lab=c(10,10,7))
  points(d1[4:15,2]~d1[4:15,1],pch=16,col="blue")
  #points(d1[1:3,2]~d1[1:3,1],pch=16,col="green")
  points(Data[,3]~Data[,1],type="l",col="red",lwd=2)
  points(Data2[,3]~Data2[,1],type="l",col="black",lwd=2,lty=2)

  text(min(Data[,1])+15,(YLIM[2]),"Survey Point Estimates Fit",pos=4,cex=0.8)
  text(min(Data[,1])+15,(YLIM[2]*0.95),"Model 15.1 Fit",pos=4,cex=0.8)
  text(min(Data[,1])+15,(YLIM[2]*0.90),"Model 15.2 Fit",pos=4,cex=0.8)
  
  points(min(Data[,1])+15,YLIM[2],pch=19,col="blue")
  text(min(Data[,1])+15,(YLIM[2]*0.95),"--",cex=1.5,col="red")
  text(min(Data[,1])+15,(YLIM[2]*0.90),"--",cex=1.5,col="black")
  detach(dat2)
  }

##filled contour plot of F by age

cont.f.mort<-function(dat,f=1,lage=2,hage=15,cl="BW") ## data file, fishery, lowest and highest ages
{
  attach(dat)
  require(lattice)
  
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}
  
  x<-paste("F_age_",f,sep="")
  xx<-get(x)
  al<-lage
  ah<-hage
  windows(12,8)
  filled.contour(y=xx[,1],x=c(lage:hage),t(xx[,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c((min(xx[,1])-0.5),(max(xx[,1])-0.5)),col=clr,lab=c(10,10,7))
  detach(dat)
  mtext("F",4)
}


 ## bubble plot for catch at age for survey and fishery used in the model
p.bub.catch<-function(dat,typ="f",f=1,fyr=1978,lyr=2018,lage=1,hage=15,siz=0.1) ## input:  data file,lowest and highest age,first and last year, scaler for graphic
{

  attach(dat)
  if(typ=="f"){
  y<-paste("pobs_fsh_",f,sep="")
     x<-get(y)
     years<-x[,1]
     titl<-"Fishery catch at age"
     }

  if(typ=="s"){
     y<-paste("pobs_ind_",f,sep="")
     x<-pobs_ind_1
     years<-x[,1]
     titl<-"Survey catch at age"
    }

     windows(6,8)
     la<-lage+1
     ha<-hage+1
     N1<-(hage-lage)+1
     plot(c(fyr,lyr),c(lage,hage),type="n",ylab="Age",xlab="Year",lab=c(10,10,7),main=titl)
     abline(2,0,lty=2,col="gray80")
     abline(4,0,lty=2,col="gray80")
     abline(6,0,lty=2,col="gray80")
     abline(8,0,lty=2,col="gray80")
     abline(10,0,lty=2,col="gray80")
     abline(12,0,lty=2,col="gray80")
     abline(14,0,lty=2,col="gray80")

     for (i in 1:length(years)){
         points(rep(x[i,1],N1),c(lage:hage),cex=c(x[i,la:ha]/siz))
         }
     detach(dat)
}


##bubble plot of numbers or biomass at age CA=0.01,B=100,NA=0.1
p.bub.age<-function(dat,typ="NA",f=1,lage=1,hage=15,fy=1978,ly=2016,siz=0.1) ## input:  data file,lowest and highest age,first and last year, scaler for graphic
{

  attach(dat)
  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1
  if(typ=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     x<-get(y)
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }}


     windows(6,8)

     ti<-(ly-min(N[,1]))+1
     tt<-max(fy,min(N[,1]))-min(N[,1])+1
     la<-lage+1
     ha<-hage+1
     N1<-(hage-lage)+1
     plot(c(lage,hage),c(fy,ly),type="n",ylab="Year",xlab="Age",lab=c(10,10,7),main=titl)

     for (i in tt : ti){
         points(c(lage:hage),rep(N[i,1],N1),cex=c(x[i,la:ha]/siz))
         }
     detach(dat)
}


## Bubble plot of numbers at age overlaid with F mortality in filled contours

cont.f.mort2<-function(dat,typ="NA",f=1,lage=1,hage=15,fy=1978,ly=2016,siz=0.1,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}

  al<-lage+1
  ah<-hage+1
  y<-paste("F_age_",f,sep="")
  Data<-get(y)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1
  if(typ=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     x<-get(y)
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }}
  windows(6,8)
  filled.contour(y=Data[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,al:ah]/siz))
             }
            }
  )
  mtext("F",4)
  detach(dat)
}

## Filled contour plots of residuals from the fishery catch at age data
cont.f.age.res<-function(dat,typ="S",f=1,lage=1,hage=15,cl="BW",nl=40){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f,sep="")
      y<-paste("pobs_fsh_",f,sep="")
      }

      if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=nl))}
      if(cl!="BW"){clr<-rainbow(nl,start=0.1,end=0.9)}

attach(dat)
require(lattice)
al<-lage
ah<-hage

xx<-get(x)
yy<-get(y)
rr<-yy-xx

windows(6,8)
filled.contour(y=xx[,1],x=c(lage:hage),t(rr[,al:ah]),
,ylab="Year",xlab="Age",col=clr,
nlevels=nl,main="Proportion Catch-at-age Residuals",
plot.axes={axis(2,xx[,1],las=2);axis(1,c(lage:hage));
contour(y=xx[,1],x=c(lage:hage),t(rr[,al:ah]),col="grey",add=T)}
)

detach(dat)
}

## catch biomass overlaid with biomass or numbers
cont.n.Cbiom<-function(dat,typ="B",f=1,lage=1,hage=15,fy=1978,ly=2016,siz=100,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}

  al<-lage+1
  ah<-hage+1
  N1<-(hage-lage)+1
  y<-paste("C_fsh_",f,sep="")
  y2<-paste("wt_fsh_",f,sep="")
  Data1<-get(y)
  Data2<-get(y2)
  Data<-Data1*Data2
  Data[,1]<-Data1[,1]

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1
  if(typ=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     x<-get(y)
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=N1+1,nrow=nrow(N))
     x[,1]<-N[,1]
    for (i in tt:yrs){
        x[i,al:ah]<-N[i,al:ah]*wt_a_pop
     }}
  windows(6,8)
  filled.contour(y=Data[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],N1),cex=c(x[i,al:ah]/siz))
             }
            }
  )
  mtext("Catch Biomass (1000 t)",4)
  detach(dat)
}


##bimoass plots with error bars
p.biom.stk<-function (dat,typ="SSB")
{

  if(typ=="SSB")titl<-"Spawning Biomass (t)"
  if(typ=="TB"){typ="TotBiom"
                titl<-"Total Biomass (t)"
                }
  if(typ=="R"){typ="R"
                titl<-"Age 1 Recruites(N)"
                            }
  attach(dat)
  require(lattice)
  Data<-get(typ)
  windows(8,8)
  plot(Data[,1],Data[,2],xlab="Year",ylab=paste(titl),
  pch=19,col="blue",ylim=c(0,(max(Data[,2])*1.5)),lab=c(10,10,7))

  arrows(Data[,1],Data[,4],Data[,1],Data[,5],col="black",angle=90,code=3,length=0.05)
  detach(dat)
  }
  
##effective N Plots

p.eff.n<-function(dat,typ="S",f=1)
  {
      if (typ=="S"){
         x<-paste("EffN_Survey_",f,sep="")
         titl<-"Survey Mean Age"
      }

      if (typ=="F"){
         x<-paste("EffN_Fsh_",f,sep="")
         titl<-"Fishery Mean Age"
      }

      attach(dat)
      require(plotrix)
      Data<-data.frame(get(x))
      
      for(i in 1:8){
  	Data[,i]<-as.numeric(as.character(Data[,i]))
  	}

  #d1<-subset(Data,Data[,2]>0)
  #Data<-data.table(Data)
  d1<-subset(Data,!is.na(Data[,2])) ##Surv_Biom
  #Data2<-data.table(Data2)
      windows(12,8)
      plot(Data[,1],Data[,4],
      ylim=c(0,(max(Data[,4])*1.5)),
      ylab=paste(titl),xlab="Year",pch=19,col="blue",lab=c(10,10,7))

      arrows(Data[,1],Data[,7],Data[,1],Data[,8],col="black",angle=90,code=3,length=0.05)
      points(Data[,1],Data[,5],type="l",col="red",lwd=2)

      text(min(Data[,1])+2,2,"Observed mean age",pos=4,cex=0.8)
      text(min(Data[,1])+2,1.6,"Model-predicted mean age",pos=4,cex=0.8)
      points( min(Data[,1])+2,2,pch=19,col="blue")
      text(min(Data[,1])+2,1.6,"--",cex=1.5,col="red")


      detach(dat)

  }

p.eff.n.com<-function(dat,dat2,typ="S",f=1)
  {
      if (typ=="S"){
         x<-paste("EffN_Survey_",f,sep="")
         titl<-"Survey Mean Age"
      }

      if (typ=="F"){
         x<-paste("EffN_Fsh_",f,sep="")
         titl<-"Fishery Mean Age"
      }

      attach(dat)
      require(plotrix)
      Data<-data.frame(get(x))
      detach(dat)
      attach(dat2)
      Data2<-data.frame(get(x))
            
      for(i in 1:8){
  	Data[,i]<-as.numeric(as.character(Data[,i]))
        Data2[,i]<-as.numeric(as.character(Data2[,i]))
  	}

      windows(12,8)
      plot(Data[,1],Data[,4],
      ylim=c(0,(max(Data[,4])*1.5)),
      ylab=paste(titl),xlab="Year",pch=19,col="blue",lab=c(10,10,7))

      arrows(Data[,1],Data[,7],Data[,1],Data[,8],col="black",angle=90,code=3,length=0.05)
      points(Data[,1],Data[,5],type="l",col="red",lwd=2)
      points(Data2[,1],Data2[,5],type="l",col="black",lwd=2,lty=2)

      text(min(Data[,1])+2,2,"Observed mean age",pos=4,cex=0.8)
      text(min(Data[,1])+2,1.5,"Model 15.1-predicted mean age",pos=4,cex=0.8)
      text(min(Data[,1])+2,1,"Model 15.2-predicted mean age",pos=4,cex=0.8)
      points( min(Data[,1])+2,2,pch=19,col="blue")
      text(min(Data[,1])+2,1.5,"--",cex=1.5,col="red")
      text(min(Data[,1])+2,1,"--",cex=1.5,col="black")


      detach(dat2)

  }
  
## proportion at age fit plots

p.age.fit<-function(dat,typ="F",f=1,lage=1,hage=15,fy=1978,ly=1984)
{
   if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f,sep="")
      y<-paste("pobs_fsh_",f,sep="")
      }

  attach(dat)
  Data<-get(y)
  Data2<-get(x)
  n1<- max(fy,min(Data[,1]))-min(Data[,1])+1
  n2<-ly-min(Data[,1])+1
  g<-min(n2,length(Data[,1]))
  al=lage+1
  ah=hage+1
  if (((g-n1+1)/2)-trunc((g-n1+1)/2)==0)b<-(g-n1+1)/2
  if (((g-n1+1)/2)-trunc((g-n1+1)/2)!=0) b<-(g-n1+2)/2
  windows(12,8)
  par(mfrow=c(2,4))
  for ( i in n1:g)
      {
      xx<-list(breaks=seq((lage-0.5),(hage+0.5),by=1),counts=c(hage:lage),
      intensities=as.numeric(Data[i,al:ah]),density=as.numeric(Data[i,al:ah]),
      mids=seq((lage),(hage),by=1),xname="Age",equidist=TRUE)

      attr(xx,"class")="histogram"

      plot(xx,freq=F,ylab="Proportion at Age",xlim=c(0,(hage+1)),
      ylim=c(0,(max(Data[,al:ah])*1.1)),main=paste(Data[i,1]))
      points(xx$mids,Data2[i,al:ah],type="l",col="red",lwd=2)

      }
detach(dat)
}
##fit to catch
p.catch.fit<-function(dat,f=1,ylab="Catch biomass (kt)" )
{
  x<-paste("Obs_catch_",f,sep="")
  y<-paste("Pred_catch_",f,sep="")
  attach(dat)
  Data<-get(y)
  Data2<-get(x)
  xx<-list(breaks=seq(min(Yr)-0.5,max(Yr)+0.5,1),counts=Yr,
  intensities=as.numeric(Data),density=as.numeric(Data),
  mids=seq(min(Yr),max(Yr),by=1),xname="Year",equidist=TRUE)

  attr(xx,"class")="histogram"
  #windows(8,8)
  plot(xx,freq=F,
  ylab=ylab,
  xlim=c(min(Yr)-1,(max(Yr)+1)),
  main=Fshry_names[f],
  ylim=c(0,(max(Data)*1.25)),col="light grey")
  points(xx$mids,as.numeric(Data2),type="l",col="red",lwd=2)
  text(min(xx$mids)+2,(max(Data)),"Model Fit",pos=4,cex=0.8)
  text(min(xx$mids)+2,(max(Data)),"--",col="red",cex=1.5)
detach(dat)
}


##histogram or line plots of selectivity
p.select.hist<-function(dat,typ="F",h="T",f=1,lage=2,hage=15,fy=1978,ly=1984)
{
  attach(dat)

  if (typ=="S"){
      x<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("sel_fsh_",f,sep="")
      }

  xx<-get(x)
  la=lage+1
  ha=hage+1
  n1<-max(fy,min(xx[,2]))-min(xx[,2])+1
  n2<-ly-min(xx[,2])+1

  if (((n2-n1+1)/2)-trunc((n1-n2+1)/2)==0)b<-(n2-n1+1)/2
  if (((n2-n1+1)/2)-trunc((n1-n2+1)/2)!=0) b<-(n2-n1+2)/2

  Data<-matrix(ncol=((hage-lage)+1),nrow=length(xx[,1]))

  for ( i in 1:length(xx[,1])){  ##create matrix of selectivities scaled to 1.0
      m<-max(xx[i,la:ha])
      Data[i,1:((hage-lage)+1)]<-xx[i,la:ha]/m
   }

  g<-min(n2,length(Data[,1]))
  windows(12,8)
  par(mfrow=c(2,b))
    if(h=="T"){

     for ( i in n1:g)
          {
          xxx<-list(breaks=seq((lage-0.5),(hage+0.5),by=1),counts=c(hage:lage),
          intensities=as.numeric(Data[i,1:ncol(Data)]),density=as.numeric(Data[i,1:ncol(Data)]),
          mids=seq((lage),(hage),by=1),xname="Age",equidist=TRUE)

          attr(xxx,"class")="histogram"

          plot(xxx,freq=F,ylab="Selectivity at Age",xlim=c(0,(hage+1)),
          ylim=c(0,(max(Data[,1:ncol(Data)])*1.1)),main=paste(xx[i,2]),col="salmon")
          }
      }


  if(h!="T"){
      for ( i in n1:n2)
      {
      plot(seq((lage),(hage),by=1),Data[i,1:ncol(Data)],type="l",lwd=2,
      main=paste(xx[i,2]),col="red",ylab="Selectivity at Age",xlab="Age")
      }

  }


detach(dat)
}



##filled countour plots of selectivity

c.select<-function(dat,typ="F",f=1,lage=2,hage=15,fy=1978,ly=2016,cl="BW")
{
  if (typ=="S"){
      x<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.13,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=0.9)}

  la=lage+1
  ha=hage+1

  attach(dat)

  x<-get(x)
  n1<-max(fy,min(x[,2]))-min(x[,2])+1
  n2<-ly-min(x[,2])+1

   Data<-matrix(ncol=((hage-lage)+1),nrow=length(x[,1]))

  for ( i in 1:length(x[,1])){  ##create matrix of selectivities scaled to 1.0
      m<-max(x[i,la:ha])
      Data[i,1:(hage-lage+1)]<-x[i,la:ha]/m
   }

  g<-min(n2,length(Data[,2]))

  ti<-(g-n1+1)

  windows(6,8)
  filled.contour(y=x[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage))})

  mtext("Selectivity",4)

detach(dat)
}



##filled countour plots of selectivity  with numbers at age

c.select.b<-function(dat,typ="F",typ2="NA",f=1,lage=1,hage=15,fy=1978,ly=2016,siz=0.1,cl="BW")
{
  if (typ=="S"){
      y<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      y<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.2,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=1)}

  la=lage+1
  ha=hage+1
  N1<-(ha-la)+1

  attach(dat)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1


  if(typ2=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ2=="CB"){
     z<-paste("C_fsh_",f,sep="")
     x<-get(z)
     titl<-"Catch at Age"
    }

  if(typ2=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=ncol(N),nrow=nrow(N))
     x[,1]<-N[,1]
    for (i in tt:yrs){
        x[i,2:ha]<-N[i,2:ha]*wt_a_pop
     }}



  y<-get(y)
  n1<-max(fy,min(y[,2]))-min(y[,2])+1
  n2<-ly-min(y[,2])+1

 Data<-matrix(ncol=(ncol(y)-2),nrow=nrow(y))
 #Data[,1]<-y[,2]

  for ( i in 1:nrow(y) ){
      m<-max(y[i,(la+1):(ha+1)])
      Data[i,lage:hage]<-y[i,(la+1):(ha+1)]/m
  }

  g<-min(n2,nrow(Data))

  ti<-(g-n1+1)

  windows(6,8)
  filled.contour(y=y[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5)
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  main=titl,plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in 1 : ti){
             points(c(lage:hage),rep(N[i,1],N1),cex=c(x[i,la:ha]/siz))
             }
            })

  mtext("Selectivity",4)



detach(dat)
}





##plotting spawner recruit curve

p.stock.rec<-function(dat,xlab="Spawning biomass",ylab="Age 2 recruits")
{
 attach(dat)
 windows(8,8)
 plot(Stock_Rec[,2],Stock_Rec[,4],xlim=c(0,max(Stock_Rec[,2])),type="o",
 xlab=xlab, ylab=ylab, pch=19,col="blue")
 points(stock_Rec_Curve, type="l",col="red",lwd=2)
 detach(dat)
 }
 
##histogram of Recruitement
p.rec.hist<-function(dat,fy=1978,ly=2016,ylab=expression(paste("Recruits at age 2 (number x ",10^9," )",sep="")))
{
  attach(dat)
  Data<-R
  Data<-subset(Data,Data[,1]<=ly&Data[,1]>=fy)
  t2<-length(Data[,1])-3
  menr<-mean(Data[1:t2,2])
  windows(12,8)
  xx<-list(breaks=seq((fy-0.5),(ly+0.5),by=1),counts=c(fy:ly),
          intensities=as.numeric(Data[,2]),density=as.numeric(Data[,2]),
          mids=seq(fy,ly,by=1),xname="Year",equidist=TRUE)
          attr(xx,"class")="histogram"
          plot(xx,freq=F,ylab=ylab,
          xlim=c((fy-1),(ly+1)),
          ylim=c(0,(max(Data[,5])*1.1)),main="",col="#ffff00",lab=c(10,10,7))
          arrows(Data[,1],Data[,4],Data[,1],Data[,5],col="black",angle=90,code=3,length=0.05)
          lines(Data[,1],rep(menr,length(Data[,1])),type="l",lty=3,col="blue",lwd=2)
          # text(fy+2,(max(Data[,2])*1.1),paste("Mean recruitment for ",fy," to ",ly-1,sep=""),pos=4,cex=0.8)
          # text(fy+2,(max(Data[,2])*1.1),"--",col="blue",cex=1.5)
detach(dat)
}


## Full selection F over time plot

p.full.f<-function(dat,f=1)## input:  data file, fishery number
{
  attach(dat)

 x<-paste("F_fsh_",f,sep="")
 xx<-get(x)
 windows(6,6)
 plot(xx[,1],xx[,3],ylab="Fishing Mortality",xlab="Year",type="o",pch=19,col="black",ylim=c(0,(max(xx[,3]*1.25))))

  detach(dat)
}

## Survey Selectivity Curve single plot

p.survey.curve<-function(dat,f=1,lage=2,hage=15)## input:  data file, survey number
{
attach(dat)
 x<-paste("sel_ind_",f,sep="")
 xx<-get(x)
 windows(6,6)
 la<-lage+1
 ha<-hage+1
 plot(c(lage:hage),xx[1,la:ha]/max(xx[1,la:ha]),ylab="Survey Selectivity"
 ,xlab="Age",type="o",pch=19,col="black",ylim=c(0,1))

  detach(dat)
}

## Fish Selectivity Curve plot

p.fish.curve<-function(dat,f=1,lage=2,hage=15)## input:  data file, survey number
{
attach(dat)
 x<-paste("sel_fsh_",f,sep="")
 xx<-get(x)
 xxx<-matrix(ncol=((hage-lage)+1),nrow=length(xx[,1]))
 la<-lage+1
 ha<-hage+1
 k<-length(xx[,1])

 for ( i in 1:length(xx[,1])){
 m<-max(xx[i,la:ha])
 xxx[i,1:((hage-lage)+1)]<-xx[i,la:ha]/m
 }


 windows(6,6)

 men_a<-array(dim=(hage-lage+1))
 for (i in lage:hage){
 men_a[((i-lage)+1)]<-mean(xxx[(k-6):(k-1),i])
 }

 plot(c(lage:hage),xxx[k-3,lage:hage],ylab="Selectivity",
 xlab="Age",type="o",pch=19,col="salmon",ylim=c(0,(max(xxx[k-3,lage:hage]*1.25))))

 points(c(lage:hage),xxx[k-2,lage:hage],type="o",col="blue",pch=3)
 points(c(lage:hage),men_a,type="l",col="black",lwd=2)
 points(c(lage:hage),2*mature_a,type="o",col="dark green",pch=17)

 text(4,0.4,paste(max(xx[,2])-2," Fishery Selectivity",sep=""),pos=4,cex=0.8 )
 text(4,0.3,paste(max(xx[,2])-1," Fishery Selectivity",sep=""),pos=4,cex=0.8)
 text(4,0.2,"Maturity at Age",pos=4,cex=0.8)
 text(4,0.1,"Five year ave. Fishery Selectivity",pos=4,cex=0.8)


 points(4,0.4,type="p",col="salmon",pch=19)
 points(4,0.2,type="p",col="dark green",pch=17)
 points(4,0.1,type="p",col="blue",pch=3)

 text(4,0.4,"--",cex=1.2,col="salmon")
 text(4,0.3,"--",cex=1.2,col="blue")
 text(4,0.2,"--",cex=1.2,col="dark green")
 text(4,0.1,"--",cex=1.2)

 detach(dat)
}
##biomass plots with polygons
## this function can handle the results of up to 2 separate models
# Still to do: 
#	make it able to handle more models (how many?)
#	make polygons transparent
p.biom.pol <- function (dat1, dat2, n.mod=2, typ="SSB",
        color=c("light green", "dark green", "light blue", "dark blue"), new=TRUE)
{
  if(typ=="SSB")titl<-"Spawning Biomass (t)"

   if(typ=="SSBNFR"){typ="SSB_NoFishR"
                titl<-"Spawning Biomass Relative to Unfished"
                }
  if(typ=="TB"){typ="TotBiom"
                titl<-"Total Biomass (t)"
                }

  if(typ=="TBNF"){typ="TotBiom_NoFish"
                titl<-"Total Biomass w/o Fishing (t)"
                }
  if(typ=="R"){typ="R"
                titl<-"Age 1 Recruites(N)"
                            }
  require(lattice)
  
  attach(dat1)
  Data = get(typ)  
  xlim1 = c(min(Data[,1]),max(Data[,1]))
  ymax1 = max(Data[,2])
  if(n.mod==1){
  	xlim = xlim1
  	ylim = c(0, ymax1*1.5)
  }
  detach(dat1)
  if(n.mod!=1){
  	attach(dat2)
	Data = get(typ)  
 	xlim2 = c(min(Data[,1]),max(Data[,1]))
 	ymax2 = max(Data[,2])
  	detach(dat2)
  	xlim = range(c(xlim1, xlim2))
  	ylim = c(0, max(ymax1, ymax2)*1.5)
  }
 
  attach(dat1)
  Data=get(typ)
  if(new==TRUE) windows(,6,6)
  par(mar=c(4,5,2,0.2))
  plot(Data[,1],Data[,2],type="n",lwd=2,cex.axis=2,cex.lab=2,
  xlab="Year",ylab=paste(titl),lab=c(10,10,7),xlim=xlim, ylim=ylim, col=color[2])
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])

  polygon(x,y,col=color[1],lty=3,border="grey")
  lines(Data[,1],Data[,2],lwd=2,lty=2, col=color[2])
  detach(dat1)
  
  if(n.mod!=1){
  	attach(dat2)
  Data=get(typ)
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])

  polygon(x,y,col=color[3],lty=3,border="grey")
  lines(Data[,1],Data[,2], lwd=2, col=color[4])
  detach(dat2)
  
  attach(dat1)
  Data=get(typ)
  lines(Data[,1],Data[,2], lwd=2, lty=2,col=color[2])
  detach(dat1)	
  }
  
}

mountains <-
  function(zmat, xvec=NULL, yvec=NULL, zscale=3, nshades=100,
           xaxs='i', yaxs='i', xlab="", ylab="", las=1, addbox=FALSE, cex.xax=1, cex.yax=1,...)
{
  ## DESCRIPTION:
  # a function by Ian Taylor designed to look like the cool-looking Figure 7 in
  # Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical model for
  # stock assessment of southern bluefin tuna with temporal changes in selectivity.
  # South African Journal of Marine Science 25:331-362.

  errors <- FALSE
  for(icol in 1:ncol(zmat)){
    if(!is.numeric(zmat[,icol])){
      errors <- TRUE
      print(paste("error: column",icol,"of zmat is not numeric"))
    }
  }
  if(errors) return(invisible())

  # fill in vectors if not provided
  nrowz <- nrow(zmat)
  ncolz <- ncol(zmat)
  if(is.null(yvec)) yvec <- 1:nrowz
  if(is.null(xvec)) xvec <- 1:ncolz

  # define some limits
  xmin <- min(xvec)
  xmax <- max(xvec)
  zmax <- zscale*max(zmat)

  ny <- length(yvec)
  if(ny!=nrowz){
      print("length(yvec) must equal nrow(zmat)",quote=FALSE)
      return()
  }
  if(length(xvec)!=ncolz){
      print("length(xvec) must equal ncol(zmat)",quote=FALSE)
      return()
  }

  zseq <- seq(0, zmax, length=nshades)
  xvec2 <- c(xmin, xvec, xmax) # adding extra points for bottom corners of polygon

  # plot(0, type='n', xlim=c(xmin, xmax), ylim=c(0, 1.1*(ymax+ny)), xaxs='i', yaxs='i', ...)
  plot(0, type='n', xlim=c(xmin, xmax), ylim=c(min(yvec), (max(yvec) + 1.1*zmax)),
       xaxs=xaxs, yaxs=yaxs, xlab=xlab, ylab=ylab, axes=F, ...)

  for(iy in ny:1){
    zvec <- as.numeric(zmat[iy, ])
    zvec2 <- c(0, zscale*zvec, 0) # row from z matrix

    # calculate set of all intersections between polygon and the horizontal lines
    x3list <- list()
    for(iz in 1:nshades){
      z <- zseq[iz]
      x3 <- numeric()
      for(ix in 2:length(xvec2)){
          z1 <- zvec2[ix-1]
          z2 <- zvec2[ix]
          x1 <- xvec2[ix-1]
          x2 <- xvec2[ix]
          if(z >= min(z1, z2) & z < max(z1, z2)){
              x3 <- c(x3, (z-z1)*(x2-x1)/(z2-z1)+x1)
          }
      }
      x3list[[iz]] <- x3
    }
    # draw little polygons between each pair of horizontal lines
    for(iz in 2:length(x3list)){

      z2 <- zseq[iz]
      z1 <- zseq[iz-1]

      x3hi <- x3list[[iz]] # x-values of intersections along upper line
      x3lo <- x3list[[iz-1]]   # x-values of intersections along lower line

      npoly <- length(x3lo)/2

      for(ipoly in 1:npoly){
        xlo <- x3lo[ipoly*2 + -1:0] # lower line intersections for individual polygon
        xhi <- x3hi[x3hi>=xlo[1] & x3hi<=xlo[2]] # upper line intersections
        extra <- (zvec2 >= z1 & zvec2 <= z2 & xvec2>=xlo[1] & xvec2<=xlo[2]) # identifying extra points to add
        xhi2 <- c(xhi,xvec2[extra]) # adding extra points to vector of upper x-values
        zhi <- c(rep(z2,length(xhi)), zvec2[extra]) # add corresponding z-values
        zhi2 <- zhi[order(xhi2)] # put the z-values in order based on order of x-values
        xhi2 <- sort(xhi2) # now order the x-values

        # make polygon
        polygon(x = c(xlo[2:1],xhi2),
                y = yvec[iy]+ c(z1,z1,zhi2),
                col = grey(1-.9*z1/zmax),border=grey(1-.9*z1/zmax))
      }
    }
    # black polygon around the outside
    polygon(xvec2, yvec[iy]+zvec2)
  }

  # add axes
  axis(1,at=xvec, cex.axis=cex.xax)
  axis(2,at=yvec,las=las, cex.axis=cex.yax)
#  axis(4,at=yvec,las=las,labels=FALSE) # extra ticks on right hand side
  if(addbox) box() # add box if desired
}


## Fish Selectivity curve over ages and years, for fisheries or surveys
sel.age.mountain = function(dat, f=1, typ='F', xvec=c(2:15), yvec=NULL, zscale=3, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1, ...)
{	
	attach(dat)
	if(typ=='F'){
		x = paste('sel_fsh_', f, sep='')
		mtitl=paste(Fshry_names[f], 'Selectivity')
	}
	if(typ=='S'){
		x = paste('sel_ind_', f, sep='')
		mtitl=paste(Index_names[f], 'Selectivity')
	}
	Select = get(x)
	zmat = Select[,-c(1:2)]
	yrs = Select[,2]
	if(new==TRUE) windows(,5,6.5)
	mountains(zmat, xvec=xvec, yvec=yrs, zscale=zscale, nshades=nshades, xaxs=xaxs, yaxs=yaxs, xlab=xlab, ylab=ylab, las=1, addbox=addbox, cex.xax=cex.xax, cex.yax=cex.yax)
	title(main=mtitl)
	detach(dat)	
}


## plot projections
plot_proj<-function(percentdb,alt=1){
  alt1<-subset(percentdb,percentdb$ALT==alt)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="SSBMean"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="o",pch=16,lwd=3,ylim=c(0,biom$SB0),ylab="Projected female spawning biomass (1000t)",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="SSBLCI"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="SSBUCI"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="l",lty=3)
  points(rep(biom$SB40,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(biom$SB35,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)
  points(rep(biom$SB0,14)~c(2011:2024),type="l",lty=2,col="blue",lwd=3)

  text(2012,biom$SB0-50,"Projected SSB",pos=4)
  text(2012,biom$SB0-60,"SSB0",pos=4)
  text(2012,biom$SB0-70,"SSB40",pos=4)
  text(2012,biom$SB0-80,"SSB35",pos=4)


    points(c(2011,2012),rep(biom$SB0-50,2),type="o",lwd=3)
    points(c(2011,2012),rep(biom$SB0-60,2),type="l",col="blue",lty=2,lwd=3)
    points(c(2011,2012),rep(biom$SB0-70,2),type="l",col="red",lty=2,lwd=1)
    points(c(2011,2012),rep(biom$SB0-80,2),type="l",col="orange",lty=2,lwd=2)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="CMean"]~alt1$YEAR[alt1$NAME=="CMean"],type="o",pch=16,lwd=3,ylim=c(0,Cofl+100),ylab="Projected Catch (1000t)",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="CLCI"]~alt1$YEAR[alt1$NAME=="CMean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="CUCI"]~alt1$YEAR[alt1$NAME=="CMean"],type="l",lty=3)
  points(rep(Cofl,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(Cabc,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)

  text(2012,Cofl+100-50,"Projected Catch",pos=4)
  text(2012,Cofl+100-60,"Cofl",pos=4)
  text(2012,Cofl+100-70,"Cabc",pos=4)

  points(c(2011,2012),rep(Cofl+100-50,2),type="o",lwd=3)
  points(c(2011,2012),rep(Cofl+100-60,2),type="l",col="red",lty=2,lwd=1)
  points(c(2011,2012),rep(Cofl+100-70,2),type="l",col="orange",lty=2,lwd=2)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="F_Mean"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="o",pch=16,lwd=3,ylim=c(0,1.0),ylab="F",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="F_LCI"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="F_UCI"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="l",lty=3)
  points(rep(Fofl,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(Fabc,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)

  text(2012,0.8,"Projected F",pos=4)
  text(2012,0.75,"Fofl",pos=4)
  text(2012,0.7,"Fabc",pos=4)


    points(c(2011,2012),rep(0.8,2),type="o",lwd=3)
    points(c(2011,2012),rep(0.75,2),type="l",col="red",lty=2,lwd=1)
    points(c(2011,2012),rep(0.7,2),type="l",col="orange",lty=2,lwd=2)


}


## comparison with old assessment results
plot_comp_old<-function(){
  maxy<-max(OLD_SPBIOM[1:43,2:5])
  maxy<-max(maxy,max(test$SSB[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1963,2012),type="l",lwd=2,ylab=expression(paste("Female spawning biomass (kt)",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$SSB[,1],test$SSB[,2],type="o",col="red",lwd=3)

  text(1970,maxy-50,"2007 Model",pos=4)
  text(1970,maxy-75,"2008 Model",pos=4)
  text(1970,maxy-100,"2009 Model",pos=4)
  text(1970,maxy-125,"2010 Model",pos=4)
  text(1970,maxy-150,"2011 Model",pos=4)
    
  points(c(1965,1970),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-75,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-100,2),type="l",col="blue",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-125,2),type="l",col="black",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-150,2),type="o",col="red",lwd=2)
    
    
  windows()
  maxy<-max(OLD_TOTBIOM[1:31,2:5])
  maxy<-max(maxy,max(test$TotBiom[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1978,2012),type="l",lwd=2,ylab=expression(paste("Total biomass (kt)",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$TotBiom[,1],test$TotBiom[,2],type="o",col="red",lwd=3)

  text(2005,maxy-50,"2007 Model",pos=4)
  text(2005,maxy-110,"2008 Model",pos=4)
  text(2005,maxy-170,"2009 Model",pos=4)
  text(2005,maxy-230,"2010 Model",pos=4)
  text(2005,maxy-290,"2011 Model",pos=4)
  points(c(2000,2005),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-110,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-170,2),type="l",col="blue",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-230,2),type="l",col="black",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-290,2),type="o",col="red",lwd=3)
}




filled.contour.TAB <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
     plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
        col = col)
    invisible()
}

#######################################################################
#Plot of B/Bmsy against F/Fmsy for the most recent year
#Trevor A. Branch  10 September 2009  tbranch@gmail.com
#######################################################################
plot.phase.plane <- function(SSB0,Fabc,Fmsy,BoverBmsy, FoverFmsy,xlim=c(0,6),ylim=c(0,1.5),header,bw.mult=1,jitter.fac=0,eyr=2015) {
   #plot(x=BoverBmsy,y=FoverFmsy,xlim=xlim,ylim=ylim,las=1,
   #       yaxs="i",xaxs="i",xlab="",ylab="")
   require(KernSmooth)
   crosshair.data.uncen <- cbind(BoverBmsy,FoverFmsy)
   #APRIL 22 version: References in Scott 1992 and Bowman and Azzalini 1997
   d<-2 # the bandwidth dimension
   bmsy.bw<-sqrt(var(crosshair.data.uncen[,1]))*(4/((d+2)*length(crosshair.data.uncen[,1])))^(1/(d+4))
   umsy.bw<-sqrt(var(crosshair.data.uncen[,2]))*(4/((d+2)*length(crosshair.data.uncen[,2])))^(1/(d+4))
   # please note the range restrictions at 2.01 to include the points that line up at the boundaries
   kernel.dens <- bkde2D(crosshair.data.uncen[,c(1,2)], bandwidth=c(bmsy.bw*bw.mult,umsy.bw*bw.mult), range.x=list(xlim,ylim))

   # generate color palette
   paletteable.egg<-colorRampPalette(c("#BFEFFF","white","white", "yellow","#FFC125"))
   

   par(oma=c(0.5,0.5,0.5, 0.5))
   filled.contour.TAB(kernel.dens$x1, kernel.dens$x2, kernel.dens$fhat, nlevels=15, color.palette =paletteable.egg,
               xlab="", ylab="", xlim=xlim, ylim=ylim, cex.lab=1.3)
   par(new=T)

   plot(x=jitter(BoverBmsy,jitter.fac),y=jitter(FoverFmsy,jitter.fac),type="l",xlim=xlim,ylim=ylim,las=1,
          yaxs="i",xaxs="i",xlab="",ylab="",col="gray50",pch=20)

  yr<-c((eyr-length(BoverBmsy)+1):eyr)-1900
  yr[yr>=100]<-yr[yr>=100]-100

  for(i in 1:length(BoverBmsy)){
    text(BoverBmsy[i],FoverFmsy[i],paste(yr[i]),cex=0.85)
    }

    k=Fabc/Fmsy*(((SSB0*0.05)/(SSB0*0.40))-0.05)/(1-0.05)
    k2<-0.05
    k3<-(0.05*SSB0)/(SSB0*0.40)
    ##points(c(k3,k3),c(0,10),type="l",lty=3)
    points(c((0.4/0.35),xlim[2]),c(Fabc/Fmsy,Fabc/Fmsy),type="l",lwd=2)
    points(c((0.4/0.35),xlim[2]),c(1,1),type="l",lwd=2,col="red")
    #points(c(k2,1.0),c(0,Fabc/Fmsy),type="l",lwd=2)
    points(c(k3,(0.4/0.35)),c(k,Fabc/Fmsy),type="l",lwd=2)
    points(c(k3,k3),c(0,k),type="l",lwd=2)
    points(c(0,(0.4/0.35)),c(0,1),type="l",lwd=2,col="red")

    text(xlim[2]-1,ylim[2]-0.1,"OFL Definition",pos=4)
    text(xlim[2]-1,ylim[2]-0.2,"ABC Control Rule",pos=4)
    text (xlim[2]-1,ylim[2]-0.3,"B20%",pos=4)

    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.1,ylim[2]-0.1),lwd=2,type="l",col="red")
    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.2,ylim[2]-0.2),lwd=2,type="l")
    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.3,ylim[2]-0.3),type="l",lwd=2,col="brown")

   mtext(side=1,line=3.2,text=expression(B/B[MSY]),cex=1.3)
   mtext(side=2,line=3,expression(F/F[MSY]),cex=1.3)
   mtext(side=3,line=0.5,header,cex=1.3)
   abline(h=1,lty=2)
   abline(v=1,lty=2)
   abline(v=(0.2/0.35),col="brown",lwd=2)
}



##old model compare
plot_comp_old<-function(){
  maxy<-max(OLD_SPBIOM[1:43,2:5])
  maxy<-max(maxy,max(test$SSB[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1963,2012),type="l",lwd=2,ylab=expression(paste("Female spawning biomass (kt)",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$SSB[,1],test$SSB[,2],type="o",col="red",lwd=3)

  text(1970,maxy-50,"2007 Model",pos=4)
  text(1970,maxy-75,"2008 Model",pos=4)
  text(1970,maxy-100,"2009 Model",pos=4)
  text(1970,maxy-125,"2010 Model",pos=4)
  text(1970,maxy-150,"2011 Model",pos=4)
    
  points(c(1965,1970),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-75,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-100,2),type="l",col="blue",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-125,2),type="l",col="black",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-150,2),type="o",col="red",lwd=2)
    
    
  windows()
  maxy<-max(OLD_TOTBIOM[1:31,2:5])
  maxy<-max(maxy,max(test$TotBiom[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1978,2012),type="l",lwd=2,ylab=expression(paste("Total biomass (kt)",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$TotBiom[,1],test$TotBiom[,2],type="o",col="red",lwd=3)

  text(2005,maxy-50,"2007 Model",pos=4)
  text(2005,maxy-110,"2008 Model",pos=4)
  text(2005,maxy-170,"2009 Model",pos=4)
  text(2005,maxy-230,"2010 Model",pos=4)
  text(2005,maxy-290,"2011 Model",pos=4)
  points(c(2000,2005),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-110,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-170,2),type="l",col="blue",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-230,2),type="l",col="black",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-290,2),type="o",col="red",lwd=3)
}



cont.CB_TB<-function(dat,mod="1",f="NMFS_summer_bottom_trawl",f1="1",lage=1,hage=15,fy=1978,ly=2016,siz=0.05,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=16))}
   if(cl!="BW"){clr<-rainbow(16,start=0.5,end=1)}


  la<-lage+1
  ha<-length(N[1,])

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1

   titl="Biomass at Age"

     y<-paste("C_fsh_",f1,sep="")
     z<-paste("wt_fsh_",f1,sep="")

     x1<-get(y)
     x2<-get(z)
     Data<-x1*x2

        yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }


  #windows(8,8)
  filled.contour(y=N[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,la:ha]),levels=seq(0,32,by=2),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],hage-lage+1),cex=c(x[i,la:ha]/siz))
             }
            }
  )
  mtext("Catch (1,000 t)",4)
  mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
  detach(dat)
}



bp.f.age.res<-function(dat,mod="1",typ="S",f=1,f1=1,lage=1,hage=15,siz=3,fyr=1978,lyr=2016){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      titl="Survey Standardized Residuals"
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f1,sep="")
      y<-paste("pobs_fsh_",f1,sep="")
      titl="Fishery Standardized Residuals"
      }

attach(dat)
require(lattice)
la<-2
ha<-length(N[1,])

xx<-get(x)
yy<-get(y)
rr<-xx-yy
mean_rr<-mean(c(rr[,lage:hage]))
sd_rr<-sd(c(rr[,lage:hage]))

ti<-(min(xx[,1]) )
tt<-max(1978,min(N[,1]))-min(N[,1])+1
sr<-rr-(mean_rr)/sd_rr

#windows(8,8)

     la<-2
     ha<-length(N[1,])
     plot(c(fyr,lyr),c(lage,hage),type="n",xlab="Year",ylab="Age",lab=c(10,10,7),main=titl)
     for (i in 1 : length(xx[,1])){
     for (j in lage:hage){
         if(sr[i,j]>0){symbols(xx[i,1],j,circles=(siz*sr[i,j]),inches=F,bg="red",fg="red",add=T)}
         if(sr[i,j]<0){symbols(xx[i,1],j,circles=(-siz*sr[i,j]),inches=F,fg="blue",add=T)}
         if(sr[i,j]==0){points(xx[i,1],j,pch=4)}
         }}
         mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
         
        # windows(2,6)
        # d<-seq(max(sr),min(sr),length=10)
        # plot(seq(2,15,length=10),d,type="n",xlab="",ylab="Standardized Residuals",xaxt="n")
        # for ( i in 1:10){
        # if (d[i]>0){ symbols(8,d[i],circles=(3*d[i]),inches=F,fg="red",bg="red",add=T) }
        # if (d[i]<0){ symbols(8,d[i],circles=(-3*d[i]),inches=F,fg="blue",add=T) }
        # if (d[i]==0){ points(8,d[i],pch=4) }
        # }
         
         


detach(dat)
}


##F with biomass or catch biomass
cont.f.mort3<-function(dat,typ="CB",f=1,lage=2,hage=15,fy=1978,ly=2016,siz=100,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}

  al<-lage
  ah<-hage
  y<-paste("F_age_",f,sep="")
  Data<-get(y)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     y1<-paste("wt_fsh_",f,sep="")
     x<-get(y)
     x1<-get(y1)
     x=x*x1
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }}
  windows(6,8)
  filled.contour(y=Data[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,al:ah]/siz))
             }
            }
  )
  mtext("F",4)
  detach(dat)
}



c.select.b2<-function(dat,typ="F",typ2="CB",f=1,lage=1,hage=15,fy=1978,ly=2016,siz=20,cl="BW")
{
  if (typ=="S"){
      y<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      y<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.2,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=1)}

  la=lage+1
  ha=hage+1
  N1<- (ha-la)+1

  attach(dat)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1


  if(typ2=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ2=="CB"){
     z<-paste("C_fsh_",f,sep="")
     z2=paste("wt_fsh_",f,sep="")
     x<-get(z)
     x1<-get(z2)
     x<-x*x1
     titl<-"Catch at Age"
    }
   
   if(typ2=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=ncol(N),nrow=nrow(N))
     x[,1]<-N[,1]
    for (i in tt:yrs){
        x[i,2:ha]<-N[i,2:ha]*wt_a_pop
     }}
  y<-get(y)
  n1<-max(fy,min(y[,2]))-min(y[,2])+1
  n2<-ly-min(y[,2])+1

 Data<-matrix(ncol=N1,nrow=nrow(y))

  for ( i in 1:length(y[,1])){
      m<-max(y[i,(la+1):(ha+1)])
      Data[i,1:(hage-lage+1)]<-y[i,(la+1):(ha+1)]/m
  }

  g<-min(n2,length(Data[,2]))

  ti<-(g-n1+1)

  windows(6,8)

  
  windows(6,8)
  filled.contour(y=y[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5)
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  main=titl,plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in 1 : ti){
             points(c(lage:hage),rep(N[i,1],N1),cex=c(x[i,la:ha]/siz))
             }
            })

  mtext("Selectivity",4)



detach(dat)
}


## show selectivities and maturity for selected years for fishery and survey
select_years<-function(dat,Syrs=c(1978),Fyrs=c(1978,1990,1999)){
  attach(dat)
  Smax<-max(sel_ind_1[,3:17])
  Fmax<-max(sel_fsh_1[,3:17])
  x1<-match(sel_ind_1[,2],Syrs)
  s1<-subset(sel_ind_1,is.na(x1)==F)/Smax

  x1<-match(sel_ind_1[,2],Fyrs)
  f1<-subset(sel_fsh_1,is.na(x1)==F)/Fmax
  
  nr<-nrow(s1)+nrow(f1)
  div<-0.5*(1/nr)
  div<-seq(0.1,0.6,by=div)
  plot(c(0,1)~c(0,15),type="n",xlab="Age", ylab="Selectivity")
   for(i in 1:nrow(s1)){
     points(s1[i,3:17]~c(1:15),type="o",pch=i,col=i)
     points(10,div[(1+i)],pch=i,col=i)
     text(10,div[1+i],paste(Syrs[i], " Survey",sep=""),pos=4)
     }
   for(i in 1:nrow(f1)){
     points(f1[i,3:17]~c(1:15),type="o",pch=(i+14),col=(nrow(s1)+i))
     points(10,div[((nrow(s1)+1)+i)],pch=(i+14),col=(nrow(s1)+i))
     text(10,div[((nrow(s1)+1)+i)],paste(Fyrs[i], " Fishery",sep=""),pos=4)
     }
     points((mature_a*2)~c(1:15),type="l",col="black",lwd=2)
     points(c(9.5,10),c(0.1,0.1),type="l",lwd=2)
     text(10,div[1],"Maturity",pos=4)

     detach(dat)
 }

 ## historic biomass estimates
 old_biom<-function(dat,typ="TB",fyr=1978,lyr=2022){
  attach(dat)
  years<-c(fyr,lyr)


  if(typ=="TB"){
    data1<-read.csv("Old_TOTBIOM.csv")
    data1$Model_NEXT<-TotBiom[,2]
    names(data1)[length(names(data1)[!is.na(names(data1))])]<-paste0("Model_",lyr)
    titl=expression("Total biomass (t " %*%"10"^3 ~")" )
    }

  if(typ!="TB"){
    data1<-read.csv("Old_SPBIOM.csv")
    data1$Model_NEXT<-SSB[2:(nrow(SSB)-1),2]
    names(data1)[length(names(data1)[!is.na(names(data1))])]<-paste0("Model_",lyr)
    titl=expression("Female spawning biomass (t " %*%"10"^3 ~")" )

    }
   nyr<-ncol(data1)-1
  

  par(mar=c(4,5,0.1,0.1))
  
  data2<-melt(data1,"YEAR")

  d<-ggplot(data2,aes(x=YEAR,y=value,color=variable,group=variable,shape=variable,size=variable))+
     geom_point(size=1)+geom_line()+theme_bw(base_size=16)+scale_size_manual(values=c(rep(0.5,nyr-1),1.2))+
     scale_shape_manual(values=c(1:(nyr-1),20))+scale_color_manual(values=c(1:(nyr-1),"black"))+
     labs(x="Year",y=titl,color="Author's Model Year",shape="Author's Model Year",size="Author's Model Year")
 
    print(d)
    
  detach(dat)
}



bp.f.age.res<-function(dat,mod="1",typ="S",f=1,f1=1,lage=1,hage=15,siz=3,fyr=1978,lyr=2018){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      titl="Survey Standardized Residuals"
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f1,sep="")
      y<-paste("pobs_fsh_",f1,sep="")
      titl="Fishery Standardized Residuals"
      }

attach(dat)
require(lattice)
la<-2
#ha<-length(N[1,])

xx<-get(x)
yy<-get(y)
rr<-xx-yy
mean_rr<-mean(c(rr[,lage:hage]))
sd_rr<-sd(c(rr[,lage:hage]))

ha<-length(xx[1,])
ti<-(min(xx[,1]) )
tt<-max(1978,min(xx[,1]))-min(xx[,1])+1
sr<-rr-(mean_rr)/sd_rr

#windows(8,8)

     la<-lage+1
     ha<-length(xx[1,])
     plot(c(fyr,lyr),c(lage,hage),type="n",xlab="Year",ylab="Age",lab=c(10,10,7),main=titl)
     for (i in 1 : length(xx[,1])){
     for (j in lage:hage){
         if(sr[i,j]>0){symbols(xx[i,1],j,circles=(siz*sr[i,j]),inches=F,bg="red",fg="red",add=T)}
         if(sr[i,j]<0){symbols(xx[i,1],j,circles=(-siz*sr[i,j]),inches=F,fg="blue",add=T)}
         if(sr[i,j]==0){points(xx[i,1],j,pch=4)}
         }}
         mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
         print(paste0("Max= ",max(sr),"     Min = ",min(sr)))
        # windows(2,6)
        # d<-seq(max(sr),min(sr),length=10)
        # plot(seq(2,15,length=10),d,type="n",xlab="",ylab="Standardized Residuals",xaxt="n")
        # for ( i in 1:10){
        # if (d[i]>0){ symbols(8,d[i],circles=(3*d[i]),inches=F,fg="red",bg="red",add=T) }
        # if (d[i]<0){ symbols(8,d[i],circles=(-3*d[i]),inches=F,fg="blue",add=T) }
        # if (d[i]==0){ points(8,d[i],pch=4) }
        # }
         
         


detach(dat)
}



p.rec.hist<-function(dat,fy=1978,ly=2016,ylab=expression(paste("Recruits at age 2 (number x ",10^9," )",sep="")))
{
  attach(dat)
  Data<-R
  Data<-subset(Data,Data[,1]<=ly&Data[,1]>=fy)
  t2<-length(Data[,1])-3
  menr<-mean(Data[1:t2,2])
  ##windows(12,8)
  xx<-list(breaks=seq((fy-0.5),(ly+0.5),by=1),counts=c(fy:ly),
          intensities=as.numeric(Data[,2]),density=as.numeric(Data[,2]),
          mids=seq(fy,ly,by=1),xname="Year",equidist=TRUE)
          attr(xx,"class")="histogram"
          plot(xx,freq=F,ylab=ylab,
          xlim=c((fy-1),(ly+1)),
          ylim=c(0,(max(Data[,5])*1.1)),main="",col="#ffff00",lab=c(10,10,7))
          arrows(Data[,1],Data[,4],Data[,1],Data[,5],col="black",angle=90,code=3,length=0.05)
          lines(Data[,1],rep(menr,length(Data[,1])),type="l",lty=3,col="blue",lwd=2)
          # text(fy+2,(max(Data[,2])*1.1),paste("Mean recruitment for ",fy," to ",ly-1,sep=""),pos=4,cex=0.8)
          # text(fy+2,(max(Data[,2])*1.1),"--",col="blue",cex=1.5)
detach(dat)
}



bp.f.age.res<-function(dat=test2,mod="1",typ="S",f=1,f1=1,lage=1,hage=15,siz=3,fyr=1978,lyr=2018){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      titl="Survey Standardized Residuals"
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f1,sep="")
      y<-paste("pobs_fsh_",f1,sep="")
      titl="Fishery Standardized Residuals"
      }

attach(dat)
require(lattice)
la<-2


xx<-get(x)
yy<-get(y)
rr<-xx-yy
mean_rr<-mean(c(rr[,lage:hage]))
sd_rr<-sd(c(rr[,lage:hage]))

ha<-length(xx[1,])

ti<-(min(xx[,1]) )
tt<-max(1978,min(xx[,1]))-min(xx[,1])+1
sr<-rr-(mean_rr)/sd_rr

#windows(8,8)

     la<-lage+1
     ha<-length(N[1,])
     plot(c(fyr,lyr),c(lage,hage),type="n",xlab="Year",ylab="Age",lab=c(10,10,7),xlim=c(fyr,lyr),main=titl)
     for (i in 1 : length(xx[,1])){
     for (j in lage:hage){
         if(sr[i,j]>0){symbols(xx[i,1],j,circles=(siz*sr[i,j]),inches=F,bg="red",fg="red",add=T)}
         if(sr[i,j]<0){symbols(xx[i,1],j,circles=(-siz*sr[i,j]),inches=F,fg="blue",add=T)}
         if(sr[i,j]==0){points(xx[i,1],j,pch=4)}
         }}
         mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
         
        # windows(2,6)
        # d<-seq(max(sr),min(sr),length=10)
        # plot(seq(2,15,length=10),d,type="n",xlab="",ylab="Standardized Residuals",xaxt="n")
        # for ( i in 1:10){
        # if (d[i]>0){ symbols(8,d[i],circles=(3*d[i]),inches=F,fg="red",bg="red",add=T) }
        # if (d[i]<0){ symbols(8,d[i],circles=(-3*d[i]),inches=F,fg="blue",add=T) }
        # if (d[i]==0){ points(8,d[i],pch=4) }
        # }
         
         


detach(dat)
}