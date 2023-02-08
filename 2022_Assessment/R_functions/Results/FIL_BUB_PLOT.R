 
fill_bub<-function(dat=readList("For_R.rep"),bub="F",fil="C",f=1,s=1,min_age=1,max_age=15, trans1="identity",trans2="identity"){
	## bubble and filled contour plot of any two age specific features.
	# N=number, C= Catch, F= fishing mortality, SF=select Fish, SS=select index, WS= weight survey, WF=weight Fisher, WC=weight catch, WP = population weight
	require(PBSadmb)
	require(ggplot2)
	AR<-(max_age-min_age)+1
	attach(dat)

	si<-paste("sel_ind_",s,sep="")
	sf<-paste("sel_fsh_",f,sep="")
	fa<-paste("F_age_",f,sep="")
	cf<-paste("C_fsh_",f,sep="")
	wts<-paste("wt_ind_",s,sep="")
	wtf<-paste("wt_fsh_",f,sep="")

 	NUM<-dat$N
 	F_A<-get(fa)
 	S_F<-get(sf)[,2:(AR+2)]
 	S_S<-get(si)[,2:(AR+2)]
 	C_F<-get(cf)
 	W_S<-get(wts)
 	W_F<-get(wtf)

	WC<-C_F*W_F
	WP<-NUM
	WP[,2:(AR+1)]<-WP[,2:(AR+1)]*(dat$wt_a_pop)
	detach(dat)

	NUM<-data.frame(NUM)
 	F_A<-data.frame(F_A)
 	S_F<-data.frame(S_F)
 	S_S<-data.frame(S_S)
 	C_F<-data.frame(C_F)
 	W_F<-data.frame(W_F)
 	W_S<-data.frame(W_S)
 	WC<-data.frame(WC)
 	WP<-data.frame(WP)
 
 
 	names(NUM)<-c("YEAR",paste("A",c(1:AR)))
 	names(F_A)<-c("YEAR",paste("A",c(1:AR)))
 	names(S_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(S_S)<-c("YEAR",paste("A",c(1:AR)))
 	names(C_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(W_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(W_S)<-c("YEAR",paste("A",c(1:AR)))
 	names(WC)<-c("YEAR",paste("A",c(1:AR)))
 	names(WP)<-c("YEAR",paste("A",c(1:AR)))

 	GRID_N<-data.frame(expand.grid(YEAR=NUM$YEAR,AGE=c(1:AR)))
 	GRID_N$N<-stack(NUM[,2:(AR+1)])$values
 	GRID_N$C_F<-stack(C_F[,2:(AR+1)])$values
 	GRID_N$F<-stack(F_A[,2:(AR+1)])$values
 	GRID_N$SEL_F<-stack(S_F[,2:(AR+1)])$values
 	GRID_N$SEL_S<-stack(S_S[,2:(AR+1)])$values
 	GRID_N$WT_F<-stack(W_F[,2:(AR+1)])$values
 	GRID_N$WT_S<-stack(W_S[,2:(AR+1)])$values
 	GRID_N$WC<-stack(WC[,2:(AR+1)])$values
 	GRID_N$WP<-stack(WP[,2:(AR+1)])$values
 
 	dat_p<-data.frame(YEAR=GRID_N$YEAR,AGE=GRID_N$AGE)
 	if(fil=="N"){dat_p$fil=GRID_N$N
 				titl1<-"Number"}
	if(fil=="F"){dat_p$fil=GRID_N$F 
 				titl1<-"F"}
	if(fil=="C"){dat_p$fil=GRID_N$C_F 
 				titl1<-paste("Fish_",f," Catch ",sep="")}
 	if(fil=="SF"){dat_p$fil=GRID_N$SEL_F 
 				titl1<-paste("Fish_",f," Select ",sep="")}
 	if(fil=="SS"){dat_p$fil=GRID_N$SEL_S 
 				titl1<-paste("Indx_",s," Select ",sep="")}
 	if(fil=="WF"){dat_p$fil=GRID_N$WT_F 
 				titl1<-paste("Fish_",f," Wt ",sep="")}
 	if(fil=="WS"){dat_p$fil=GRID_N$WT_S 
				titl1<-paste("Indx_",s," Wt ",sep="")}
 	if(fil=="WC"){dat_p$fil=GRID_N$WC 
				titl1<-paste("Catch_",f," Wt ",sep="")}
 	if(fil=="WP"){dat_p$fil=GRID_N$WP 
				titl1<-paste("Pop Wt ",sep="")}

 
	if(bub=="N"){dat_p$bub=GRID_N$N
 				titl2<-"Number"}
 	if(bub=="F"){dat_p$bub=GRID_N$F 
 				titl2<-"F"}
 	if(bub=="C"){dat_p$bub=GRID_N$C_F 
 				titl2<-paste("Fish_",f," Catch ",sep="")}
 	if(bub=="SF"){dat_p$bub=GRID_N$SEL_F 
 				titl2<-paste("Fish_",f," Select ",sep="")}
 	if(bub=="SS"){dat_p$bub=GRID_N$SEL_S 
 				titl2<-paste("Indx_",s," Select ",sep="")}
 	if(bub=="WF"){dat_p$bub=GRID_N$WT_F 
 				titl2<-paste("Fish_",f," Wt ",sep="")}
 	if(bub=="WS"){dat_p$bub=GRID_N$WT_S 
				titl2<-paste("Indx_",s," Wt ",sep="")}
 	if(bub=="WC"){dat_p$bub=GRID_N$WC 
				titl2<-paste("Catch_",f," Wt ",sep="")}
 	if(bub=="WP"){dat_p$bub=GRID_N$WP 
				titl2<-paste("Pop Wt ",sep="")}

 	p=ggplot(dat_p,aes(x=YEAR,y=AGE,size=bub,fill=fil))
 	p + geom_tile()+scale_fill_continuous(low="white",high="blue",name=paste(titl1),trans=paste(trans1))+geom_point(pch=1,colour="red", fill="gray90")+scale_size(trans=paste(trans2),name=paste(titl2))+theme(legend.background=element_rect(size=4,fill="gray95"))
   
}

#source(paste(getwd(),"/R_functions/Results/FIL_BUB_PLOT.R",sep=""))


p.bub<-function(dat=readList("For_R.rep"),bub="F",f=1,s=1,min_age=1,max_age=15, trans1="identity"){
	require(PBSadmb)
	require(ggplot2)
	AR<-(max_age-min_age)+1
	attach(dat)

	si<-paste("sel_ind_",s,sep="")
	sf<-paste("sel_fsh_",f,sep="")
	fa<-paste("F_age_",f,sep="")
	cf<-paste("C_fsh_",f,sep="")
	wts<-paste("wt_ind_",s,sep="")
	wtf<-paste("wt_fsh_",f,sep="")

 	NUM<-dat$N
 	F_A<-get(fa)
 	S_F<-get(sf)[,2:(AR+2)]
 	S_S<-get(si)[,2:(AR+2)]
 	C_F<-get(cf)
 	W_S<-get(wts)
 	W_F<-get(wtf)

	WC<-C_F*W_F
	WP<-NUM
	WP[,2:(AR+1)]<-WP[,2:(AR+1)]*(dat$wt_a_pop)
	detach(dat)

	NUM<-data.frame(NUM)
 	F_A<-data.frame(F_A)
 	S_F<-data.frame(S_F)
 	S_S<-data.frame(S_S)
 	C_F<-data.frame(C_F)
 	W_F<-data.frame(W_F)
 	W_S<-data.frame(W_S)
 	WC<-data.frame(WC)
 	WP<-data.frame(WP)
 
 
 	names(NUM)<-c("YEAR",paste("A",c(1:AR)))
 	names(F_A)<-c("YEAR",paste("A",c(1:AR)))
 	names(S_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(S_S)<-c("YEAR",paste("A",c(1:AR)))
 	names(C_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(W_F)<-c("YEAR",paste("A",c(1:AR)))
 	names(W_S)<-c("YEAR",paste("A",c(1:AR)))
 	names(WC)<-c("YEAR",paste("A",c(1:AR)))
 	names(WP)<-c("YEAR",paste("A",c(1:AR)))

 	GRID_N<-data.frame(expand.grid(YEAR=NUM$YEAR,AGE=c(1:AR)))
 	GRID_N$N<-stack(NUM[,2:(AR+1)])$values
 	GRID_N$C_F<-stack(C_F[,2:(AR+1)])$values
 	GRID_N$F<-stack(F_A[,2:(AR+1)])$values
 	GRID_N$SEL_F<-stack(S_F[,2:(AR+1)])$values
 	GRID_N$SEL_S<-stack(S_S[,2:(AR+1)])$values
 	GRID_N$WT_F<-stack(W_F[,2:(AR+1)])$values
 	GRID_N$WT_S<-stack(W_S[,2:(AR+1)])$values
 	GRID_N$WC<-stack(WC[,2:(AR+1)])$values
 	GRID_N$WP<-stack(WP[,2:(AR+1)])$values
 
 	dat_p<-data.frame(YEAR=GRID_N$YEAR,AGE=GRID_N$AGE)
 	 
	if(bub=="N"){dat_p$bub=GRID_N$N
 				titl2<-"Number"}
 	if(bub=="F"){dat_p$bub=GRID_N$F 
 				titl2<-"F"}
 	if(bub=="C"){dat_p$bub=GRID_N$C_F 
 				titl2<-paste("Fish_",f," Catch ",sep="")}
 	if(bub=="SF"){dat_p$bub=GRID_N$SEL_F 
 				titl2<-paste("Fish_",f," Select ",sep="")}
 	if(bub=="SS"){dat_p$bub=GRID_N$SEL_S 
 				titl2<-paste("Indx_",s," Select ",sep="")}
 	if(bub=="WF"){dat_p$bub=GRID_N$WT_F 
 				titl2<-paste("Fish_",f," Wt ",sep="")}
 	if(bub=="WS"){dat_p$bub=GRID_N$WT_S 
				titl2<-paste("Indx_",s," Wt ",sep="")}
 	if(bub=="WC"){dat_p$bub=GRID_N$WC 
				titl2<-paste("Catch_",f," Wt ",sep="")}
 	if(bub=="WP"){dat_p$bub=GRID_N$WP 
				titl2<-paste("Pop Wt ",sep="")}

 	p=ggplot(dat_p,aes(x=YEAR,y=AGE,size=bub))
 	p +geom_point(pch=1, fill="gray90")+scale_size(trans=paste(trans1),name=paste(titl2))+theme(legend.background=element_rect(size=4,fill="gray95"))
   
}


p.sur.stk<-function(dat,f=1,ylab="Index", yf=1978,yl=2013)
  {
  attach(test)
  require(ggplot2)
  x<-paste("Obs_Survey_",f,sep="")
  Data1<-data.frame(get(x))
  names(Data1)<-c("YEAR","Obs","Pred","SD","x1","x2")
  Data1$ymin<-Data1$Obs-Data1$SD*1.96
  Data1$ymax<-Data1$Obs+Data1$SD*1.96

  p<-ggplot(Data1,aes(x=YEAR,y=Obs,ymin=ymin,ymax=ymax))
  p+geom_pointrange(cex=1.25)+geom_line(aes(y=Pred),lwd=1.5,colour="red",name="Predicted")



  #d1<-subset(Data,Data[,2]>0)
  d1<-Surv_Biom
  
  windows(8,8)
  plotCI(d1[,1],d1[,2],2*d1[,3],
  main=Index_names[f],
  ylim=c(0,(max(Data[,3],max(d1[,2])*1.9))),xlim=c(yf,yl),pch=19,
  ylab=ylab,
  xlab="Year",col="green",scol="black",lab=c(10,10,7))
  points(Data[,2]~Data[,1],pch=16,col="blue")

  points(Data[,3]~Data[,1],type="l",col="red",lwd=2)
  text(min(Data[,1])+2,(max(d1[,2])*1.6),"Survey Point Estimates Not Fit",pos=4,cex=0.8)
   text(min(Data[,1])+2,(max(d1[,2])*1.5),"Survey Point Estimates Fit",pos=4,cex=0.8)
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"Model Fit",pos=4,cex=0.8)
  points(min(Data[,1])+2,(max(d1[,2])*1.6),pch=16,col="green")
 
  points(min(Data[,1])+2,(max(d1[,2])*1.5),pch=19,col="blue")
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"--",cex=1.5,col="red")
  detach(dat)

  }