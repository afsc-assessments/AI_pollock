p.bub.age<-function(dat,typ="NA",f=1,lage=2,hage=15,fy=1978,ly=2011,siz=0.1) ## input:  data file,lowest and highest age,first and last year, scaler for graphic
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
     la<-lage
     ha<-hage
     plot(c(lage,hage),c(fy,ly),type="n",ylab="Year",xlab="Age",lab=c(10,10,7),main=titl)

     for (i in tt : ti){
         points(c(lage:hage),rep(N[i,1],14),cex=c(x[i,la:ha]/siz))
         }
     detach(dat)
}




plot(test$sel_fsh_1[34,3:16]/max(test$sel_fsh_1[34,3:16])~c(2:15),type="o",pch=16,ylab="Selectivity",xlab="Age",cex.lab=1.5,cex.axis=1.2)
points(test$sel_fsh_1[21,3:16]/max(test$sel_fsh_1[21,3:16])~c(2:15),col="red",type="o",pch=17)
points(test$sel_fsh_1[1,3:16]/max(test$sel_fsh_1[1,3:16])~c(2:15),col="blue",type="o",pch=18)

points(test$sel_ind_1[1,3:16]/max(test$sel_ind_1[1,3:16])~c(2:15),col="purple",type="o",pch=1,lwd=2)
points(test$sel_ind_1[1,3:16]/max(test$sel_ind_1[1,3:16])~c(2:15),col="yellow",pch=16,cex=0.5)
points(test$mature_a*2~c(2:15),col="black",type="l",pch=1,lwd=3)
text(12,0.4,"Fishery 1978-1990",pos=4)
text(12,0.34,"Fishery 1991-1998",pos=4)
text(12,0.28,"Fishery 1999-2011",pos=4)
text(12,0.22,"Survey",pos=4)
text(12,0.16,"Maturity",pos=4)
points(12,0.4,pch=16)
points(12,0.34,col="red",pch=17)
points(12,0.28,col="blue",pch=18)
points(12,0.22,col="purple")
points(12,0.22,col="yellow",pch=16,cex=0.5)
points(c(11.9,12),c(0.16,0.16),type="l",lwd=3)
