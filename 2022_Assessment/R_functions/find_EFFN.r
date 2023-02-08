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

  Ot<-vector("list")
  Ot$EFFN_F<-mean(Fsh_N)
  Ot$EffN_I<-mean(Ind_N)
  Ot$RMSE<-sqrt(mean(log(data$Obs_Survey_1[,2]/data$Obs_Survey_1[,3])^2))
  Ot
}