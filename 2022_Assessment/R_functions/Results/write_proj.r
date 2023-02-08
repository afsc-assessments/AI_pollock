write_PROJ<-function(file1=data_file_name,spcat=spcat,setup=setup){

  year=as.numeric(format(Sys.Date(), "%Y"))
  years<-c(year,year+13)
  Proj_catch<-c( CATCH$CATCH_TOTAL$TONS[CATCH$CATCH_TOTAL$YEAR==year]/1000,rep(19.00,13))
  Proj_catch<-data.frame(YEAR=years,CATCH=Proj_catch)


  write(setup,paste(getwd(),"/PROJ_FILES/setup.dat",sep=""))
  
  write(noquote(as.character(spcat))[1:4],paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""))
  write(noquote(paste("data/",file1,".dat",sep="")),paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),append = T)
  write(noquote(as.character(spcat[5:9])),paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),append = T)
  write.table(Proj_catch,paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),row.names=F,col.names=F,append = T)
}




#source("functions2011/write_proj.r")
#write_PROJ()
  year=as.numeric(format(Sys.Date(), "%Y"))
  years<-c(year,year+13)         
  Proj_catch<-c(CATCH$CATCH_TOTAL$TONS[CATCH$CATCH_TOTAL$YEAR==year]/1000,rep(19.00,13))
  Proj_catch<-data.frame(YEAR=years,CATCH=Proj_catch)


  write(setup,paste(getwd(),"/PROJ_FILES/setup.dat",sep=""))
  write(noquote(as.character(spcat))[1:4],paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""))
  write(noquote(paste("data/",data_file_name,".dat",sep="")),paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),append = T)
  write(noquote(as.character(spcat[5:9])),paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),append = T)
  write.table(Proj_catch,paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""),row.names=F,col.names=F,append = T)
  remove(year)





file.copy(paste(getwd(),"/proj.dat",sep=""), paste(getwd(),"/proj/data/",sep=""),overwrite=T)

file.copy(paste(getwd(),"/PROJ_FILES/A_spcat.dat",sep=""), paste(getwd(),"/proj/data/",sep=""),overwrite=T)
file.copy(paste(getwd(),"/PROJ_FILES/setup.dat",sep=""), paste(getwd(),"/proj/setup.dat",sep=""),overwrite=T)

file.rename(paste(getwd(),"/proj/data/A_spcat.dat",sep=""), paste(getwd(),"/proj/data/",data_file_name,"_spcat.dat",sep=""))

file.rename(paste(getwd(),"/proj/data/proj.dat",sep=""), paste(getwd(),"/proj/data/",data_file_name,".dat",sep=""))

#system("cd proj",sep="")

#system(paste("run1 ",data_file_name,sep=""))
#system("cd..:)