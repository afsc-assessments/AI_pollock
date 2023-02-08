library(RODBC)
library(mgcv)
setwd("C:/WORKING_FOLDER/2022 Stock Assessments/AI_pollock/R_functions")


  AFSC=odbcConnect("AFSC","sbarb","BluFsh$$7890",believeNRows=FALSE)
  CHINA=odbcConnect("AKFIN","sbarbeaux","$tockmen12",believeNRows=FALSE)

  source("get_Sage.r")
  source("get_AIbiom.r")
  source("get_Slength.r")
  source("find_AL.r")
  source("AGE_LENGTH.r")
  source("SWEIGHT.r")
  source("get_Fage.r")
  source("get_Sage.r")
  source("get_Flength.r")
  source("get_Dage.r")
  source("get_Dlength.r")
  source("write_dat.r")
  source("write_dat2.r")
  source("get_FSS.r")
  source("get_DSS.r")
  source("FWEIGHT.r")
  source("get_all_data.r")

 test=get_all_data(fyear=2022, species=21740,region=52,area="AI",MINAGE=100, filename="ai22_NEW.dat",WRITE=T)
 write_dat(data_file=="AI22.dat")

for (i in 1:10){
test=get_all_data(fyear=(2022-i), species=21740,region=52,area="AI",MINAGE=100, filename=paste("ai22_NEW_M",i,".dat",sep=""),WRITE=T)
 write_dat(data_file=paste("AI22M",i,".dat",sep=""))
}