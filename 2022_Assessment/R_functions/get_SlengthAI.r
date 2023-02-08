
get_SlengthAI<-function(sp=species){
 		require(data.table)
 		sizecomp<-paste0("SELECT AI.SIZECOMP_AREA.YEAR,
  				AI.SIZECOMP_AREA.LENGTH,
  				AI.SIZECOMP_AREA.MALES,
  				AI.SIZECOMP_AREA.FEMALES,
  				AI.SIZECOMP_AREA.UNSEXED,
  				AI.SIZECOMP_AREA.TOTAL
				FROM AI.SIZECOMP_AREA
				WHERE AI.SIZECOMP_AREA.SPECIES_CODE       =", sp,
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
		Ssize<-data.frame(Ssize[,list(FREQUENCY=sum(FREQ)),by=c("YEAR","LENGTH","SEX")])
		Nc<-nchar(as.character(max(Ssize$FREQUENCY)))-4
		Ssize$FREQUENCY<-round(Ssize$FREQUENCY/(10^Nc))
		Ssize
}

