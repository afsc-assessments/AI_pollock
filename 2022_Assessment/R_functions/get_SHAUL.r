get_SHAUL<-function(sy=1990, species=21740){

  test<-paste0("SELECT
	racebase.haul.region,
    	ai.cpue.year,
    	TO_CHAR(RACEBASE.HAUL.START_TIME,'HH24') AS TIME,
    	racebase.haul.cruise,
    	racebase.haul.vessel,
    	racebase.haul.haul,
    	racebase.haul.end_latitude,
    	racebase.haul.end_longitude,
    	racebase.haul.stratum,
    	racebase.haul.bottom_depth,
    	racebase.haul.gear_temperature,
    	ai.cpue.wgtcpue AS cpue
   FROM
    	ai.cpue
    	INNER JOIN racebase.haul ON ai.cpue.hauljoin = racebase.haul.hauljoin
                                AND ai.cpue.cruise = racebase.haul.cruise
                                AND ai.cpue.vessel = racebase.haul.vessel
                                AND ai.cpue.haul = racebase.haul.haul
   WHERE
    	ai.cpue.species_code = ",species,
    	"AND racebase.haul.abundance_haul = 'Y'
    	AND ai.cpue.year > ",sy)

 haul=sqlQuery(AFSC,test)
 haul
}



