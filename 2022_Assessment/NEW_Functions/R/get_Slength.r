get_Slength<-function(region=52,species=21740,sy=1977){


  test<-paste("SELECT RACEBASE.LENGTH.REGION,\n ",
                "TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR,\n ",
                "RACEBASE.LENGTH.CRUISE,\n ",
                "RACEBASE.LENGTH.VESSEL,\n ",
                "RACEBASE.LENGTH.HAULJOIN,\n ",
                "RACEBASE.LENGTH.HAUL,\n ",
                "RACEBASE.LENGTH.SPECIES_CODE,\n ",
                "RACEBASE.LENGTH.SEX,\n ",
                "RACEBASE.LENGTH.LENGTH,\n ",
                "RACEBASE.LENGTH.FREQUENCY,\n ",
                "RACEBASE.HAUL.END_LONGITUDE,\n ",
                "RACEBASE.HAUL.GEAR,\n ",
                "RACEBASE.HAUL.STRATUM,\n ",
                "RACEBASE.HAUL.HAUL_TYPE\n ",
                "FROM RACE_DATA.V_CRUISES \n ",
                "INNER JOIN RACEBASE.HAUL \n ",
                "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n ",
                "INNER JOIN RACEBASE.LENGTH \n ",
                "ON RACEBASE.HAUL.CRUISEJOIN                    = RACEBASE.LENGTH.CRUISEJOIN \n ",
                "AND RACEBASE.HAUL.HAULJOIN                     = RACEBASE.LENGTH.HAULJOIN \n ",
                "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID = ", region," \n ",
                "AND RACEBASE.HAUL.HAUL_TYPE                    = 3 \n ",
                "AND RACEBASE.HAUL.PERFORMANCE                 >= 0 \n ",
                "AND RACEBASE.HAUL.STATIONID                   IS NOT NULL \n ",  
                "AND RACEBASE.LENGTH.SPECIES_CODE in (",species,")\n ",
                "AND RACE_DATA.V_CRUISES.YEAR >= ",sy,sep="")

 Length=sqlQuery(AFSC,test)
 Length
}



