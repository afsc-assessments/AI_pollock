
get_Sage<-function(region=52,species=21740,sy=1977){     

  test<-paste("SELECT RACEBASE.SPECIMEN.REGION,\n ",
                "TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR,\n ",
                "RACEBASE.SPECIMEN.CRUISE,\n ",
                "RACEBASE.HAUL.VESSEL,\n ",
                "RACEBASE.SPECIMEN.HAULJOIN,\n ",
                "RACEBASE.SPECIMEN.HAUL,\n ",
                "RACEBASE.SPECIMEN.SPECIES_CODE,\n ",
                "RACEBASE.SPECIMEN.LENGTH,\n ",
                "RACEBASE.SPECIMEN.SEX,\n ",
                "RACEBASE.SPECIMEN.WEIGHT,\n ",
                "RACEBASE.SPECIMEN.MATURITY,\n ",
                "RACEBASE.SPECIMEN.AGE,\n ",
                "RACEBASE.HAUL.END_LONGITUDE,\n ",
                "RACEBASE.HAUL.HAUL_TYPE,\n ",
                "RACEBASE.HAUL.GEAR,\n ",
                "RACEBASE.HAUL.PERFORMANCE,\n ",
                "RACEBASE.SPECIMEN.SPECIMEN_SAMPLE_TYPE,\n ",
                "RACEBASE.SPECIMEN.SPECIMENID,\n ",
                "RACEBASE.SPECIMEN.BIOSTRATUM\n ",
                "FROM RACE_DATA.V_CRUISES \n ",
                "INNER JOIN RACEBASE.HAUL \n ",
                "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n ",
                "INNER JOIN RACEBASE.SPECIMEN \n ",
                "ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN \n ",
                "AND RACEBASE.HAUL.CRUISEJOIN = RACEBASE.SPECIMEN.CRUISEJOIN \n ",
                "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID = ", region," \n ",
                "AND RACEBASE.HAUL.HAUL_TYPE = 3 \n ",
                "AND RACEBASE.HAUL.PERFORMANCE >= 0 \n ",
                "AND RACEBASE.HAUL.STATIONID                   IS NOT NULL \n ",
                "AND RACEBASE.SPECIMEN.SPECIES_CODE in (", species,")\n ",
                "AND RACE_DATA.V_CRUISES.YEAR >=",sy,"\n ",
                "ORDER BY RACE_DATA.V_CRUISES.YEAR",sep="")

     Age=sqlQuery(AFSC,test)
     Age
}


