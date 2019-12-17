Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
names(Losses) <- tolower(names(Losses))
names(Losses)[ names(Losses) == "measureditemsuafbs"] <-  "measureditemcpc"

ctycomd <-Losses[(flagobservationstatus %in% c(" ","")) &(timepointyears>1990),c("geographicaream49","measureditemcpc", "timepointyears"),with=F]
ctycomd$geographicaream49 <- as.character(ctycomd$geographicaream49 )
ctycomd[,combo := paste(geographicaream49, measureditemcpc, sep=";")]
ctycomd2 <- unique(ctycomd$combo)
length( unique(ctycomd$combo))
length(unique(ctycomd$geographicaream49))
length(unique(ctycomd$measureditemcpc))

CountryGroup$geographicaream49 <- CountryGroup$m49_code
ctycomd <- join(ctycomd, CountryGroup[,c("geographicaream49","m49_region")], by = "geographicaream49", type = "left")
FAOCrops$measureditemcpc <-  FAOCrops$cpc

ctycomd <-  join(ctycomd,unique(FAOCrops[,c("measureditemcpc","description")]), by = "measureditemcpc", type = "left")


ctycomd[,combo := paste(m49_region, description, sep=";")]

CtryofficialReport <- as.data.table(ctycomd %>%
  filter(timepointyears>2015) %>%
  group_by(combo) %>%
  dplyr:: summarise(nYears= n()))

CtryofficialReport[,c("ctry","crp") := tstrsplit(combo, ";",fixed =TRUE)]

length( unique(CtryofficialReport[nYears>1,]))
length(unique(CtryofficialReport$ctry))
length(unique(CtryofficialReport$crp))

write.table(CtryofficialReport, "ReportingCountries_13May19.csv", sep= ",")
