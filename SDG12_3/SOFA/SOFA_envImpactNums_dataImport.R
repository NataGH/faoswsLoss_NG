
library(readxl)
library(faoswsLoss)
library(shiny)
library(shinythemes)
library(rmarkdown)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)
library(plotly)
library(yaml)
library(rdrop2)


suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(plyr)
  library(dplyr)
  
})


############# Computation Parameters #####################################
savesws <- TRUE
LocalRun <- TRUE # For if you are running the model on a local environment and loading data tables from local fiiles

if(CheckDebug()){
  maxYear <- format(Sys.Date(), "%Y")
  selectedYear <- as.character(1991:2016)
  ReportingYear<-  as.character(c(2015))
  aggregation <-  "geographicaream49"
  weights <- "intl_prices"
  basketn <- "top2perhead_byCtry"
  ComparisonYear <- (c(2005,2016))
  gfli_Reporting <- TRUE
  gfli_compare <- TRUE
}

#####################

BaseYear = as.character(c(2014,2016)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)


##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
if(CheckDebug()){
  message("Not on server, so setting up environment...")
  USER <- if_else(.Platform$OS.type == "unix",
                  Sys.getenv('USER'),
                  Sys.getenv('USERNAME'))
  
  
  library(faoswsModules)
  settings <- ReadSettings(file = file.path(paste(getwd(),"sws.yml", sep='/')))
  SetClientFiles(settings[["certdir"]])
  
  
  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )
  
  
  
}else if(CheckDebug() & LocalRun){
  #Load local last dataset
  load("InputData.RData")
  
}else{
  # Remove domain from username
  USER <- regmatches(
    swsContext.username,
    regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
  )
  
  options(error = function(){
    dump.frames()
    
    filename <- file.path(Sys.getenv("R_SWS_SHARE_PATH"), USER, "PPR")
    
    dir.create(filename, showWarnings = FALSE, recursive = TRUE)
    
    save(last.dump, file = file.path(filename, "last.dump.RData"))
  })
}


FWF_Impact_factors <- as.data.table(read_excel("~/faoswsLossa/data-raw/SOFA/FWF Impact factors.xlsx", sheet = "Database"))
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
gfli_basket <- ReadDatatable("gfli_basket")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
Loss_per_stage <- ReadDatatable("sn_vc_est")
Loss_per_stage_envF <- ReadDatatable("snv_environ_factors")


LossPer_SVC <- ReadDatatable("sn_vc_est")
FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]

load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markovadj_Protected.RData")
#load("~/faoswsLossa/shiny/Soup2Nuts/timeSeriesDataToBeImputed_5126_jan_all_markov_1.RData")

Losses <- timeSeriesDataToBeImputed_5126 %>% filter(timePointYears <= 2016)



names(FWF_Impact_factors)
FWF_Impact_factors[, c("# rows" ,"# columns","Commodity name abbreviation" ,
  "Region * Commodity" ,"Sub-region * Sub-commodity" ,"Region * Commodity * Phase",
  "Sub-region * Sub-commodity * Phase" ,"Region * Sub-commodity","**END**"  ):= NULL ]

keep <- c("Sub region name" , "Sub region #" ,"FSC Step name","Sub production name","Sub production #","Emission factors\r\n(kg CO2 eq.  / kg food)" ,
          "Carbon footprint\r\n(1000 tons CO2 eq.)","BLUE Water - Impact factors\r\n(m3  / ton food)","GREEN Water - Impact factors\r\n(m3  / ton food)",                            
          "GREY Water - Impact factors\r\n(m3  / ton food)", "Land use - Impact factors\r\n(Ha  / ton food)", "Economic assessment - Impact factors\r\n (USD / kg of food)",
           "fsc_location", "geographicaream49" ,"measureditemcpc"   )

keepNew <- c("Sub_region_name" , "Sub_region_Num" ,"FSC_Step_name","Sub_production_name","Sub_production_Num","emission_kgco2perkgfood" ,
          "carbon_1000tonsco2eq","water_blue_m3_tonfood","water_green_m3_tonfood",                            
          "water_grey_m3_tonfood", "land_ha_tonfood", "econ_USD_kgfood",
          "fsc_location", "geographicaream49" ,"measureditemcpc"   )



FWF_Impact_factors$fsc_location <- ""
FWF_Impact_factors[FWF_Impact_factors$`FSC Step name` == "Agricultural production", fsc_location := "farm" ] 
FWF_Impact_factors[FWF_Impact_factors$`FSC Step name` == "Postharvest handling and storage" , fsc_location := "storage" ] 
FWF_Impact_factors[FWF_Impact_factors$`FSC Step name` == "Processing", fsc_location := "processing" ]                 
FWF_Impact_factors[FWF_Impact_factors$`FSC Step name` == "Distribution", fsc_location := "wholesale" ]                   
FWF_Impact_factors[FWF_Impact_factors$`FSC Step name` == "Consumption", fsc_location := "consumer" ] 


FWF_Impact_factors$geographicaream49 <- "" 
FWF_Impact_factors[FWF_Impact_factors$`Sub region name` ==  "USA", "Sub region name"] <- "United States of America"

R2_1 <-  c("Australia")
           
R2_2 <- "Canada"
R2_3 <- "New Zealand"
)
          
R2_4 <- "United States of America"
R3_1 <- c("China",'China, Macao Special Administrative Region','China, Hong Kong Special Administrative Region')
R3_2 <- "Japan"
R3_3 <-"Republic of Korea" 
R5_2 <- c("Mongolia", "Democratic People's Republic of Korea")


R1_1 <- c('Albania','Austria', 'Belarus','Belgium','Bosnia and Herzegovina','Bulgaria','Croatia','Denmark','Estonia','Finland','France','Germany',
    'Greece','Hungary','Iceland','Ireland','Italy','Latvia','Lithuania','Luxembourg','Malta','Montenegro','Netherlands','Norway','Poland','Portugal','Republic of Moldova',
    'Romania','Russian Federation','Serbia','Slovakia','Slovenia','Spain','Sweden','Switzerland','The former Yugoslav Republic of Macedonia','Ukraine',"Czechia",
    "United Kingdom of Great Britain and Northern Ireland",'Aland Islands','Channel Islands','Faroe Islands','Guernsey','Isle of Man','Jersey','Svalbard and Jan Mayen Islands','Andorra',
    'Gibraltar','Holy See', 'San Marino','Liechtenstein','Monaco')

R4_1 <- c('Burundi','Comoros','Djibouti','Eritrea','Ethiopia','Kenya','Madagascar','Malawi','Mauritius','Mozambique','Rwanda','Seychelles',
  'Uganda','United Republic of Tanzania', 'Zambia', 'Zimbabwe','British Indian Ocean Territory','French Southern and Antarctic Territories',  'Mayotte',
  'Reunion',  'Somalia',  'South Sudan')

R4_2 <- c('Angola','Cameroon','Central African Republic','Chad','Congo','Democratic Republic of the Congo','Gabon','Sao Tome and Principe','Equatorial Guinea')
R4_3 <- c('Botswana','Lesotho','Namibia','South Africa','Swaziland')
R4_4<- c('Benin','Burkina Faso',"Cabo Verde","Cote d'Ivoire",'Gambia','Ghana','Guinea','Guinea-Bissau','Liberia','Mali','Mauritania','Niger','Nigeria','Senegal','Sierra Leone',
         'Togo','Saint Helena')
         
R5_1 <- c('Kazakhstan','Kyrgyzstan','Tajikistan','Turkmenistan','Uzbekistan')
R5_3 <- c('Algeria','Egypt','Libya','Morocco','Sudan','Tunisia','Western Sahara')

R5_4 <- c('Armenia','Azerbaijan','Cyprus','Georgia','Israel','Jordan','Kuwait','Lebanon',"State of Palestine",'Saudi Arabia','Syrian Arab Republic','Turkey',
          'United Arab Emirates','Yemen','Bahrain','Iraq','Oman', 'Qatar')
R6_1 <- c('Brunei Darussalam','Cambodia','Indonesia',"Lao People's Democratic Republic", 'Malaysia','Myanmar','Philippines','Thailand','Timor-Leste','Viet Nam','Singapore')

R6_2 <- c('Bangladesh','India','Iran (Islamic Republic of)','Maldives','Nepal','Pakistan','Sri Lanka','Afghanistan','Bhutan')

R7_1 <- c('Antigua and Barbuda','Bahamas','Barbados','Cuba','Dominica','Dominican Republic','Grenada','Haiti','Jamaica',"Curacao","Aruba","Bonaire, Sint Eustatius and Saba",
          'Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Trinidad and Tobago','Anguilla','British Virgin Islands','Cayman Islands',
          'Guadeloupe','Martinique','Montserrat','Puerto Rico','Saint Barthelemy','Saint Martin (French Part)','Sint Maarten (Dutch part)','Turks and Caicos Islands',
          'United States Virgin Islands', "Christmas Island","Cocos (Keeling) Islands", "Heard Island and McDonald Islands","Norfolk Island",'Fiji',
          'New Caledonia','Papua New Guinea','Solomon Islands','Vanuatu','Guam','Kiribati','Marshall Islands','Micronesia (Federated States of)',
          'Nauru','Northern Mariana Islands','Palau','United States minor outlying islands','Bermuda','Greenland','Saint Pierre and Miquelon','American Samoa',
          'Cook Islands','French Polynesia','Niue','Pitcairn','Samoa','Tokelau','Tonga','Tuvalu','Wallis and Futuna Islands')

R7_2 <- c('Belize','Costa Rica','El Salvador','Guatemala','Honduras','Mexico','Nicaragua','Panama')

R7_3 <- c('Argentina','Bolivia (Plurinational State of)','Brazil','Chile','Colombia','Ecuador','Guyana','Paraguay','Peru','Suriname','Uruguay','Venezuela (Bolivarian Republic of)',
          'Falkland Islands (Malvinas)','French Guiana','South Georgia and the South Sandwich Islands')


# length(R7_3)
# CountryGroup[CountryGroup$m49_region %in% R7_3 ,"m49_region",with=F]
# grep("Aru",CountryGroup$m49_region, value=T)

unique(FWF_Impact_factors$`Sub production name`)

gfli1 <- merge(gfli_basket, unique(FAOCrops[,c("measureditemcpc","crop"),with=F]), by ="measureditemcpc", all.x=T)
FWF_Impact_factors$measureditemcpc <- "" 
C1_1 <- c("0111","0116") #Wheat + Rye
C1_2 <- c("0115","0117", "01191", "01192","01193","01194","01195","01199.02","01199.90") #Oats + Barley + Cereals, other
C1_3 <- c( "0112")
C1_4 <- c( "0113")

C1_5  <- c("0118", "0114") #Millet + Sorghum

C2_1 <- c("01510","01520.01","01520.02","01530","01540","01550","01591","01599.1","01599.2","21313") #Starchy roots
C3_1 <- c("0141", "0142","0143","01441","01442","01443","01444","01445","01446","01447","01448","01449.01", "01449.02",
          "01449.9","01450","01460","01491.01","01491.02","01492","01499.01","01499.02","01499.03","01499.04","01499.05",
          "01499.06","01499.07") #	Oilcrops
C3_2 <- c("01701","01702","01703","01704","01705","01706","01707","01708","01709.01","01709.02","01709.90") #	Pulses

C4_1<- c("01341")
C4_2<- c("01312")
C4_3<- c("01321","01322", "01323","01324","01329")
C4_4<- c("01330")

C4_5 <- c( "01311","01312","01313","01314","01315","01316","01317","01318","01319","01330","01341","01342.01","01342.02","01343","01344.01", 
            "01344.02","01345","01346","01349.10","01349.20","01351.01","01351.02","01352","01353.01","01354" ,
            "01355.01","01355.02","01355.90","01359.01","01359.02","01359.90","21411","21412","21419.01","21419.02", 
            "21419.91","21419.99","21431.01","21431.02","21432","21432.01","21433","21433.01","21434","21435.01", 
            "21435.02","21439.01","21439.02","21439.03","21439.04","21439.05","21439.06","21439.07","21439.08", "21439.9" ,
            "21491","23170.04","23670.02","23991.03","24212.01","F0623") #	Fruits, other

C8_1 <- c("01211","01212","01213","01214","01215","01216","01219.01","01221","01229","01231","01232","01233","01234","01235",
          "01239.01","01241.01","01241.90","01242","01243","01251","01252","01253.01","01253.02","01254","01270","01290.01",
          "01290.90","01356","01691","21319.01" ,"21321","21329","21330.90","21340","21393.01","21393.90","21397.01","21399.01",
          "21399.02","21399.03","23912.01","23991.02","F0472","F0473","F0475") #	Vegetables

C5_1 <- c("21111.01","21111.02") #	Bovine Meat
C5_2 <- c("21115","21116") #	Mutton & Goat Meat
C5_3 <- c( "21113.01","21113.02","21181","F1042") #	Pig Meat
C5_4 <- c("21121","21122","21123","21124","21170.01","F1061") #	Poultry Meat
C7_1<- c( "02211","02212","02291","02292","02293") # Milk
C7_2<- c( "0231","0232")

#grep( "egg",gfli1$crop, value=T)
#gfli_basket == "Fruits & Vegetables"
#gfli1[crop %in% c("hen eggs in shell, fresh"    ,                  "eggs from other birds in shell, fresh, n.e.c.") ,"measureditemcpc",with=F]

FWF_Impact_factors <- FWF_Impact_factors[,keep, with=F] 
setnames(FWF_Impact_factors, old = keep, new= keepNew)
########### Data Table #############
loc2 <- c("farm","transport","storage", "trader","wholesale", "processing", "retail")
maxYear <- as.numeric(format(Sys.Date(), "%Y"))

SubNat_Est <- as.data.table(expand.grid(geographicaream49 = as.character(unique(CountryGroup$m49_code)),
                                        measureditemcpc = as.character(unique(gfli_basket$measureditemcpc)),
                                        locations = as.character(loc2)))
SubNat_Est$emission_kgco2perkgfood =0
SubNat_Est$carbon_1000tonsco2eq = 0
SubNat_Est$water_blue_m3_tonfood = 0
SubNat_Est$water_green_m3_tonfood = 0
SubNat_Est$water_grey_m3_tonfood = 0
SubNat_Est$land_ha_tonfood = 0
SubNat_Est$econ_USD_kgfood = 0

regiongrp <-list(R2_1,R2_2, R2_3,R2_4,R3_1,R3_2,R3_3, R5_2, R1_1,R4_1,R4_2,R4_3,R4_4,R5_1,R5_3,R5_4,R6_1,R6_2,R7_1,R7_2,R7_3)
regiongrpN <- c("R2_1","R2_2", "R2_3","R2_4","R3_1","R3_2","R3_3", "R5_2", "R1_1","R4_1","R4_2","R4_3","R4_4","R5_1","R5_3","R5_4","R6_1","R6_2","R7_1","R7_2","R7_3")
cmdgrp <- list(C1_1,C1_2,C1_3,C1_4,C1_5,C2_1,C3_1, C3_2,C4_1, C4_2,C4_3, C4_4,C4_5,C5_1, C5_2,C5_3,C5_4,C7_1,C7_2,C8_1)
cmgrpn <- c("C1_1","C1_2","C1_3","C1_4","C1_5","C2_1","C3_1", "C3_2","C4_1", "C4_2","C4_3", "C4_4","C4_5","C5_1", "C5_2","C5_3","C5_4","C7_1","C7_2","C8_1")



SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(R1_1) ,"m49_code",with=F])) & 
             (measureditemcpc  %in% unlist(cmdgrp[1])) & 
             (locations %in% loc2[1])
           ,]

FWF_Impact_factors[(Sub_region_Num ==  regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[1]) & (fsc_location %in% loc2[1]),]

for(k in 1:length(regiongrpN)){ 
for(j in 1:length(cmgrpn)){                   
for(i in 1:length(loc2)){
  #print(R1_1)
  if(dim(FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num == cmgrpn[j]) & (fsc_location %in% loc2[i]),])[1]>0){
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in%  unlist(regiongrp[k]) ,"m49_code",with=F])) & 
               (measureditemcpc  %in%  unlist(cmdgrp[j])) & 
               (locations %in% loc2[i])
               , "emission_kgco2perkgfood"]=
    FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "emission_kgco2perkgfood",with= F]
    
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]) ,"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "carbon_1000tonsco2eq"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num == cmgrpn[j]) & (fsc_location %in% loc2[i]),  "carbon_1000tonsco2eq",with= F]
    
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]) ,"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "water_blue_m3_tonfood"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "water_blue_m3_tonfood",with= F]
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]) ,"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "water_green_m3_tonfood"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "water_green_m3_tonfood",with= F]
    
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]),"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "water_grey_m3_tonfood"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "water_grey_m3_tonfood",with= F]
    
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]) ,"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "land_ha_tonfood"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "land_ha_tonfood",with= F]
    
    SubNat_Est[(geographicaream49 %in% unlist(CountryGroup[CountryGroup$m49_region %in% unlist(regiongrp[k]) ,"m49_code",with=F])) & 
                 (measureditemcpc  %in% unlist(cmdgrp[j])) & 
                 (locations %in% loc2[i])
               , "econ_USD_kgfood"]=
      FWF_Impact_factors[(Sub_region_Num == regiongrpN[k]) & ( Sub_production_Num ==  cmgrpn[j]) & (fsc_location %in% loc2[i]),  "econ_USD_kgfood",with= F]
  }
}}
  }
names(SubNat_Est)[names(SubNat_Est) == "locations" ] <- 'fsc_locations'
vv = SubNat_Est[vc_stages == "farm" &  carbon_1000tonsco2eq ==0,c("geographicaream49", "measureditemcpc"), with=F]
ctry <- unique(vv$geographicaream49)
vv = merge(vv,CountryGroup[,c("m49_code","country"),with=F ], by.x = "geographicaream49", by.y ="m49_code") 
vv = merge(vv,FAOCrops[,c("measureditemcpc","crop"),with=F ], by = "measureditemcpc") 

write.table(vv, "Missing_envVar.csv", sep= ",")
SubNat_Est$source <- 'None'
SubNat_Est[emission_kgco2perkgfood>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[carbon_1000tonsco2eq>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[water_blue_m3_tonfood>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[water_green_m3_tonfood>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[ water_grey_m3_tonfood>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[land_ha_tonfood>0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]
SubNat_Est[ econ_USD_kgfood >0,source :=  "Not for external distribution; Bio Intelligence Services (now Deloitte) was commissioned to develop the FWF studies for the 2011 study"]

names(SubNat_Est) <- tolower(names(SubNat_Est))
SubNat_Est[is.na(SubNat_Est$water_grey_m3_tonfood), "water_grey_m3_tonfood"] <- 0 
SubNat_Est <- SubNat_Est[source != 'None',]

if(savesws){
  # names(FullSet) <- tolower(names(FullSet))
  # ## Delete
  table = "snv_environ_factors"
  changeset <- Changeset(table)
  newdat <- ReadDatatable(table, readOnly = FALSE)
  AddDeletions(changeset, newdat)
  Finalise(changeset)
  ## Add
  AddInsertions(changeset, SubNat_Est)
  Finalise(changeset)
} 
